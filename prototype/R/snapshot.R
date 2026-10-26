# =============================================================================
#  As-of snapshot: what the reviewer sees for a given "review as of" date,
#  filters and thresholds. Pure functions (no Shiny) so they are unit-testable.
#  Spec: sheet "Derivation Rules" (DR-001, DR-005, DR-011, DR-016, DR-017).
# =============================================================================
suppressPackageStartupMessages(library(dplyr))

review_snapshot <- function(rv, asof, site = "All sites", pop = "all", cfg = APP_CFG) {
  # DR-017: a record is visible only if its availability date <= asof
  subj <- rv$subj |>
    filter(DMAVDT <= asof) |>
    mutate(DOSEDFL = !is.na(TRTAVDT) & TRTAVDT <= asof,                    # DR-004
           WDFL    = !is.na(WDAVDT) & WDAVDT <= asof,
           ITTFL   = DOSEDFL & AGE >= cfg$itt_min_age)
  if (!identical(site, "All sites")) subj <- subj |> filter(SITEID == site)
  if (identical(pop, "itt")) subj <- subj |> filter(ITTFL)
  ids <- subj$SUBJID

  # DR-005: current value = latest version available by asof
  effv <- rv$eff |> filter(SUBJID %in% ids, AVAILDT <= asof) |>
    group_by(SUBJID, VISITNUM, PARAMCD) |> slice_max(VERSION, n = 1, with_ties = FALSE) |> ungroup()
  effw <- effv |> select(SUBJID, VISITNUM, PARAMCD, AVAL) |>
    tidyr::pivot_wider(names_from = PARAMCD, values_from = AVAL)
  for (pc in c("IGA", "MCSS")) if (!pc %in% names(effw)) effw[[pc]] <- rep(NA_real_, nrow(effw))
  comp <- rv$comp |> filter(SUBJID %in% ids, AVAILDT <= asof)

  visit <- rv$visit |>
    filter(SUBJID %in% ids) |>
    left_join(subj |> select(SUBJID, ITTFL, DOSEDFL, WDFL, WDDT), by = "SUBJID") |>
    mutate(KNOWN   = !is.na(ENTRYDT) & ENTRYDT <= asof,
           ENTERED = KNOWN & !VISNDFL,
           STATUS  = case_when(                                                 # DR-001
             WDFL & TARGDT > WDDT              ~ "Discontinued",
             ENTERED & OOWFL                   ~ "Out of window",
             ENTERED                           ~ "Complete",
             TARGDT + WINDOW >= asof           ~ "Not yet due",
             KNOWN & VISNDFL                   ~ "Missed",
             TRUE                              ~ "Overdue / not entered"),
           DAYSOVER = if_else(STATUS == "Overdue / not entered", as.integer(asof - (TARGDT + WINDOW)), NA_integer_),
           ENTLAG   = if_else(ENTERED, ENTLAG, NA_integer_)) |>
    left_join(effw, by = c("SUBJID", "VISITNUM")) |>
    left_join(comp |> select(SUBJID, VISITNUM, COMPPCT, DOSPRESC, DOSMISS), by = c("SUBJID", "VISITNUM")) |>
    mutate(IGA = if_else(ENTERED, IGA, NA_real_), MCSS = if_else(ENTERED, MCSS, NA_real_),
           COMPPCT = if_else(ENTERED, COMPPCT, NA_real_))

  ae <- rv$ae |> filter(SUBJID %in% ids, ENTRYDT <= asof) |>
    mutate(CODEDFL = !is.na(CODEDT) & CODEDT <= asof,                          # DR-010
           PT  = if_else(CODEDFL, AEDECOD, paste0("Uncoded: ", AETERM)),
           SOC = if_else(CODEDFL, AESOC, "Not yet coded"),
           UPDDT = if_else(!is.na(UPDDT) & UPDDT <= asof, UPDDT, as.Date(NA)))
  pk <- rv$pk |> filter(SUBJID %in% ids, AVAILDT <= asof)
  qry <- rv$qry |> filter(SUBJID %in% ids, OPENDT <= asof) |>
    mutate(STATUS = case_when(!is.na(CLOSEDT) & CLOSEDT <= asof     ~ "Closed",   # DR-016
                              !is.na(ANSWERDT) & ANSWERDT <= asof   ~ "Answered",
                              TRUE                                   ~ "Open"),
           AGEDAYS = if_else(STATUS == "Closed", NA_integer_, as.integer(asof - OPENDT)))
  lastcomp <- visit |> filter(!is.na(COMPPCT)) |> group_by(SUBJID) |>
    slice_max(VISITNUM, n = 1, with_ties = FALSE) |> ungroup()

  list(asof = asof, subj = subj, visit = visit, effv = effv, ae = ae, pk = pk, qry = qry,
       lastcomp = lastcomp, comp = comp)
}

#' DR-011: change log between two refreshes (since, asof]
change_log <- function(s, since, pk_alert = APP_CFG$pk_alert, cthr = APP_CFG$comp_threshold) {
  asof <- s$asof
  inwin <- function(d) !is.na(d) & d > since & d <= asof
  cols <- c("CHANGE", "DOMAIN", "SUBJID", "SITEID", "ITEM", "DETAIL", "CHGDT", "PRIORITY")
  pick <- function(df) if (nrow(df)) df[, cols] else NULL
  sites <- stats::setNames(s$subj$SITEID, s$subj$SUBJID)

  v_new <- s$visit |> filter(ENTERED, inwin(ENTRYDT)) |>
    transmute(CHANGE = "New", DOMAIN = "Visit", SUBJID, SITEID, ITEM = VISIT,
              DETAIL = paste0("Visit entered",
                              if_else(is.na(IGA), "", sprintf(" | IGA %+d", as.integer(IGA))),
                              if_else(is.na(MCSS), "", sprintf(" | MCSS %.1f", MCSS)),
                              if_else(is.na(COMPPCT), "", sprintf(" | Dosing %d%%", as.integer(COMPPCT)))),
              CHGDT = ENTRYDT,
              PRIORITY = case_when(!is.na(IGA) & IGA < 0 ~ "Review",
                                   !is.na(COMPPCT) & COMPPCT < cthr ~ "Low compliance", TRUE ~ ""))
  entered_before <- s$visit |> filter(ENTERED, ENTRYDT <= since) |> select(SUBJID, VISITNUM)
  e_chg <- s$effv |> filter(VERSION > 1, inwin(AVAILDT)) |>
    semi_join(entered_before, by = c("SUBJID", "VISITNUM")) |>
    transmute(CHANGE = "Changed", DOMAIN = "Efficacy", SUBJID, SITEID = unname(sites[SUBJID]), ITEM = VISIT,
              DETAIL = sprintf("%s corrected %.1f -> %.1f (%s)", PARAMCD, PREVAL, AVAL, SRCREC),
              CHGDT = AVAILDT, PRIORITY = "")
  a_new <- s$ae |> filter(inwin(ENTRYDT)) |>
    transmute(CHANGE = "New", DOMAIN = "AE", SUBJID, SITEID, ITEM = paste0("AE #", AESEQ),
              DETAIL = paste0(PT, " | ", ASEV, if_else(ARELFL, " | Related", " | Not related"),
                              if_else(ASERFL, " | SERIOUS", "")),
              CHGDT = ENTRYDT, PRIORITY = case_when(ASERFL ~ "SAE", ASEV == "Severe" ~ "Review", TRUE ~ ""))
  a_chg <- s$ae |> filter(inwin(UPDDT), ENTRYDT <= since) |>
    transmute(CHANGE = "Changed", DOMAIN = "AE", SUBJID, SITEID, ITEM = paste0("AE #", AESEQ),
              DETAIL = paste0(PT, " | ", UPDNOTE), CHGDT = UPDDT, PRIORITY = "")
  p_new <- s$pk |> filter(inwin(AVAILDT)) |>
    transmute(CHANGE = "New", DOMAIN = "PK", SUBJID, SITEID, ITEM = VISIT,
              DETAIL = paste("Study drug", PCORRES, PCORRESU), CHGDT = AVAILDT,
              PRIORITY = if_else(!BLQFL & AVAL >= pk_alert, "PK alert", ""))
  q_new <- s$qry |> filter(inwin(OPENDT)) |>
    transmute(CHANGE = "New", DOMAIN = "Query", SUBJID, SITEID, ITEM, DETAIL = paste0(QRYID, ": ", QRYTEXT),
              CHGDT = OPENDT, PRIORITY = "")

  out <- bind_rows(pick(v_new), pick(e_chg), pick(a_new), pick(a_chg), pick(p_new), pick(q_new))
  if (is.null(out) || !nrow(out)) return(as.data.frame(stats::setNames(replicate(8, character(0), simplify = FALSE), cols)))
  out |>
    mutate(.rk = match(PRIORITY, c("SAE", "PK alert", "Review", "Low compliance", ""))) |>
    arrange(.rk, desc(CHGDT)) |> select(-.rk)
}
