# =============================================================================
#  Screen-level summaries computed from a snapshot. Pure functions, unit-tested.
#  Spec: "Derivation Rules" DR-007, DR-012 .. DR-015; "Screen Traceability".
# =============================================================================
suppressPackageStartupMessages(library(dplyr))

#' DR-012: mean and 95% CI of a parameter by visit, visits with n < min_n dropped
visit_mean_ci <- function(s, param = "IGA", post_only = TRUE, cfg = APP_CFG) {
  d <- s$visit |> filter(ENTERED, ITTFL, !is.na(.data[[param]]))
  if (post_only) d <- d |> filter(NOMWK > 0)
  d |> group_by(VISITNUM, VISIT, NOMWK) |>
    summarise(N = n(), MEAN = mean(.data[[param]]), SD = stats::sd(.data[[param]]), .groups = "drop") |>
    filter(N >= cfg$min_n) |>
    mutate(HALF = stats::qt(0.975, N - 1) * SD / sqrt(N), LCL = MEAN - HALF, UCL = MEAN + HALF) |>
    arrange(NOMWK)
}

#' DR-013: subject-level efficacy review table with flags
efficacy_subjects <- function(s) {
  e <- s$visit |> filter(ENTERED, ITTFL)
  if (!nrow(e)) return(data.frame())
  base <- e |> filter(NOMWK == 0)  |> select(SUBJID, MCSS_BL = MCSS)
  runi <- e |> filter(NOMWK == -8) |> select(SUBJID, MCSS_RI = MCSS)
  e |> filter(NOMWK > 0, !is.na(IGA)) |>
    group_by(SUBJID) |> slice_max(VISITNUM, n = 1, with_ties = FALSE) |> ungroup() |>
    select(SUBJID, SITEID, VISIT, IGA, MCSS) |>
    left_join(base, by = "SUBJID") |> left_join(runi, by = "SUBJID") |>
    left_join(s$lastcomp |> select(SUBJID, COMPPCT), by = "SUBJID") |>
    mutate(PCHG = round(100 * (MCSS - MCSS_BL) / MCSS_BL),
           FLAG = case_when(IGA < 0 ~ "Worsening",
                            coalesce(abs(MCSS_BL - MCSS_RI) > 3, FALSE) ~ "Run-in instability",
                            IGA <= 0 ~ "No response",
                            TRUE ~ "")) |>
    arrange(desc(FLAG != ""), PCHG)
}

#' DR-014: site performance with status score
site_summary <- function(s, cthr = APP_CFG$comp_threshold, cfg = APP_CFG) {
  if (!nrow(s$subj)) return(data.frame())
  due  <- s$visit |> filter(!STATUS %in% c("Not yet due", "Discontinued"))
  vsum <- due |> group_by(SITEID) |>
    summarise(VISDUE = n(), PCTWIN = round(100 * mean(STATUS == "Complete")),
              OVERDUE = sum(STATUS == "Overdue / not entered"), MISSED = sum(STATUS == "Missed"),
              LAGMED = if (any(!is.na(ENTLAG))) stats::median(ENTLAG, na.rm = TRUE) else NA_real_,
              LAGP90 = if (any(!is.na(ENTLAG))) unname(stats::quantile(ENTLAG, .9, na.rm = TRUE, type = 1)) else NA_real_,
              PCTLAG = if (any(!is.na(ENTLAG))) round(100 * mean(ENTLAG[!is.na(ENTLAG)] <= cfg$lag_target)) else NA_real_,
              .groups = "drop")
  qsum <- s$qry |> group_by(SITEID) |>
    summarise(NQRY = n(), OPENQ = sum(STATUS != "Closed"), .groups = "drop")
  csum <- s$lastcomp |> group_by(SITEID) |>
    summarise(COMPMEAN = round(mean(COMPPCT)), LOWCOMP = sum(COMPPCT < cthr), .groups = "drop")
  s$subj |> group_by(SITEID) |>
    summarise(ENROLLED = n(), DOSED = sum(DOSEDFL), .groups = "drop") |>
    left_join(vsum, by = "SITEID") |> left_join(qsum, by = "SITEID") |> left_join(csum, by = "SITEID") |>
    mutate(across(c(VISDUE, OVERDUE, MISSED, NQRY, OPENQ, LOWCOMP), ~ coalesce(as.numeric(.x), 0)),
           QRATE = if_else(VISDUE > 0, round(10 * NQRY / VISDUE, 1), NA_real_),
           SCORE = 2 * coalesce(LAGMED > cfg$lag_target, FALSE) + 2 * (OVERDUE > 0) + (MISSED > 0) +
                   (OPENQ >= 3) + (LOWCOMP > 0) + coalesce(PCTWIN < 85, FALSE),
           STATUS = case_when(SCORE >= 3 ~ "Needs attention", SCORE >= 1 ~ "Watch", TRUE ~ "On track")) |>
    arrange(desc(SCORE), SITEID)
}

#' Subjects whose latest diary interval is below threshold (DR-008)
low_compliance <- function(s, cthr = APP_CFG$comp_threshold) {
  iv <- s$visit |> filter(!is.na(COMPPCT)) |> arrange(SUBJID, VISITNUM)
  if (!nrow(iv)) return(data.frame())
  iv |> group_by(SUBJID, SITEID) |>
    summarise(VISIT = last(VISIT), COMPPCT = last(COMPPCT), DOSMISS = last(DOSMISS), DOSPRESC = last(DOSPRESC),
              MEANPCT = round(mean(COMPPCT)),
              DELTA = if (n() > 1) last(COMPPCT) - nth(COMPPCT, n() - 1) else NA_real_, .groups = "drop") |>
    filter(COMPPCT < cthr) |> arrange(COMPPCT)
}

#' DR-015: attention panel counts
attention_items <- function(s, chg, pk_alert, cthr, cfg = APP_CFG) {
  latest_iga <- s$visit |> filter(ENTERED, !is.na(IGA)) |> group_by(SUBJID) |>
    slice_max(VISITNUM, n = 1, with_ties = FALSE) |> ungroup()
  ss <- site_summary(s, cthr)
  data.frame(
    ITEM = c("New serious AEs this refresh", sprintf("PK samples >= %.1f ng/mL (all-time)", pk_alert),
             "Subjects with IGA below 0 (latest visit)", sprintf("Subjects with dosing compliance < %d%% (latest)", cthr),
             sprintf("Visits overdue > %d days", cfg$overdue_flag_days), sprintf("Queries open > %d days", cfg$query_old_days),
             sprintf("Sites with median entry lag > %d days", cfg$lag_target), "AEs awaiting MedDRA coding"),
    N = c(sum(chg$PRIORITY == "SAE"),
          sum(!s$pk$BLQFL & s$pk$AVAL >= pk_alert, na.rm = TRUE),
          sum(latest_iga$IGA < 0),
          sum(s$lastcomp$COMPPCT < cthr),
          sum(s$visit$STATUS == "Overdue / not entered" & s$visit$DAYSOVER > cfg$overdue_flag_days, na.rm = TRUE),
          sum(s$qry$STATUS == "Open" & s$qry$AGEDAYS > cfg$query_old_days, na.rm = TRUE),
          if (nrow(ss)) sum(ss$LAGMED > cfg$lag_target, na.rm = TRUE) else 0,
          sum(!s$ae$CODEDFL)),
    TAB = c("safety", "safety", "efficacy", "sites", "dq", "dq", "sites", "checks"),
    LEVEL = c("crit", "warn", "ser", "warn", "ser", "ser", "ser", "warn"),
    stringsAsFactors = FALSE)
}

#' Issue listing: visit problems + unresolved queries
issue_listing <- function(s) {
  v <- s$visit |> filter(STATUS %in% c("Overdue / not entered", "Missed", "Out of window")) |>
    transmute(TYPE = STATUS, SUBJID, SITEID, ITEM = VISIT,
              DETAIL = case_when(STATUS == "Out of window" ~ sprintf("Visit %+d days from target (window +/-%d)", VISOFF, WINDOW),
                                 STATUS == "Missed" ~ paste("Visit not done:", tolower(VISNDRS)),
                                 TRUE ~ paste("Due", format(TARGDT, "%d %b"), "- nothing entered")),
              DAYS = coalesce(DAYSOVER, 0L))
  q <- s$qry |> filter(STATUS != "Closed") |>
    transmute(TYPE = STATUS, SUBJID, SITEID, ITEM = paste(QRYID, ITEM, sep = " | "), DETAIL = QRYTEXT, DAYS = AGEDAYS)
  bind_rows(v, q) |> arrange(desc(DAYS))
}
