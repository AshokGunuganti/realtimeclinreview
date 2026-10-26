# =============================================================================
#  Review data layer: build the review datasets (RV*) from typed sources.
#  Datasets hold the full timeline with an availability date (*AVDT/AVAILDT)
#  so any "review as of" date can be reconstructed (see snapshot.R).
#  Spec: sheets "Review Datasets", "Variable Mapping", "Derivation Rules".
# =============================================================================
suppressPackageStartupMessages(library(dplyr))

derive_review <- function(src, sched = load_schedule(), cfg = APP_CFG) {
  fold2num <- stats::setNames(sched$VISITNUM, sched$FOLDER)
  vend2num <- stats::setNames(sched$VISITNUM, sched$VENDORVIS)
  num2vis  <- stats::setNames(sched$VISIT, sched$VISITNUM)

  # ---- RVSUBJ: one row per subject (DR-003, DR-004) --------------------------
  ex1 <- src$ex |> arrange(SUBJID, EXSTDAT) |> distinct(SUBJID, .keep_all = TRUE)
  ds1 <- src$ds |> arrange(SUBJID, DSSTDAT) |> distinct(SUBJID, .keep_all = TRUE)
  subj <- src$dm |>
    transmute(SUBJID, SITEID, AGE, SEX, ENRLDT = ENRLDAT, DMAVDT = CREATED) |>
    left_join(ex1 |> transmute(SUBJID, TRTSDT = EXSTDAT, TRTAVDT = CREATED), by = "SUBJID") |>
    left_join(ds1 |> transmute(SUBJID, WDDT = DSSTDAT, WDREAS = DSDECOD, WDPHASE = DSPHASE, WDAVDT = CREATED), by = "SUBJID") |>
    mutate(PLANDY1 = ENRLDT + cfg$runin_days,                 # DR-003
           ANCHDT  = coalesce(TRTSDT, PLANDY1)) |>
    arrange(SUBJID)

  # ---- RVVISIT: subject x scheduled visit (DR-002) ----------------------------
  sv <- src$sv |>
    mutate(VISITNUM = unname(fold2num[FOLDER])) |>
    filter(!is.na(VISITNUM)) |>
    arrange(SUBJID, VISITNUM, CREATED) |>
    distinct(SUBJID, VISITNUM, .keep_all = TRUE) |>
    transmute(SUBJID, VISITNUM, VISDT = VISDAT, VISNDFL = VISND == "Y", VISNDRS = if ("VISNDRS" %in% names(src$sv)) VISNDRS else "",
              ENTRYDT = CREATED)
  visit <- tidyr::crossing(SUBJID = subj$SUBJID, sched |> select(VISITNUM, FOLDER, VISIT, VISSHORT, NOMWK, WINDOW, ANCHOR, PKFL, DIARYFL, IGAFL)) |>
    left_join(subj |> select(SUBJID, SITEID, ENRLDT, ANCHDT), by = "SUBJID") |>
    mutate(TARGDT = if_else(ANCHOR == "ENRLDT", ENRLDT, ANCHDT + 7L * NOMWK)) |>
    left_join(sv, by = c("SUBJID", "VISITNUM")) |>
    mutate(VISNDFL = coalesce(VISNDFL, FALSE),
           VISOFF  = as.integer(VISDT - TARGDT),
           OOWFL   = !is.na(VISOFF) & WINDOW > 0 & abs(VISOFF) > WINDOW,     # DR-002
           ENTLAG  = if_else(!VISNDFL, as.integer(ENTRYDT - VISDT), NA_integer_)) |>   # DR-006
    select(SUBJID, SITEID, VISITNUM, FOLDER, VISIT, VISSHORT, NOMWK, WINDOW, TARGDT, VISDT, VISOFF, OOWFL,
           VISNDFL, VISNDRS, ENTRYDT, ENTLAG, PKFL, DIARYFL, IGAFL) |>
    arrange(SUBJID, VISITNUM)

  # ---- RVEFF: assessments, long and versioned (DR-005) ------------------------
  iga <- src$iga |>
    transmute(SUBJID, VISITNUM = unname(fold2num[FOLDER]), PARAMCD = "IGA", PARAM = "IGA (investigator)",
              AVAL = IGASCORE, ADT = IGADAT, AVAILDT = CREATED, SRCREC = "EDC")
  mcss <- src$img |>
    transmute(SUBJID, VISITNUM = unname(vend2num[VISIT]), PARAMCD = "MCSS", PARAM = "MCSS total (blinded central)",
              AVAL = MCSS_TOTAL, ADT = PHOTODT, AVAILDT = TRANSDT, SRCREC = RECSTAT)
  eff <- bind_rows(iga, mcss) |>
    filter(!is.na(VISITNUM)) |>
    arrange(SUBJID, VISITNUM, PARAMCD, AVAILDT) |>
    group_by(SUBJID, VISITNUM, PARAMCD) |>
    mutate(VERSION = row_number(), PREVAL = lag(AVAL)) |>
    ungroup() |>
    mutate(VISIT = unname(num2vis[as.character(VISITNUM)]))

  # ---- RVCOMP: dosing diary compliance per interval (DR-008) ------------------
  comp <- src$dry |>
    transmute(SUBJID, VISITNUM = unname(fold2num[FOLDER]), INTSTDT = DRYSTDAT, INTENDT = DRYENDAT,
              DOSPRESC, DOSMISS, DOSAPPL = DOSPRESC - DOSMISS,
              COMPPCT = round(100 * (DOSPRESC - DOSMISS) / DOSPRESC), AVAILDT = CREATED) |>
    filter(!is.na(VISITNUM)) |>
    mutate(VISIT = unname(num2vis[as.character(VISITNUM)])) |>
    arrange(SUBJID, VISITNUM)

  # ---- RVAE: adverse events (DR-010) ------------------------------------------
  ae <- src$ae |>
    left_join(subj |> select(SUBJID, TRTSDT), by = "SUBJID") |>
    transmute(SUBJID, SITEID, AESEQ = AESPID, AETERM, AEDECOD, AESOC, CODEDT = AECODDT,
              AESTDT = AESTDAT,
              ASTDY = as.integer(if_else(AESTDT >= TRTSDT, AESTDT - TRTSDT + 1, AESTDT - TRTSDT)),
              ASEV = tools::toTitleCase(tolower(AESEV)), AEREL,
              ARELFL = AEREL %in% cfg$related_terms, ASERFL = AESER == "Y", AEOUT,
              ENTRYDT = CREATED, UPDDT = if_else(UPDATED > CREATED, UPDATED, as.Date(NA)),
              UPDNOTE = tools::toTitleCase(tolower(AUDITNOTE))) |>
    arrange(SUBJID, AESEQ)

  # ---- RVPK: systemic Study drug (DR-009) --------------------------------------
  pk <- src$pk |>
    transmute(SUBJID, VISITNUM = unname(vend2num[VISIT]), PCTEST, PCDT = PCDTC, PCORRES, PCORRESU,
              LLOQ = PCLLOQ, BLQFL = grepl("^\\s*<", PCORRES),
              AVAL = if_else(grepl("^\\s*<", PCORRES), NA_real_, suppressWarnings(as.numeric(PCORRES))),
              AVAILDT = TRANSDT) |>
    mutate(VISIT = unname(num2vis[as.character(VISITNUM)]),
           PCTLIM = round(100 * AVAL / cfg$pk_limit)) |>
    left_join(subj |> select(SUBJID, SITEID), by = "SUBJID") |>
    arrange(SUBJID, VISITNUM)

  # ---- RVQRY: query log (DR-016) ----------------------------------------------
  qry <- src$qry |>
    mutate(VISITNUM = if_else(FORM == "AE", NA_real_, unname(as.numeric(fold2num[FOLDER])))) |>
    transmute(SUBJID, SITEID = if ("SITEID" %in% names(src$qry)) SITEID else substr(SUBJID, 1, 3),
              QRYID, FORM, VISITNUM, AESEQ = AESPID,
              ITEM = if_else(FORM == "AE", paste0("AE #", AESPID), unname(num2vis[as.character(VISITNUM)])),
              QRYTEXT, OPENDT, ANSWERDT, CLOSEDT) |>
    arrange(QRYID)

  list(subj = subj, visit = visit, eff = eff, comp = comp, ae = ae, pk = pk, qry = qry, sched = sched,
       manifest = src$manifest, parse_issues = src$parse_issues)
}

#' Convenience: read sources and derive in one step
load_review_data <- function(dir = APP_CFG$source_dir) derive_review(read_sources(dir))
