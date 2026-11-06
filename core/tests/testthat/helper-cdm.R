# Small hand-built CDM: DM, EX (first dose), SV (visits), IGA, PK.
cdm_tbl <- function(form, recid, subjid, ..., availdt = "2026-03-01", visitnum = NA_real_, version = 1) {
  n <- length(recid)
  data.frame(STUDYID = "S1", SITEID = substr(subjid, 1, 3), SUBJID = subjid, FORMOID = form,
             FOLDER = NA_character_, VISITNUM = rep_len(visitnum, n), RECID = recid,
             ENTRYDT = as.Date(availdt), UPDDT = as.Date(NA), AVAILDT = as.Date(availdt),
             VERSION = rep_len(version, n), ..., stringsAsFactors = FALSE)
}
