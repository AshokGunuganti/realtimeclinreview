# =============================================================================
#  Source data checks (SC-xxx). Run on every load; the as-of checks rerun when
#  the review date changes. Spec: sheet "Data Checks".
# =============================================================================
suppressPackageStartupMessages(library(dplyr))

CHECK_DEFS <- data.frame(
  CHECK = sprintf("SC-%03d", 1:11),
  SEVERITY = c("Error", "Error", "Error", "Warning", "Warning", "Warning", "Warning", "Info", "Error", "Error", "Warning"),
  DESCRIPTION = c(
    "Source file present with all required columns",
    "Date/time values parse in the expected format",
    "Every subject in a source file exists in Demographics",
    "Visit folder / vendor visit label matches the visit schedule",
    "No duplicate records on the source key",
    "Post-baseline visit entered without an IGA score",
    "Visit entered without a blinded MCSS from the imaging vendor",
    "Adverse event entered but not yet MedDRA coded",
    "PK result is numeric or a '<LLOQ' text result",
    "Diary missed doses between 0 and doses prescribed",
    "Post-baseline visit date on or before first dose date"),
  stringsAsFactors = FALSE)

run_checks <- function(src, rv, s) {
  out <- list()
  add <- function(id, df) if (nrow(df)) out[[length(out) + 1]] <<- df |> mutate(CHECK = id, .before = 1) |>
    mutate(across(everything(), as.character))
  dm_ids <- src$dm$SUBJID
  sched  <- rv$sched

  # SC-002 parse issues from the adapter
  if (nrow(src$parse_issues)) add("SC-002", src$parse_issues |> select(SOURCE, SUBJID, FIELD, VALUE, DETAIL))

  # SC-003 orphan subjects
  for (k in c("ex", "ds", "sv", "iga", "img", "ae", "dry", "pk", "qry")) {
    bad <- setdiff(unique(src[[k]]$SUBJID), dm_ids)
    if (length(bad)) add("SC-003", data.frame(SOURCE = SOURCE_SPECS[[k]]$file, SUBJID = bad, FIELD = "SUBJID",
                                               VALUE = bad, DETAIL = "Subject not in edc_dm.csv"))
  }
  # SC-004 unknown visits
  for (k in c("sv", "iga", "dry")) {
    bad <- src[[k]] |> filter(!FOLDER %in% sched$FOLDER)
    if (nrow(bad)) add("SC-004", bad |> transmute(SOURCE = SOURCE_SPECS[[k]]$file, SUBJID, FIELD = "FOLDER",
                                                   VALUE = FOLDER, DETAIL = "Folder not in visit schedule"))
  }
  for (k in c("img", "pk")) {
    bad <- src[[k]] |> filter(!VISIT %in% sched$VENDORVIS)
    if (nrow(bad)) add("SC-004", bad |> transmute(SOURCE = SOURCE_SPECS[[k]]$file, SUBJID, FIELD = "VISIT",
                                                   VALUE = VISIT, DETAIL = "Vendor visit not in visit schedule"))
  }
  # SC-005 duplicates on source keys
  dupkey <- function(k, keys, label) {
    d <- src[[k]] |> count(across(all_of(keys))) |> filter(n > 1)
    if (nrow(d)) add("SC-005", d |> transmute(SOURCE = SOURCE_SPECS[[k]]$file, SUBJID,
                                               FIELD = paste(keys, collapse = "+"),
                                               VALUE = do.call(paste, c(across(all_of(keys[-1])), sep = "/")),
                                               DETAIL = sprintf("%d records on %s key", n, label)))
  }
  dupkey("sv", c("SUBJID", "FOLDER"), "visit"); dupkey("iga", c("SUBJID", "FOLDER"), "IGA")
  dupkey("ae", c("SUBJID", "AESPID"), "AE"); dupkey("dry", c("SUBJID", "FOLDER"), "diary")
  dupkey("pk", c("SUBJID", "VISIT"), "PK")

  # As-of checks (depend on the review date)
  ent <- s$visit |> filter(ENTERED)
  add("SC-006", ent |> filter(NOMWK > 0, is.na(IGA)) |>
        transmute(SOURCE = "edc_iga.csv", SUBJID, FIELD = "IGASCORE", VALUE = VISIT, DETAIL = "Visit entered, IGA not entered"))
  add("SC-007", ent |> filter(is.na(MCSS)) |>
        transmute(SOURCE = "vendor_imaging_mcss.csv", SUBJID, FIELD = "MCSS_TOTAL", VALUE = VISIT,
                  DETAIL = "Visit entered, central read not yet received"))
  add("SC-008", s$ae |> filter(!CODEDFL) |>
        transmute(SOURCE = "edc_ae.csv", SUBJID, FIELD = "AEDECOD", VALUE = AETERM,
                  DETAIL = sprintf("Entered %s, awaiting coding", format(ENTRYDT, "%d %b"))))
  # SC-009 PK result format
  badpk <- src$pk |> filter(!grepl("^\\s*<\\s*[0-9.]+\\s*$", PCORRES), is.na(suppressWarnings(as.numeric(PCORRES))))
  add("SC-009", badpk |> transmute(SOURCE = "vendor_pk.csv", SUBJID, FIELD = "PCORRES", VALUE = PCORRES,
                                   DETAIL = "Result not numeric and not '<LLOQ'"))
  # SC-010 diary range
  add("SC-010", src$dry |> filter(is.na(DOSMISS) | DOSMISS < 0 | DOSMISS > DOSPRESC) |>
        transmute(SOURCE = "edc_diary.csv", SUBJID, FIELD = "DOSMISS", VALUE = paste(DOSMISS, "of", DOSPRESC),
                  DETAIL = "Missed doses out of range"))
  # SC-011 visit dates vs first dose
  add("SC-011", rv$visit |> left_join(rv$subj |> select(SUBJID, TRTSDT), by = "SUBJID") |>
        filter(NOMWK > 0, !is.na(VISDT), !is.na(TRTSDT), VISDT <= TRTSDT) |>
        transmute(SOURCE = "edc_sv.csv", SUBJID, FIELD = "VISDAT", VALUE = format(VISDT), DETAIL = paste(VISIT, "on/before first dose")))

  findings <- if (length(out)) bind_rows(out) else
    data.frame(CHECK = character(), SOURCE = character(), SUBJID = character(), FIELD = character(),
               VALUE = character(), DETAIL = character())
  summary <- CHECK_DEFS |>
    left_join(findings |> count(CHECK, name = "N"), by = "CHECK") |>
    mutate(N = coalesce(N, 0L), RESULT = if_else(N == 0, "Pass", SEVERITY))
  list(summary = summary, findings = findings)
}
