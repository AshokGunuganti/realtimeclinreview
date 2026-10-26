# =============================================================================
#  Source adapters: read raw source files, enforce required columns, and type
#  every field. This is the ONLY layer that knows source file formats.
#  To onboard a real EDC export, change SOURCE_SPECS and the parsers here.
#  Spec: sheets "Sources" and "Source Variables".
# =============================================================================

# Required columns per source (SC-001)
SOURCE_SPECS <- list(
  dm  = list(file = "edc_dm.csv",              cols = c("SITEID", "SUBJID", "AGE", "SEX", "ENRLDAT", "CREATED")),
  ex  = list(file = "edc_ex1.csv",             cols = c("SUBJID", "EXSTDAT", "CREATED")),
  ds  = list(file = "edc_ds.csv",              cols = c("SUBJID", "DSPHASE", "DSDECOD", "DSSTDAT", "CREATED")),
  sv  = list(file = "edc_sv.csv",              cols = c("SUBJID", "FOLDER", "VISDAT", "VISND", "CREATED")),
  iga = list(file = "edc_iga.csv",             cols = c("SUBJID", "FOLDER", "IGADAT", "IGASCORE", "CREATED")),
  img = list(file = "vendor_imaging_mcss.csv", cols = c("SUBJID", "VISIT", "PHOTODT", "MCSS_TOTAL", "RECSTAT", "TRANSDT")),
  ae  = list(file = "edc_ae.csv",              cols = c("SUBJID", "AESPID", "AETERM", "AEDECOD", "AESOC", "AECODDT",
                                                        "AESTDAT", "AESEV", "AEREL", "AESER", "AEOUT", "CREATED", "UPDATED", "AUDITNOTE")),
  dry = list(file = "edc_diary.csv",           cols = c("SUBJID", "FOLDER", "DRYSTDAT", "DRYENDAT", "DOSPRESC", "DOSMISS", "CREATED")),
  pk  = list(file = "vendor_pk.csv",           cols = c("SUBJID", "VISIT", "PCDTC", "PCTEST", "PCORRES", "PCORRESU", "PCLLOQ", "TRANSDT")),
  qry = list(file = "edc_queries.csv",         cols = c("SUBJID", "QRYID", "FORM", "FOLDER", "AESPID", "QRYTEXT",
                                                        "OPENDT", "ANSWERDT", "CLOSEDT"))
)

# ---- parsers (Rave: "DD MON YYYY" and "DD MON YYYY HH:MM:SS"; vendors: ISO 8601) ----
parse_rave_date <- function(x) {
  x <- trimws(as.character(x)); x[x == ""] <- NA
  as.Date(x, format = "%d %b %Y")
}
parse_rave_dtm <- function(x) {             # audit timestamp -> calendar date of the event
  x <- trimws(as.character(x)); x[x == ""] <- NA
  as.Date(as.POSIXct(x, format = "%d %b %Y %H:%M:%S", tz = "UTC"))
}
parse_iso_date <- function(x) {
  x <- trimws(as.character(x)); x[x == ""] <- NA
  as.Date(substr(x, 1, 10), format = "%Y-%m-%d")
}

read_source_file <- function(dir, key) {
  sp <- SOURCE_SPECS[[key]]
  path <- file.path(dir, sp$file)
  if (!file.exists(path)) stop(sprintf("SC-001: source file missing: %s", sp$file))
  df <- utils::read.csv(path, stringsAsFactors = FALSE, colClasses = "character", na.strings = character(0))
  missing <- setdiff(sp$cols, names(df))
  if (length(missing)) stop(sprintf("SC-001: %s missing required column(s): %s", sp$file, paste(missing, collapse = ", ")))
  df
}

# Parse a set of date columns, recording any non-blank value that fails to parse (SC-002)
type_dates <- function(df, cols, parser, src, issues) {
  for (cc in cols) {
    raw <- df[[cc]]
    val <- parser(raw)
    bad <- which(!is.na(raw) & trimws(raw) != "" & is.na(val))
    if (length(bad)) issues[[length(issues) + 1]] <- data.frame(
      CHECK = "SC-002", SOURCE = src, SUBJID = df$SUBJID[bad], FIELD = cc, VALUE = raw[bad],
      DETAIL = "Date/time value could not be parsed", stringsAsFactors = FALSE)
    df[[cc]] <- val
  }
  list(df = df, issues = issues)
}

#' Read and type all sources. Returns list of typed tables plus parse issues.
read_sources <- function(dir = APP_CFG$source_dir) {
  raw <- lapply(names(SOURCE_SPECS), function(k) read_source_file(dir, k))
  names(raw) <- names(SOURCE_SPECS)
  issues <- list()
  td <- function(key, rave_d = NULL, rave_t = NULL, iso = NULL) {
    df <- raw[[key]]
    if (length(rave_d)) { r <- type_dates(df, rave_d, parse_rave_date, SOURCE_SPECS[[key]]$file, issues); df <- r$df; issues <<- r$issues }
    if (length(rave_t)) { r <- type_dates(df, rave_t, parse_rave_dtm,  SOURCE_SPECS[[key]]$file, issues); df <- r$df; issues <<- r$issues }
    if (length(iso))    { r <- type_dates(df, iso,    parse_iso_date,  SOURCE_SPECS[[key]]$file, issues); df <- r$df; issues <<- r$issues }
    df
  }
  out <- list(
    dm  = td("dm",  "ENRLDAT", c("CREATED", "UPDATED")),
    ex  = td("ex",  "EXSTDAT", c("CREATED", "UPDATED")),
    ds  = td("ds",  "DSSTDAT", c("CREATED", "UPDATED")),
    sv  = td("sv",  "VISDAT",  c("CREATED", "UPDATED")),
    iga = td("iga", "IGADAT",  c("CREATED", "UPDATED")),
    img = td("img", iso = c("PHOTODT", "READDT", "TRANSDT")),
    ae  = td("ae",  c("AECODDT", "AESTDAT"), c("CREATED", "UPDATED")),
    dry = td("dry", c("DRYSTDAT", "DRYENDAT"), c("CREATED", "UPDATED")),
    pk  = td("pk",  iso = c("PCDTC", "TRANSDT")),
    qry = td("qry", rave_t = c("OPENDT", "ANSWERDT", "CLOSEDT"))
  )
  num <- function(x) suppressWarnings(as.numeric(x))
  out$dm$AGE        <- num(out$dm$AGE)
  out$iga$IGASCORE  <- num(out$iga$IGASCORE)
  out$img$MCSS_TOTAL <- num(out$img$MCSS_TOTAL)
  out$ae$AESPID     <- as.integer(num(out$ae$AESPID))
  out$dry$DOSPRESC  <- num(out$dry$DOSPRESC)
  out$dry$DOSMISS   <- num(out$dry$DOSMISS)
  out$pk$PCLLOQ     <- num(out$pk$PCLLOQ)
  out$qry$AESPID    <- as.integer(num(out$qry$AESPID))
  mf <- file.path(dir, "manifest.csv")
  out$manifest <- if (file.exists(mf)) utils::read.csv(mf, stringsAsFactors = FALSE) else NULL
  out$parse_issues <- if (length(issues)) do.call(rbind, issues) else
    data.frame(CHECK = character(), SOURCE = character(), SUBJID = character(), FIELD = character(),
               VALUE = character(), DETAIL = character())
  out
}
