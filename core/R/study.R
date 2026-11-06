#' Load a study
#'
#' Reads study.yml, compiles the mapping spec, reads one extract and maps it,
#' and builds the configured checks and derivations: everything the review
#' screens need before a review date is chosen.
#'
#' @param dir Study folder (holds study.yml).
#' @param extract Extract folder name under the study's `extracts` folder;
#'   defaults to the latest (folders are named by extract date). With
#'   `study_db` in study.yml, the label of a cut loaded into the study
#'   database ([intake_cut()]) instead; defaults to the latest loaded cut.
#' With `check_spec` in study.yml, the review checks come from that
#' workbook ([read_check_spec()]) and the programmer checks from
#' [library_checks()] always run; `checks` in study.yml can add more. Without
#' it, `checks` in study.yml lists every check.
#'
#' @return A list: `id`, `dir`, `config`, `mapping`, `schedule`,
#'   `extract_date`, `cut_id` (`NA` when read from a folder), `tables`,
#'   `issues`, `checks`, `check_spec` (from
#'   [read_check_spec()], or `NULL`), `derivations`, `thresholds`.
#' @export
load_study <- function(dir, extract = NULL) {
  cfg <- yaml::read_yaml(file.path(dir, "study.yml"))
  path <- function(f) if (grepl("^(/|[A-Za-z]:)", f)) f else file.path(dir, f)
  mapping <- compile_spec(path(cfg$mapping_spec))
  schedule <- utils::read.csv(path(cfg$visit_schedule), stringsAsFactors = FALSE)
  src <- if (is.null(cfg$study_db)) folder_sources(path, cfg, mapping, extract) else
    db_sources(cfg$study_db, dir, cfg$study_id, mapping, extract)
  extract <- src$label
  m <- map_extract(mapping, src$sources, schedule)
  spec <- if (!is.null(cfg$check_spec)) read_check_spec(path(cfg$check_spec))
  checks <- if (is.null(spec)) define_checks(cfg$checks) else define_checks(c(library_checks(), cfg$checks, spec$checks))
  list(
    id = cfg$study_id, dir = dir, config = cfg, mapping = mapping, schedule = schedule,
    extract_date = if (is.na(src$extract_dt)) as.Date(extract, optional = TRUE) else as.Date(src$extract_dt),
    cut_id = src$cut_id, tables = m$tables, issues = m$issues,
    checks = checks, check_spec = spec, derivations = define_derivations(cfg$derivations),
    thresholds = if (is.null(cfg$thresholds)) list() else cfg$thresholds
  )
}

# Source tables from an extract folder: the named one, else the latest.
folder_sources <- function(path, cfg, mapping, extract) {
  ext_root <- path(if (is.null(cfg$extracts)) "extracts" else cfg$extracts)
  if (is.null(extract)) {
    dirs <- sort(list.dirs(ext_root, full.names = FALSE, recursive = FALSE))
    if (!length(dirs)) stop("No extracts in ", ext_root, ".", call. = FALSE)
    extract <- dirs[length(dirs)]
  }
  list(label = extract, extract_dt = NA_character_, cut_id = NA_integer_,
       sources = read_extract(file.path(ext_root, extract), mapping))
}

# Source tables from the study database: the cut with this label, else the
# latest loaded cut.
db_sources <- function(config, dir, study, mapping, extract) {
  con <- open_study_db(config, dir)
  on.exit(DBI::dbDisconnect(con))
  cuts <- list_cuts(con, study)
  if (!nrow(cuts)) stop("No loaded cuts for ", study, " in the study database.", call. = FALSE)
  cut <- if (is.null(extract)) cuts[nrow(cuts), ] else cuts[cuts$LABEL == extract, ]
  if (!nrow(cut)) stop("No loaded cut labelled ", extract, " for ", study, ".", call. = FALSE)
  cut <- cut[nrow(cut), ]
  list(label = cut$LABEL, extract_dt = cut$EXTRACT_DT, cut_id = cut$CUTID,
       sources = read_cut(con, cut$CUTID, mapping))
}

#' Review data for a review date
#'
#' The derived snapshot, ADSL and check results that every screen reads.
#'
#' @param study A study from [load_study()].
#' @param asof Review date.
#' @return A list: `asof`, `tables` (derived snapshot), `adsl`, `findings`,
#'   `summary`.
#' @export
review_data <- function(study, asof) {
  asof <- as.Date(asof)
  dv <- derive_snapshot(study$tables, study$derivations, asof, study$schedule, study$thresholds)
  ck <- run_checks(study$tables, study$checks, asof, study$issues, study$schedule, study$thresholds,
                   derivations = study$derivations, derived = dv)
  list(asof = asof, tables = dv$tables, adsl = dv$adsl, findings = ck$findings, summary = ck$summary)
}
