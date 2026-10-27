#' Load a study
#'
#' Reads study.yml, compiles the mapping spec, reads one extract and maps it,
#' and builds the configured checks and derivations: everything the review
#' screens need before a review date is chosen.
#'
#' @param dir Study folder (holds study.yml).
#' @param extract Extract folder name under the study's `extracts` folder;
#'   defaults to the latest (folders are named by extract date).
#' With `check_spec` in study.yml, the review checks come from that
#' workbook ([read_check_spec()]) and the programmer checks from
#' [library_checks()] always run; `checks` in study.yml can add more. Without
#' it, `checks` in study.yml lists every check.
#'
#' @return A list: `id`, `dir`, `config`, `mapping`, `schedule`,
#'   `extract_date`, `tables`, `issues`, `checks`, `check_spec` (from
#'   [read_check_spec()], or `NULL`), `derivations`, `thresholds`.
#' @export
load_study <- function(dir, extract = NULL) {
  cfg <- yaml::read_yaml(file.path(dir, "study.yml"))
  path <- function(f) if (grepl("^(/|[A-Za-z]:)", f)) f else file.path(dir, f)
  mapping <- compile_spec(path(cfg$mapping_spec))
  schedule <- utils::read.csv(path(cfg$visit_schedule), stringsAsFactors = FALSE)
  ext_root <- path(if (is.null(cfg$extracts)) "extracts" else cfg$extracts)
  if (is.null(extract)) {
    dirs <- sort(list.dirs(ext_root, full.names = FALSE, recursive = FALSE))
    if (!length(dirs)) stop("No extracts in ", ext_root, ".", call. = FALSE)
    extract <- dirs[length(dirs)]
  }
  m <- map_extract(mapping, read_extract(file.path(ext_root, extract), mapping), schedule)
  spec <- if (!is.null(cfg$check_spec)) read_check_spec(path(cfg$check_spec))
  checks <- if (is.null(spec)) define_checks(cfg$checks) else define_checks(c(library_checks(), cfg$checks, spec$checks))
  list(
    id = cfg$study_id, dir = dir, config = cfg, mapping = mapping, schedule = schedule,
    extract_date = as.Date(extract, optional = TRUE), tables = m$tables, issues = m$issues,
    checks = checks, check_spec = spec, derivations = define_derivations(cfg$derivations),
    thresholds = if (is.null(cfg$thresholds)) list() else cfg$thresholds
  )
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
                   derivations = study$derivations)
  list(asof = asof, tables = dv$tables, adsl = dv$adsl, findings = ck$findings, summary = ck$summary)
}
