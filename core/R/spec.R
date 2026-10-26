# Mapping spec: authored in Excel by data managers and programmers, compiled
# to YAML by the pipeline, and validated before any mapping runs. The YAML is
# generated, never edited by hand.

mapping_schema_version <- 1L

# "report" is an operational EDC report (queries, form status): keyed to
# subjects, kept out of clinical displays.
form_types <- c("subject", "visit", "log", "vendor", "report")

# Header variables filled by the mapper itself, never by the spec.
auto_header_vars <- c("FORMOID", "RECID", "VERSION")

required_header_vars <- function(type) {
  base <- c("STUDYID", "SITEID", "SUBJID", "ENTRYDT", "AVAILDT")
  if (type %in% c("visit", "vendor")) c(base, "FOLDER", "VISITNUM") else base
}

spec_sheets <- list(
  Forms     = c("FORMOID", "FORMTYPE", "SOURCE", "KEY", "VERSIONING", "DESCRIPTION"),
  Mapping   = c("FORMOID", "VARIABLE", "SOURCE_FIELDS", "TRANSFORM", "ARGS", "REQUIRED", "NOTES"),
  Codelists = c("CODELIST", "CODE", "DECODE")
)

#' Compile an Excel mapping spec to YAML
#'
#' Reads the `Forms`, `Mapping` and `Codelists` sheets, validates the result
#' with [validate_mapping()], and optionally writes it as YAML.
#'
#' Spec layout:
#' * **Forms**: `FORMOID`, `FORMTYPE` (subject, visit, log, vendor, or
#'   report for operational EDC reports such as queries and form status),
#'   `SOURCE` (file name in the extract), `KEY` (comma-separated source fields
#'   that identify a record), `VERSIONING` (`single`: one row per key;
#'   `rows`: several rows per key are successive versions, such as vendor
#'   corrections, ordered by `AVAILDT`), `DESCRIPTION`.
#' * **Mapping**: one row per target variable, in output order. `SOURCE_FIELDS`
#'   is comma-separated; `ARGS` is `name=value; name=value`; `REQUIRED` is
#'   `Y` when the source fields must exist in the extract. Rows with
#'   transform `filter` or `pivot` reshape the source before any field is
#'   mapped and leave `VARIABLE` blank.
#' * **Codelists**: `CODELIST`, `CODE`, `DECODE`.
#'
#' @param xlsx Path to the spec workbook.
#' @param yml Optional output path for the compiled YAML.
#' @param ... Passed to [validate_mapping()].
#' @return The compiled mapping (a list), invisibly.
#' @export
compile_spec <- function(xlsx, yml = NULL, ...) {
  sheets <- openxlsx::getSheetNames(xlsx)
  miss <- setdiff(names(spec_sheets), sheets)
  if (length(miss)) stop("Spec is missing sheet(s): ", paste(miss, collapse = ", "), ".", call. = FALSE)
  rd <- function(sheet) {
    df <- openxlsx::read.xlsx(xlsx, sheet = sheet, colNames = TRUE, skipEmptyRows = TRUE)
    if (is.null(df)) df <- as.data.frame(stats::setNames(rep(list(character()), length(spec_sheets[[sheet]])),
                                                         spec_sheets[[sheet]]))
    cols <- setdiff(spec_sheets[[sheet]], names(df))
    if (length(cols)) stop("Sheet ", sheet, " is missing column(s): ", paste(cols, collapse = ", "), ".", call. = FALSE)
    for (col in names(df)) {
      v <- as.character(df[[col]])
      if (col != "ARGS") v <- trimws(v)       # ARGS keeps spaces, e.g. sep=" "
      v[!is.na(v) & trimws(v) == ""] <- NA
      df[[col]] <- v
    }
    df
  }
  fm <- rd("Forms")
  mp <- rd("Mapping")
  cl <- rd("Codelists")

  forms <- list()
  for (i in seq_len(nrow(fm))) {
    id <- fm$FORMOID[i]
    rows <- mp[!is.na(mp$FORMOID) & mp$FORMOID == id, , drop = FALSE]
    entries <- lapply(seq_len(nrow(rows)), function(j) {
      list(variable = rows$VARIABLE[j],
           source = if (is.na(rows$SOURCE_FIELDS[j])) character() else split_list(rows$SOURCE_FIELDS[j]),
           transform = rows$TRANSFORM[j],
           args = parse_args(rows$ARGS[j]),
           required = identical(toupper(rows$REQUIRED[j]), "Y"),
           notes = rows$NOTES[j])
    })
    step <- vapply(entries, function(e) !is.na(e$transform) && e$transform %in% c("filter", "pivot"), logical(1))
    forms[[id]] <- list(
      type = fm$FORMTYPE[i],
      source = fm$SOURCE[i],
      key = if (is.na(fm$KEY[i])) character() else split_list(fm$KEY[i]),
      versioning = if (is.na(fm$VERSIONING[i])) "single" else fm$VERSIONING[i],
      description = fm$DESCRIPTION[i],
      steps = lapply(entries[step], function(e) e[c("transform", "args")]),
      fields = entries[!step]
    )
  }
  orphan <- setdiff(stats::na.omit(mp$FORMOID), fm$FORMOID)

  codelists <- list()
  for (nm in unique(stats::na.omit(cl$CODELIST))) {
    r <- cl[!is.na(cl$CODELIST) & cl$CODELIST == nm, ]
    codelists[[nm]] <- list(code = r$CODE, decode = r$DECODE)
  }

  m <- list(schema_version = mapping_schema_version, forms = forms, codelists = codelists)
  if (length(orphan)) {
    stop("Mapping rows refer to form(s) not on the Forms sheet: ", paste(orphan, collapse = ", "), ".", call. = FALSE)
  }
  validate_mapping(m, ...)
  if (!is.null(yml)) yaml::write_yaml(m, yml)
  invisible(m)
}

#' Read a compiled mapping
#'
#' @param yml Path to a mapping YAML written by [compile_spec()].
#' @param ... Passed to [validate_mapping()].
#' @return The mapping (a list), validated.
#' @export
read_mapping <- function(yml, ...) {
  m <- yaml::read_yaml(yml)
  norm <- function(e) {
    e$source <- as.character(unlist(e$source))
    e$args <- lapply(e$args, as.character)
    if (is.null(e$args)) e$args <- list()
    e
  }
  for (id in names(m$forms)) {
    f <- m$forms[[id]]
    f$key <- as.character(unlist(f$key))
    f$steps <- lapply(f$steps, norm)
    f$fields <- lapply(f$fields, norm)
    m$forms[[id]] <- f
  }
  m$codelists <- lapply(m$codelists, function(cl) lapply(cl, as.character))
  validate_mapping(m, ...)
  m
}

#' Validate a mapping
#'
#' Checks every form and field against the schema: known form types and
#' transforms, allowed and required arguments, number of source fields,
#' codelists that exist, the standard header mapped, and no variable mapped
#' twice. All problems are reported together.
#'
#' @param m A mapping, as returned by [compile_spec()] or [read_mapping()].
#' @param derive_share Warn when more than this share of a study's fields use
#'   `derive`, a sign the transform library is missing something.
#' @return `m`, invisibly.
#' @export
validate_mapping <- function(m, derive_share = 0.25) {
  p <- character()
  add <- function(...) p <<- c(p, paste0(...))
  if (!identical(as.integer(m$schema_version), mapping_schema_version)) {
    add("schema_version must be ", mapping_schema_version, ".")
  }
  if (!length(m$forms)) add("No forms defined.")

  n_fields <- 0
  n_derive <- 0
  for (id in names(m$forms)) {
    f <- m$forms[[id]]
    at <- paste0("Form ", id, ": ")
    if (!isTRUE(f$type %in% form_types)) add(at, "FORMTYPE must be one of ", paste(form_types, collapse = ", "), ".")
    if (!length(f$source) || is.na(f$source)) add(at, "SOURCE is missing.")
    if (!length(f$key)) add(at, "KEY is missing.")
    if (!isTRUE(f$versioning %in% c("single", "rows"))) add(at, "VERSIONING must be single or rows.")

    for (s in f$steps) p <- c(p, check_entry(s, at, m$codelists, step = TRUE))
    vars <- vapply(f$fields, function(e) if (is.null(e$variable) || is.na(e$variable)) NA_character_ else e$variable,
                   character(1))
    for (k in seq_along(f$fields)) {
      e <- f$fields[[k]]
      if (is.na(vars[k])) {
        add(at, "a mapping row has no VARIABLE.")
        next
      }
      p <- c(p, check_entry(e, paste0(at, vars[k], ": "), m$codelists, step = FALSE))
      n_fields <- n_fields + 1
      if (identical(e$transform, "derive")) n_derive <- n_derive + 1
    }
    flags <- unlist(lapply(f$fields, function(e) if (identical(e$transform, "number")) e$args$flag))
    all_vars <- c(stats::na.omit(vars), flags)
    dup <- unique(all_vars[duplicated(all_vars)])
    if (length(dup)) add(at, "variable(s) mapped more than once: ", paste(dup, collapse = ", "), ".")
    auto <- intersect(all_vars, auto_header_vars)
    if (length(auto)) add(at, paste(auto, collapse = ", "), " are filled automatically and must not be mapped.")
    if (isTRUE(f$type %in% form_types)) {
      need <- setdiff(required_header_vars(f$type), all_vars)
      if (length(need)) add(at, "header variable(s) not mapped: ", paste(need, collapse = ", "), ".")
    }
  }
  for (nm in names(m$codelists)) {
    cl <- m$codelists[[nm]]
    if (anyDuplicated(cl$code)) add("Codelist ", nm, ": duplicate codes.")
  }
  if (length(p)) stop("Mapping spec is not valid:\n", paste0("- ", p, collapse = "\n"), call. = FALSE)
  if (n_fields > 0 && n_derive / n_fields > derive_share) {
    warning(sprintf("%d of %d fields (%.0f%%) use derive; consider adding a transform to the library.",
                    n_derive, n_fields, 100 * n_derive / n_fields), call. = FALSE)
  }
  invisible(m)
}

# Problems with one mapping row or table step.
check_entry <- function(e, at, codelists, step) {
  p <- character()
  add <- function(...) p <<- c(p, paste0(at, ...))
  tr <- e$transform
  if (is.null(tr) || is.na(tr) || !tr %in% names(transform_defs)) {
    add("unknown transform '", if (is.null(tr)) "" else tr, "'.")
    return(p)
  }
  def <- transform_defs[[tr]]
  if (!step && is_table_step(tr)) add(tr, " is a table step and must leave VARIABLE blank.")
  args <- e$args
  if (!is.null(def$args)) {
    extra <- setdiff(names(args), def$args)
    if (length(extra)) add("argument(s) not used by ", tr, ": ", paste(extra, collapse = ", "), ".")
  }
  need <- setdiff(def$required, names(args))
  if (length(need)) add(tr, " needs argument(s): ", paste(need, collapse = ", "), ".")
  if (!is.null(def$n_src)) {
    n <- length(e$source)
    if (n < def$n_src[1] || n > def$n_src[2]) {
      add(tr, " takes ", if (def$n_src[1] == def$n_src[2]) def$n_src[1] else
        paste0(def$n_src[1], " or more"), " source field(s), got ", n, ".")
    }
  }
  if (tr == "date") {
    if (!is.null(args$type) && !args$type %in% c("date", "datetime", "text")) add("date type must be date, datetime or text.")
    if (!is.null(args$format) && !valid_date_format(args$format)) add("date format '", args$format, "' not recognised.")
  }
  if (tr == "decode" && !is.null(args$codelist) && !args$codelist %in% names(codelists)) {
    add("codelist ", args$codelist, " is not on the Codelists sheet.")
  }
  if (tr == "recode") {
    if (!is.null(args$map) && inherits(try(parse_value_map(args$map), silent = TRUE), "try-error")) {
      add("recode map must look like 'Y:Yes,N:No'.")
    }
    if (!is.null(args$default) && !args$default %in% c("keep", "missing")) add("recode default must be keep or missing.")
  }
  if (tr == "filter" && is.null(args$keep) && is.null(args$drop)) add("filter needs keep or drop.")
  if (tr == "pivot" && !is.null(args$direction)) {
    if (args$direction == "wider") {
      need <- setdiff(c("names", "values"), names(args))
    } else if (args$direction == "longer") {
      need <- setdiff(c("cols", "names_to", "values_to"), names(args))
    } else {
      need <- character()
      add("pivot direction must be wider or longer.")
    }
    if (length(need)) add("pivot ", args$direction, " needs argument(s): ", paste(need, collapse = ", "), ".")
  }
  p
}

# "format=DD MON YYYY; type=datetime" -> list(format = "DD MON YYYY", type = "datetime")
parse_args <- function(s) {
  if (is.null(s) || is.na(s) || !nzchar(trimws(s))) return(list())
  parts <- sub("^\\s+", "", strsplit(s, ";", fixed = TRUE)[[1]])
  parts <- parts[nzchar(trimws(parts))]
  eq <- regexpr("=", parts, fixed = TRUE)
  if (any(eq < 1)) stop("ARGS must be name=value pairs separated by ';': ", s, call. = FALSE)
  vals <- substring(parts, eq + 1)
  vals <- ifelse(trimws(vals) == "", vals, trimws(vals))   # keep sep=" "
  stats::setNames(as.list(vals), trimws(substr(parts, 1, eq - 1)))
}
