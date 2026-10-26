# Check registry. A check type is one generic function with declared
# parameters; a check is a configured instance of a type with the metadata
# reviewers see (ID, name, tier, timing, severity, owner). Every check returns
# findings in the same shape, so the app, the workbook and review state treat
# them identically.

check_tiers <- c("structural", "cross-form", "reconciliation", "clinical")
check_timings <- c("load", "asof")
check_severities <- c("error", "warning", "info")
check_owners <- c("data management", "clinical", "safety", "sites")

check_type_registry <- new.env(parent = emptyenv())

#' Register a check type
#'
#' A check type is a generic function that a study configures into one or
#' more checks. The library registers its own types when the package loads;
#' a study can register more from its study folder.
#'
#' @param name Type name, used as `type` in study.yml.
#' @param fn `function(ctx, params)` returning a data frame of raw findings
#'   with columns `SUBJID`, `FORMOID`, `RECID`, `VISITNUM`, `FIELD`, `VALUE`,
#'   `MESSAGE` and `DEPENDS` (text of the values the finding relies on; its
#'   hash decides when a review mark resets). `ctx` holds `tables` (CDM
#'   snapshot as of the review date), `issues` (parse log), `schedule` and
#'   `asof`.
#' @param required,optional Parameter names the type requires or accepts.
#' @param timing Default timing: `"load"` (once per extract) or `"asof"`
#'   (re-run for each review date).
#' @return `name`, invisibly.
#' @export
register_check_type <- function(name, fn, required = character(), optional = character(), timing = "asof") {
  stopifnot(is.character(name), length(name) == 1, is.function(fn), timing %in% check_timings)
  assign(name, list(fn = fn, required = required, optional = optional, timing = timing), envir = check_type_registry)
  invisible(name)
}

#' Registered check types
#' @return Character vector of type names.
#' @export
check_types <- function() sort(ls(check_type_registry))

#' Built-in structural checks
#'
#' Checks the library ships ready to switch on by ID in study.yml. They need
#' no parameters.
#'
#' @return A list of check definitions (see [define_checks()]).
#' @export
library_checks <- function() {
  pl <- function(id, name, description, transforms, severity) {
    list(id = id, type = "parse_log", name = name, description = description, tier = "structural",
         timing = "load", severity = severity, owner = "data management", params = list(transforms = transforms))
  }
  list(
    pl("STR-001", "Dates parse", "Date and time values parse in the format the spec declares", "date", "error"),
    pl("STR-002", "Values convert", "Numbers, codes, recodes and lookups resolve",
       c("number", "decode", "recode", "lookup", "derive"), "error"),
    pl("STR-003", "Visits match the schedule", "EDC folder or vendor visit label is in the visit schedule",
       "visit", "warning"),
    pl("STR-004", "Record keys unique", "No two source rows share a record key on a single-version form",
       "key", "warning"),
    list(id = "STR-005", type = "subject_exists", name = "Subjects exist",
         description = "Every subject on any form exists on the subject form", tier = "structural",
         timing = "asof", severity = "error", owner = "data management", params = list(subject_form = "DM")),
    pl("STR-006", "Records have an availability date", "Rows with no availability date cannot be placed in time",
       "availdt", "error")
  )
}

#' Build the checks a study switches on
#'
#' Each entry is either the ID of a library check (from [library_checks()]),
#' optionally with `params` overrides, or a full definition: `id`, `type`,
#' `name`, `description`, `tier`, `timing` (defaults to the type's),
#' `severity`, `owner` and `params`. Parameter values written `"$name"` are
#' taken from the study's thresholds when the checks run.
#'
#' @param config List of entries, e.g. the `checks` section of study.yml.
#' @return A validated list of checks, named by ID.
#' @export
define_checks <- function(config) {
  lib <- library_checks()
  names(lib) <- vapply(lib, `[[`, "", "id")
  out <- list()
  p <- character()
  for (e in config) {
    if (is.character(e)) e <- list(id = e)
    id <- if (is.null(e$id)) "(no id)" else e$id
    if (id %in% names(lib) && is.null(e$type)) {
      chk <- lib[[id]]
      if (!is.null(e$params)) chk$params <- utils::modifyList(chk$params, e$params)
      for (f in setdiff(names(e), c("id", "params"))) chk[[f]] <- e[[f]]
    } else {
      chk <- e
      if (is.null(chk$params)) chk$params <- list()
      type <- if (is.null(chk$type)) NULL else get0(chk$type, envir = check_type_registry, inherits = FALSE)
      if (is.null(chk$timing) && !is.null(type)) chk$timing <- type$timing
    }
    p <- c(p, check_problems(chk))
    if (id %in% names(out)) p <- c(p, paste0(id, ": defined more than once."))
    out[[id]] <- chk
  }
  if (length(p)) stop("Check configuration is not valid:\n", paste0("- ", p, collapse = "\n"), call. = FALSE)
  out
}

check_problems <- function(chk) {
  id <- if (is.null(chk$id)) "(no id)" else chk$id
  p <- character()
  add <- function(...) p <<- c(p, paste0(id, ": ", ...))
  for (f in c("id", "type", "name", "description", "tier", "timing", "severity", "owner")) {
    if (is.null(chk[[f]]) || !nzchar(chk[[f]])) add(f, " is missing.")
  }
  allowed <- list(tier = check_tiers, timing = check_timings, severity = check_severities, owner = check_owners)
  for (f in names(allowed)) {
    if (!is.null(chk[[f]]) && !chk[[f]] %in% allowed[[f]]) {
      add(f, " must be one of ", paste(allowed[[f]], collapse = ", "), ".")
    }
  }
  if (!is.null(chk$type)) {
    type <- get0(chk$type, envir = check_type_registry, inherits = FALSE)
    if (is.null(type)) {
      add("unknown check type '", chk$type, "'.")
    } else {
      need <- setdiff(type$required, names(chk$params))
      if (length(need)) add("parameter(s) required by ", chk$type, ": ", paste(need, collapse = ", "), ".")
      extra <- setdiff(names(chk$params), c(type$required, type$optional))
      if (length(extra)) add("parameter(s) not used by ", chk$type, ": ", paste(extra, collapse = ", "), ".")
    }
  }
  p
}

#' Check definitions as a table
#'
#' One row per check, for the spec's Data Checks sheet and the workbook's
#' contents sheet, so both are generated from the registry.
#'
#' @param checks Checks from [define_checks()].
#' @return A data frame.
#' @export
checks_table <- function(checks) {
  fmt_params <- function(p) {
    if (!length(p)) return("")
    paste(vapply(names(p), function(n) {
      v <- p[[n]]
      v <- if (is.list(v)) paste(vapply(v, function(c) paste(unlist(c), collapse = " "), ""), collapse = "; ") else
        paste(v, collapse = ", ")
      paste0(n, "=", v)
    }, ""), collapse = "; ")
  }
  data.frame(
    ID = vapply(checks, `[[`, "", "id"),
    NAME = vapply(checks, `[[`, "", "name"),
    DESCRIPTION = vapply(checks, `[[`, "", "description"),
    TIER = vapply(checks, `[[`, "", "tier"),
    APPLIES_TO = vapply(checks, function(c) paste(check_forms(c), collapse = ", "), ""),
    TIMING = vapply(checks, `[[`, "", "timing"),
    SEVERITY = vapply(checks, `[[`, "", "severity"),
    OWNER = vapply(checks, `[[`, "", "owner"),
    TYPE = vapply(checks, `[[`, "", "type"),
    PARAMS = vapply(checks, function(c) fmt_params(c$params), ""),
    row.names = NULL, stringsAsFactors = FALSE)
}

check_forms <- function(chk) {
  p <- chk$params
  f <- unique(unlist(p[intersect(names(p), c("form", "ref_form", "form_a", "form_b", "subject_form"))]))
  if (length(f)) f else "All forms"
}

finding_cols <- c("FINDINGKEY", "CHECKID", "SEVERITY", "OWNER", "SUBJID", "SITEID", "FORMOID", "RECID",
                  "VISITNUM", "ANCHOR", "FIELD", "VERSION", "VALUE", "MESSAGE", "DEPHASH")

#' Run checks for a review date
#'
#' As-of checks see each CDM table as of `asof` ([as_of()]); load checks see
#' the latest version of every record and the extract's parse log.
#'
#' @param tables Named list of CDM tables (all versions), e.g. from
#'   [map_extract()].
#' @param checks Checks from [define_checks()].
#' @param asof Review date.
#' @param issues Parse log from [map_extract()].
#' @param schedule Visit schedule data frame.
#' @param thresholds Named list of study thresholds, for `"$name"` parameters.
#' @return A list: `findings` (one row per finding; `FINDINGKEY` is
#'   check + subject + anchor + field and is unique) and `summary` (one row
#'   per check with its finding count and result).
#' @export
run_checks <- function(tables, checks, asof, issues = NULL, schedule = NULL, thresholds = list()) {
  snap <- lapply(tables, as_of, d = asof)
  latest <- lapply(tables, function(t) if (nrow(t)) as_of(t, max(t$AVAILDT)) else t)
  sites <- subject_sites(snap)

  found <- list()
  for (chk in checks) {
    type <- get(chk$type, envir = check_type_registry, inherits = FALSE)
    ctx <- list(tables = if (chk$timing == "load") latest else snap, issues = issues, schedule = schedule,
                asof = asof)
    params <- resolve_params(chk$params, thresholds, chk$id)
    raw <- type$fn(ctx, params)
    found[[chk$id]] <- complete_findings(raw, chk, ctx$tables, sites)
  }
  findings <- do.call(rbind, c(list(empty_findings()), unname(found)))
  rownames(findings) <- NULL
  dup <- duplicated(findings$FINDINGKEY)
  if (any(dup)) {
    stop("Check ", findings$CHECKID[dup][1], " returned duplicate finding keys, e.g. ",
         findings$FINDINGKEY[dup][1], ".", call. = FALSE)
  }

  summary <- checks_table(checks)[c("ID", "NAME", "TIER", "TIMING", "SEVERITY", "OWNER")]
  summary$N <- vapply(summary$ID, function(id) sum(findings$CHECKID == id), integer(1))
  summary$RESULT <- ifelse(summary$N == 0, "pass", summary$SEVERITY)
  list(findings = findings, summary = summary)
}

empty_findings <- function() {
  df <- as.data.frame(stats::setNames(replicate(length(finding_cols), character(0), simplify = FALSE), finding_cols),
                      stringsAsFactors = FALSE)
  df$VISITNUM <- numeric(0)
  df$VERSION <- numeric(0)
  df
}

# Add the standard columns to a check type's raw findings.
complete_findings <- function(raw, chk, tables, sites) {
  if (is.null(raw) || !nrow(raw)) return(empty_findings())
  need <- c("SUBJID", "FORMOID", "RECID", "VISITNUM", "FIELD", "VALUE", "MESSAGE")
  miss <- setdiff(need, names(raw))
  if (length(miss)) stop("Check type ", chk$type, " did not return: ", paste(miss, collapse = ", "), call. = FALSE)
  n <- nrow(raw)
  chr <- function(v) as.character(v)
  anchor <- ifelse(!is.na(raw$RECID), paste0(raw$FORMOID, ":", raw$RECID), paste0("VISIT:", raw$VISITNUM))
  depends <- if (is.null(raw$DEPENDS)) chr(raw$VALUE) else chr(raw$DEPENDS)

  version <- rep(NA_real_, n)
  for (fm in intersect(unique(raw$FORMOID), names(tables))) {
    i <- which(raw$FORMOID == fm & !is.na(raw$RECID))
    t <- tables[[fm]]
    version[i] <- t$VERSION[match(raw$RECID[i], as.character(t$RECID))]
  }

  out <- data.frame(
    FINDINGKEY = paste(chk$id, raw$SUBJID, anchor, raw$FIELD, sep = "|"),
    CHECKID = chk$id, SEVERITY = chk$severity, OWNER = chk$owner,
    SUBJID = chr(raw$SUBJID), SITEID = unname(sites[chr(raw$SUBJID)]),
    FORMOID = chr(raw$FORMOID), RECID = chr(raw$RECID), VISITNUM = as.numeric(raw$VISITNUM),
    ANCHOR = anchor, FIELD = chr(raw$FIELD), VERSION = version, VALUE = chr(raw$VALUE),
    MESSAGE = chr(raw$MESSAGE),
    DEPHASH = vapply(depends, function(d) rlang::hash(d), character(1), USE.NAMES = FALSE),
    stringsAsFactors = FALSE)
  out[finding_cols]
}

subject_sites <- function(tables) {
  pairs <- do.call(rbind, lapply(tables, function(t) t[!is.na(t$SUBJID) & !is.na(t$SITEID), c("SUBJID", "SITEID")]))
  if (is.null(pairs) || !nrow(pairs)) return(character())
  pairs <- pairs[!duplicated(pairs$SUBJID), ]
  stats::setNames(as.character(pairs$SITEID), pairs$SUBJID)
}

# "$name" parameter values come from the study's thresholds.
resolve_params <- function(params, thresholds, id) {
  res <- function(v) {
    if (is.list(v)) return(lapply(v, res))
    if (is.character(v) && length(v) == 1 && startsWith(v, "$")) {
      nm <- substring(v, 2)
      if (is.null(thresholds[[nm]])) stop("Check ", id, ": threshold '", nm, "' is not set in study.yml.", call. = FALSE)
      return(thresholds[[nm]])
    }
    v
  }
  lapply(params, res)
}
