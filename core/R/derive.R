# Derivation registry. Like checks: a derivation type is one generic function
# with declared parameters; a study configures derivations in study.yml. They
# run in order on the as-of snapshot, so a late or corrected source value
# moves every derived value that depends on it. Derived variables sit beside
# collected ones and never overwrite them; subject-level reference dates and
# status go to a derived subject table, ADSL.

derivation_type_registry <- new.env(parent = emptyenv())

#' Register a derivation type
#'
#' @param name Type name, used as `type` in study.yml.
#' @param fn `function(state, params)` returning the updated `state`. `state`
#'   holds `tables` (the as-of snapshot, with earlier derived columns), `adsl`
#'   (subject-level table, once created), `schedule` and `asof`. Add columns
#'   with `add_derived()` so collected values are never overwritten.
#' @param required,optional Parameter names the type requires or accepts.
#' @return `name`, invisibly.
#' @export
register_derivation_type <- function(name, fn, required = character(), optional = character()) {
  stopifnot(is.character(name), length(name) == 1, is.function(fn))
  assign(name, list(fn = fn, required = required, optional = optional), envir = derivation_type_registry)
  invisible(name)
}

#' Registered derivation types
#' @return Character vector of type names.
#' @export
derivation_types <- function() sort(ls(derivation_type_registry))

#' Build the derivations a study configures
#'
#' @param config List of entries, e.g. the `derivations` section of
#'   study.yml. Each has `id`, `type`, optional `description`, and `params`.
#' @return A validated list of derivations, named by ID, in run order.
#' @export
define_derivations <- function(config) {
  out <- list()
  p <- character()
  for (e in config) {
    id <- if (is.null(e$id)) "(no id)" else e$id
    add <- function(...) p <<- c(p, paste0(id, ": ", ...))
    if (is.null(e$id)) add("id is missing.")
    if (id %in% names(out)) add("defined more than once.")
    if (is.null(e$params)) e$params <- list()
    type <- if (is.null(e$type)) NULL else get0(e$type, envir = derivation_type_registry, inherits = FALSE)
    if (is.null(type)) {
      add("unknown derivation type '", if (is.null(e$type)) "" else e$type, "'.")
    } else {
      need <- setdiff(type$required, names(e$params))
      if (length(need)) add("parameter(s) required by ", e$type, ": ", paste(need, collapse = ", "), ".")
      extra <- setdiff(names(e$params), c(type$required, type$optional))
      if (length(extra)) add("parameter(s) not used by ", e$type, ": ", paste(extra, collapse = ", "), ".")
    }
    out[[id]] <- e
  }
  if (length(p)) stop("Derivation configuration is not valid:\n", paste0("- ", p, collapse = "\n"), call. = FALSE)
  out
}

#' Derive on the snapshot for a review date
#'
#' Takes each CDM table as of `asof` ([as_of()]) and runs the derivations in
#' order.
#'
#' @param tables Named list of CDM tables (all versions).
#' @param derivations Derivations from [define_derivations()].
#' @param asof Review date.
#' @param schedule Visit schedule data frame.
#' @param thresholds Named list of study thresholds, for `"$name"` parameters.
#' @return A list: `tables` (snapshot tables with derived columns added) and
#'   `adsl` (one row per subject, or `NULL` if no derivation built it).
#' @export
derive_snapshot <- function(tables, derivations, asof, schedule = NULL, thresholds = list()) {
  snap <- lapply(tables, as_of, d = asof)
  state <- list(tables = snap, adsl = NULL, schedule = schedule, asof = asof,
                collected = lapply(snap, names))
  for (dv in derivations) {
    type <- get(dv$type, envir = derivation_type_registry, inherits = FALSE)
    state <- type$fn(state, resolve_params(dv$params, thresholds, dv$id))
  }
  list(tables = state$tables, adsl = state$adsl)
}

# Add derived columns to a form, refusing to overwrite collected or earlier derived values.
add_derived <- function(state, form, cols) {
  t <- state$tables[[form]]
  clash <- intersect(names(cols), names(t))
  if (length(clash)) {
    what <- if (any(clash %in% state$collected[[form]])) "collected" else "already derived"
    stop("Derivation would overwrite ", what, " variable(s) on ", form, ": ", paste(clash, collapse = ", "), ".",
         call. = FALSE)
  }
  for (nm in names(cols)) t[[nm]] <- cols[[nm]]
  state$tables[[form]] <- t
  state
}

add_adsl <- function(state, cols) {
  clash <- intersect(names(cols), names(state$adsl))
  if (length(clash)) stop("ADSL already has variable(s): ", paste(clash, collapse = ", "), ".", call. = FALSE)
  for (nm in names(cols)) state$adsl[[nm]] <- cols[[nm]]
  state
}

# A subject-level reference value (from ADSL) aligned to a table's rows.
adsl_ref <- function(state, var, subjid) {
  if (is.null(state$adsl)) stop("ADSL has not been built; run reference_dates first.", call. = FALSE)
  if (!var %in% names(state$adsl)) stop("ADSL has no variable ", var, ".", call. = FALSE)
  state$adsl[[var]][match(subjid, state$adsl$SUBJID)]
}

dv_table <- function(state, form) {
  t <- state$tables[[form]]
  if (is.null(t)) stop("Form ", form, " is not in the data.", call. = FALSE)
  t
}

dv_field <- function(t, field, form) {
  need_field(t, field, form)
  t[[field]]
}

yn <- function(x) ifelse(is.na(x), NA_character_, ifelse(x, "Y", "N"))
