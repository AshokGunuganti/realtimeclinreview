#' Read an extract of delimited source files
#'
#' Generic CSV connector: reads every file a mapping needs (form sources and
#' lookup tables) as text, trimming blanks to `NA`. Typing happens in the
#' mapping, so nothing is lost or guessed here.
#'
#' @param dir Extract folder.
#' @param mapping A mapping from [compile_spec()] or [read_mapping()].
#' @return Named list of data frames, one per file.
#' @export
read_extract <- function(dir, mapping) {
  files <- unique(c(
    vapply(mapping$forms, `[[`, "", "source"),
    unlist(lapply(mapping$forms, function(f) {
      lapply(f$fields, function(e) if (identical(e$transform, "lookup")) e$args$table)
    }))
  ))
  out <- list()
  for (fl in files) {
    path <- file.path(dir, fl)
    if (!file.exists(path)) stop("Source file missing from extract: ", fl, ".", call. = FALSE)
    df <- utils::read.csv(path, colClasses = "character", na.strings = character(0),
                          check.names = FALSE, stringsAsFactors = FALSE)
    df[] <- lapply(df, function(v) { v <- trimws(v); v[v == ""] <- NA_character_; v })
    out[[fl]] <- df
  }
  out
}

#' Map an extract to collected data model tables
#'
#' Applies each form's table steps, then each field transform in spec order,
#' then fills the automatic header variables: `FORMOID`, `RECID` (the form's
#' key fields joined with `|`) and `VERSION`. Rows the mapping cannot place in
#' time (no `AVAILDT`) are dropped and logged.
#'
#' @param mapping A mapping from [compile_spec()] or [read_mapping()].
#' @param sources Named list of source tables, from [read_extract()].
#' @param schedule Visit schedule data frame (needs `VISITNUM` and the
#'   columns named by `visit` transforms).
#' @return A list with `tables` (named list of CDM tables, each passing
#'   [validate_cdm()]) and `issues` (the parse log: one row per value the
#'   mapping could not use, with the record's `RECID` and the `TRANSFORM` that
#'   failed; `key` for duplicate keys, `availdt` for rows with no `AVAILDT`).
#' @export
map_extract <- function(mapping, sources, schedule = NULL) {
  log <- new.env(parent = emptyenv())
  log$rows <- list()
  tables <- list()
  for (id in names(mapping$forms)) {
    tables[[id]] <- map_form(id, mapping$forms[[id]], mapping, sources, schedule, log)
  }
  issues <- if (length(log$rows)) do.call(rbind, log$rows) else
    data.frame(FORMOID = character(), RECID = character(), VARIABLE = character(), TRANSFORM = character(),
               ROW = integer(), SUBJID = character(), VALUE = character(), DETAIL = character(),
               stringsAsFactors = FALSE)
  rownames(issues) <- NULL
  list(tables = tables, issues = issues)
}

map_form <- function(id, f, mapping, sources, schedule, log) {
  df <- sources[[f$source]]
  if (is.null(df)) stop("Form ", id, ": source ", f$source, " was not read.", call. = FALSE)

  # Required source fields must exist before anything runs.
  need <- unique(c(f$key, unlist(lapply(f$fields, function(e) if (isTRUE(e$required)) e$source))))
  miss <- setdiff(need, names(df))
  if (length(miss)) {
    stop("Form ", id, ": ", f$source, " is missing required column(s): ", paste(miss, collapse = ", "), ".",
         call. = FALSE)
  }
  for (s in f$steps) {
    df <- switch(s$transform, filter = step_filter(df, s$args), pivot = step_pivot(df, s$args))
  }
  rownames(df) <- NULL

  recid <- do.call(paste, c(unname(as.list(df[f$key])), sep = "|"))
  issue_for <- function(var, transform) {
    function(rows, values, detail) {
      log$rows[[length(log$rows) + 1]] <- data.frame(
        FORMOID = id, RECID = recid[rows], VARIABLE = var, TRANSFORM = transform, ROW = as.integer(rows),
        SUBJID = if ("SUBJID" %in% names(df)) df$SUBJID[rows] else NA_character_,
        VALUE = as.character(values), DETAIL = detail, stringsAsFactors = FALSE)
    }
  }
  ctx <- list(form = id, src = df, sources = sources, schedule = schedule, codelists = mapping$codelists)

  out <- list()
  for (e in f$fields) {
    x <- lapply(e$source, function(col) if (col %in% names(df)) df[[col]] else rep(NA_character_, nrow(df)))
    ctx$variable <- e$variable
    ctx$issue <- issue_for(e$variable, e$transform)
    fn <- get(paste0("tf_", e$transform), mode = "function")
    res <- fn(x, e$args, ctx)
    if (is.list(res) && !is.null(res$value)) {
      out[[e$variable]] <- res$value
      for (nm in names(res$extra)) out[[nm]] <- res$extra[[nm]]
    } else {
      out[[e$variable]] <- res
    }
  }
  out <- as.data.frame(out, stringsAsFactors = FALSE, optional = TRUE)
  if (!nrow(out)) out <- out[seq_len(nrow(df)), , drop = FALSE]
  out$.ROW <- seq_len(nrow(df))            # source row, for the parse log

  # Automatic and optional header variables.
  out$FORMOID <- rep(id, nrow(df))
  out$RECID <- do.call(paste, c(unname(as.list(df[f$key])), sep = "|"))
  if (is.null(out$FOLDER)) out$FOLDER <- rep(NA_character_, nrow(out))
  if (is.null(out$VISITNUM)) out$VISITNUM <- rep(NA_real_, nrow(out))
  if (is.null(out$UPDDT)) out$UPDDT <- rep(as.Date(NA), nrow(out))

  noavail <- is.na(out$AVAILDT)
  if (any(noavail)) {
    issue_for("AVAILDT", "availdt")(which(noavail), NA_character_, "No availability date; record dropped")
    out <- out[!noavail, , drop = FALSE]
  }
  out <- out[order(out$RECID, out$AVAILDT), , drop = FALSE]
  if (f$versioning == "rows") {
    out$VERSION <- stats::ave(seq_len(nrow(out)), out$RECID, FUN = seq_along)
  } else {
    dup <- duplicated(out$RECID, fromLast = TRUE)
    if (any(dup)) {
      issue_for("RECID", "key")(out$.ROW[dup], out$RECID[dup], "Duplicate record key; earlier row dropped")
      out <- out[!dup, , drop = FALSE]
    }
    out$VERSION <- rep(1L, nrow(out))
  }

  out <- out[c(cdm_header_vars(), setdiff(names(out), c(cdm_header_vars(), ".ROW")))]
  rownames(out) <- NULL
  validate_cdm(out)
  out
}
