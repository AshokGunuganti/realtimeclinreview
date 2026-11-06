#' Snapshot of a CDM table as of a review date
#'
#' For each record (`FORMOID + RECID`), keeps the highest `VERSION` whose
#' `AVAILDT` is on or before `d`. Records first available after `d` are hidden.
#' Works off the standard header only, so it never needs to know what a form
#' contains.
#'
#' A deletion is recorded as a new version with `DELFL = TRUE` (a tombstone).
#' The pipeline writes one only when the form was extracted in full and the
#' record was absent; a missing or truncated file never produces a deletion.
#' Tables without a `DELFL` column have no deletions.
#'
#' @param x A CDM table (see [validate_cdm()]).
#' @param d Review date: a `Date`, `POSIXct`, or `"YYYY-MM-DD"` string. A date
#'   includes everything available up to the end of that day (UTC).
#' @param include_deleted If `TRUE`, keep records whose current version is a
#'   deletion.
#' @return The current version of each record available on `d`, ordered by
#'   `FORMOID`, `RECID`. Columns are unchanged.
#' @export
#' @examples
#' ae <- data.frame(FORMOID = "AE", RECID = c(1, 1, 2), VERSION = c(1, 2, 1),
#'                  AVAILDT = as.Date(c("2026-10-01", "2026-10-15", "2026-10-20")),
#'                  AESEV = c("Mild", "Moderate", "Mild"))
#' as_of(ae, "2026-10-10")
#' as_of(ae, "2026-10-31")
as_of <- function(x, d, include_deleted = FALSE) {
  validate_cdm(x, header = FALSE)
  x <- x[available_by(x$AVAILDT, d), , drop = FALSE]
  x <- x[order(x$FORMOID, x$RECID, -x$VERSION), , drop = FALSE]
  x <- x[!duplicated(record_key(x)), , drop = FALSE]
  if (!include_deleted && "DELFL" %in% names(x)) x <- x[!x$DELFL, , drop = FALSE]
  rownames(x) <- NULL
  x
}

#' Change log between two review dates
#'
#' Compares the snapshot at `since` with the snapshot at `d` (the window
#' `(since, d]`) and classifies every record present in either:
#'
#' * **New**: not in the `since` snapshot, present at `d`.
#' * **Changed**: in both, and at least one compared field differs.
#' * **Deleted**: in the `since` snapshot, deleted by `d`.
#' * **Unchanged**: in both with the same values. A new version whose compared
#'   fields are all equal (for example only `UPDDT` moved) counts as Unchanged,
#'   so review marks are not reset needlessly.
#'
#' A record created and deleted inside the window appears in neither snapshot
#' and is not reported.
#'
#' Compared fields are all columns except `RECID`, `VERSION`, `AVAILDT`,
#' `ENTRYDT`, `UPDDT` and `DELFL`; see [change_fields()] for old and new values.
#'
#' @inheritParams as_of
#' @param since Earlier review date, same forms as `d`; must not be after `d`.
#' @return The `d` snapshot rows (the last live version for Deleted records)
#'   with three columns added: `RECSTAT`, `PREVVER` (version current at
#'   `since`, `NA` for New) and `CHGVARS` (comma-separated changed fields, `""`
#'   if none).
#' @export
change_log <- function(x, since, d) {
  cmp <- compare_snapshots(x, since, d)
  out <- cmp$rows
  out$RECSTAT <- cmp$status
  out$PREVVER <- cmp$prevver
  out$CHGVARS <- vapply(cmp$changed, paste, character(1), collapse = ", ")
  out
}

#' Field-level changes between two review dates
#'
#' One row per changed field of each record that is Changed in
#' [change_log()], with old and new values as text (dates in ISO 8601,
#' missing values as `NA`).
#'
#' @inheritParams change_log
#' @return A data frame with `FORMOID`, `RECID`, `SUBJID` (if present),
#'   `VARIABLE`, `OLD`, `NEW`, `PREVVER` and `VERSION`.
#' @export
change_fields <- function(x, since, d) {
  cmp <- compare_snapshots(x, since, d)
  i <- which(cmp$status == "Changed")
  n <- lengths(cmp$changed[i])
  ri <- rep(i, n)
  vars <- as.character(unlist(cmp$changed[i], use.names = FALSE))
  old <- cmp$old
  new <- cmp$rows
  out <- data.frame(FORMOID = new$FORMOID[ri], RECID = new$RECID[ri], stringsAsFactors = FALSE)
  if ("SUBJID" %in% names(new)) out$SUBJID <- new$SUBJID[ri]
  out$VARIABLE <- vars
  out$OLD <- cell_text(old, cmp$oldrow[ri], vars)
  out$NEW <- cell_text(new, ri, vars)
  out$PREVVER <- cmp$prevver[ri]
  out$VERSION <- new$VERSION[ri]
  out
}

# Internal ---------------------------------------------------------------------

# Fields that are bookkeeping, not collected data, and so never count as a change.
not_compared <- c("RECID", "VERSION", "AVAILDT", "ENTRYDT", "UPDDT", "DELFL")

compare_snapshots <- function(x, since, d) {
  validate_cdm(x, header = FALSE)
  if (date_after(since, d)) {
    stop("`since` must not be after `d`.", call. = FALSE)
  }
  old <- as_of(x, since)
  now <- as_of(x, d, include_deleted = TRUE)
  now_del <- if ("DELFL" %in% names(now)) now$DELFL else rep(FALSE, nrow(now))

  m <- match(record_key(now), record_key(old))
  in_old <- !is.na(m)
  keep <- in_old | !now_del           # drop records created and deleted in the window
  now <- now[keep, , drop = FALSE]
  m <- m[keep]
  in_old <- in_old[keep]
  now_del <- now_del[keep]

  # Deleted records report their last live values.
  rows <- now
  rows[now_del, ] <- old[m[now_del], , drop = FALSE]

  vars <- setdiff(names(x), not_compared)
  changed <- vector("list", nrow(now))
  for (j in seq_len(nrow(now))) changed[[j]] <- character(0)
  both <- which(in_old & !now_del)
  for (v in vars) {
    diff <- !same_value(old[[v]][m[both]], now[[v]][both])
    for (j in both[diff]) changed[[j]] <- c(changed[[j]], v)
  }

  status <- as.character(ifelse(!in_old, "New",
                   ifelse(now_del, "Deleted",
                          ifelse(lengths(changed) > 0, "Changed", "Unchanged"))))
  list(rows = rows, old = old, oldrow = m, status = status,
       prevver = old$VERSION[m], changed = changed)
}

# Element-wise equality treating NA as equal to NA.
same_value <- function(a, b) {
  if (is.factor(a)) a <- as.character(a)
  if (is.factor(b)) b <- as.character(b)
  na_a <- is.na(a)
  na_b <- is.na(b)
  eq <- na_a & na_b
  both <- !na_a & !na_b
  eq[both] <- a[both] == b[both]
  eq
}

cell_text <- function(df, rows, vars) {
  out <- character(length(rows))
  for (k in seq_along(rows)) {
    val <- df[[vars[k]]][rows[k]]
    out[k] <- if (is.na(val)) NA_character_ else if (inherits(val, "POSIXct")) {
      format(val, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    } else as.character(val)
  }
  out
}

# TRUE where a version with this availability date is visible on review date d.
# A date-only d includes the whole of that day (UTC).
available_by <- function(availdt, d) {
  d <- review_date(d)
  if (inherits(availdt, "POSIXct") && inherits(d, "Date")) return(as.Date(availdt, tz = "UTC") <= d)
  if (inherits(availdt, "Date") && inherits(d, "POSIXct")) return(availdt <= as.Date(d, tz = "UTC"))
  availdt <= d
}

date_after <- function(a, b) {
  a <- review_date(a)
  b <- review_date(b)
  if (class(a)[1] != class(b)[1]) {
    a <- as.Date(a, tz = "UTC")
    b <- as.Date(b, tz = "UTC")
  }
  a > b
}

review_date <- function(d) {
  if (length(d) != 1 || is.na(d)) stop("Review date must be a single, non-missing value.", call. = FALSE)
  if (is.character(d)) {
    d <- if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", d)) as.Date(d, format = "%Y-%m-%d") else as.Date(NA)
    if (is.na(d)) stop("Review date must be a date, e.g. \"2026-10-22\".", call. = FALSE)
  }
  if (!inherits(d, c("Date", "POSIXct"))) {
    stop("Review date must be a Date, POSIXct or \"YYYY-MM-DD\" string.", call. = FALSE)
  }
  d
}
