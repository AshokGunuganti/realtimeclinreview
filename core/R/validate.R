#' Key variables the as-of engine needs
#'
#' `FORMOID + RECID` identifies a record across versions; `VERSION` and
#' `AVAILDT` say which version was current on a review date. The optional
#' `DELFL` column marks a version that records a deletion (see [as_of()]).
#'
#' @return Character vector of variable names.
#' @export
cdm_key_vars <- function() {
  c("FORMOID", "RECID", "VERSION", "AVAILDT")
}

#' Validate a collected data model table
#'
#' Checks the contract every CDM table must meet before the as-of engine runs
#' on it: the standard header is present, `FORMOID + RECID + VERSION` is
#' unique, versions are positive whole numbers, every version has an
#' availability date, and a later version never became available before an
#' earlier one.
#'
#' @param x A data frame.
#' @param header If `TRUE`, require the full standard header
#'   ([cdm_header_vars()]); if `FALSE`, only the key variables
#'   ([cdm_key_vars()]).
#' @return `x`, invisibly. Signals an error describing the first problem found.
#' @export
validate_cdm <- function(x, header = TRUE) {
  if (!is.data.frame(x)) stop("CDM table must be a data frame.", call. = FALSE)
  need <- if (header) cdm_header_vars() else cdm_key_vars()
  miss <- setdiff(need, names(x))
  if (length(miss)) {
    stop("CDM table is missing header variable(s): ", paste(miss, collapse = ", "), ".", call. = FALSE)
  }
  if (!inherits(x$AVAILDT, c("Date", "POSIXct"))) {
    stop("AVAILDT must be a Date or POSIXct.", call. = FALSE)
  }
  if (anyNA(x$AVAILDT)) stop("AVAILDT is missing on ", sum(is.na(x$AVAILDT)), " row(s).", call. = FALSE)
  if (anyNA(x$FORMOID) || anyNA(x$RECID)) stop("FORMOID and RECID must not be missing.", call. = FALSE)
  v <- x$VERSION
  if (!is.numeric(v) || anyNA(v) || any(v < 1) || any(v != round(v))) {
    stop("VERSION must be a whole number >= 1 on every row.", call. = FALSE)
  }
  if ("DELFL" %in% names(x) && (!is.logical(x$DELFL) || anyNA(x$DELFL))) {
    stop("DELFL must be logical with no missing values.", call. = FALSE)
  }

  key <- record_key(x)
  dup <- duplicated(paste(key, v, sep = "\x1f"))
  if (any(dup)) {
    stop("FORMOID + RECID + VERSION is not unique; first duplicate: ",
         x$FORMOID[dup][1], " / ", x$RECID[dup][1], " / ", v[dup][1], ".", call. = FALSE)
  }

  o <- order(key, v)
  k <- key[o]
  a <- as.numeric(x$AVAILDT[o])
  back <- k[-1] == k[-length(k)] & a[-1] < a[-length(a)]
  if (any(back)) {
    i <- o[which(back)[1] + 1]
    stop("A later version became available before an earlier one: ",
         x$FORMOID[i], " / ", x$RECID[i], " version ", v[i], ".", call. = FALSE)
  }
  invisible(x)
}

# Internal: one string per record identity (FORMOID + RECID).
record_key <- function(x) paste(x$FORMOID, x$RECID, sep = "\x1f")
