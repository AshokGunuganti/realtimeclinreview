# Transform vocabulary for the mapping spec. Deliberately small: each
# transform is one function, unit-tested once. Field transforms return one
# value per source row; table steps (filter, pivot) reshape the source first.
#
# A field transform is function(x, args, ctx): `x` is a list of source
# vectors (one per source field), `args` a named list of strings, and `ctx`
# carries the source table, other sources, schedule, codelists and an
# `issue(rows, values, detail)` callback for the parse log. It returns a
# vector, or list(value = , extra = list(VAR = vector)) to add columns.

transform_defs <- list(
  direct   = list(n_src = c(1, 1),   args = character(),               required = character()),
  date     = list(n_src = c(1, 1),   args = c("format", "type"),       required = "format"),
  decode   = list(n_src = c(1, 1),   args = "codelist",                required = "codelist"),
  recode   = list(n_src = c(1, 1),   args = c("map", "default"),       required = "map"),
  visit    = list(n_src = c(1, 1),   args = "by",                      required = "by"),
  number   = list(n_src = c(1, 1),   args = "flag",                    required = character()),
  lookup   = list(n_src = c(1, 1),   args = c("table", "by", "value"), required = c("table", "by", "value")),
  concat   = list(n_src = c(1, Inf), args = "sep",                     required = character()),
  constant = list(n_src = c(0, 0),   args = "value",                   required = "value"),
  derive   = list(n_src = c(0, Inf), args = NULL,                      required = "fn"),
  filter   = list(table = TRUE, args = c("field", "keep", "drop"),     required = "field"),
  pivot    = list(table = TRUE, args = c("direction", "names", "values", "id", "cols", "names_to", "values_to"),
                  required = "direction")
)

#' Transforms available in the mapping spec
#'
#' @return Character vector of transform names. `filter` and `pivot` are
#'   table steps; the rest map one target field.
#' @export
mapping_transforms <- function() names(transform_defs)

is_table_step <- function(transform) isTRUE(transform_defs[[transform]]$table)

# Field transforms ---------------------------------------------------------------

tf_direct <- function(x, args, ctx) x[[1]]

tf_date <- function(x, args, ctx) {
  type <- if (is.null(args$type)) "date" else args$type
  parse_spec_date(x[[1]], args$format, type, ctx)
}

tf_decode <- function(x, args, ctx) {
  cl <- ctx$codelists[[args$codelist]]
  v <- x[[1]]
  m <- match(v, cl$code)
  bad <- !is.na(v) & is.na(m)
  if (any(bad)) ctx$issue(which(bad), v[bad], paste0("Code not in codelist ", args$codelist))
  cl$decode[m]
}

tf_recode <- function(x, args, ctx) {
  map <- parse_value_map(args$map)
  v <- x[[1]]
  out <- unname(map[v])
  hit <- !is.na(v) & v %in% names(map)
  default <- if (is.null(args$default)) "keep" else args$default
  miss <- !is.na(v) & !hit
  if (default == "keep") {
    out[miss] <- v[miss]
  } else if (any(miss)) {
    ctx$issue(which(miss), v[miss], "Value not in recode map")
  }
  out[is.na(v)] <- NA_character_
  out
}

tf_visit <- function(x, args, ctx) {
  sched <- ctx$schedule
  if (is.null(sched)) stop("The visit transform needs a visit schedule.", call. = FALSE)
  if (!args$by %in% names(sched)) {
    stop("Visit schedule has no column ", args$by, ".", call. = FALSE)
  }
  v <- x[[1]]
  m <- match(v, as.character(sched[[args$by]]))
  bad <- !is.na(v) & is.na(m)
  if (any(bad)) ctx$issue(which(bad), v[bad], paste0("Visit not in schedule (", args$by, ")"))
  as.numeric(sched$VISITNUM[m])
}

tf_number <- function(x, args, ctx) {
  v <- trimws(x[[1]])
  below <- !is.na(v) & grepl("^<", v)
  num <- suppressWarnings(as.numeric(v))
  num[below] <- NA_real_
  bad <- !is.na(v) & !below & is.na(num)
  if (any(bad)) ctx$issue(which(bad), v[bad], "Value is not a number")
  if (is.null(args$flag)) {
    if (any(below)) ctx$issue(which(below), v[below], "Below-limit result but no flag variable set")
    return(num)
  }
  flag <- below
  flag[is.na(v)] <- NA
  list(value = num, extra = stats::setNames(list(flag), args$flag))
}

tf_lookup <- function(x, args, ctx) {
  tbl <- ctx$sources[[args$table]]
  if (is.null(tbl)) stop("Lookup table ", args$table, " was not read.", call. = FALSE)
  for (col in c(args$by, args$value)) {
    if (!col %in% names(tbl)) stop("Lookup table ", args$table, " has no column ", col, ".", call. = FALSE)
  }
  pairs <- unique(tbl[c(args$by, args$value)])
  amb <- unique(pairs[[args$by]][duplicated(pairs[[args$by]])])
  v <- x[[1]]
  m <- match(v, pairs[[args$by]])
  bad <- !is.na(v) & is.na(m)
  if (any(bad)) ctx$issue(which(bad), v[bad], paste0("No match in ", args$table))
  conflict <- !is.na(v) & v %in% amb
  if (any(conflict)) ctx$issue(which(conflict), v[conflict], paste0("More than one value in ", args$table))
  pairs[[args$value]][m]
}

tf_concat <- function(x, args, ctx) {
  sep <- if (is.null(args$sep)) " " else args$sep
  m <- do.call(cbind, lapply(x, as.character))
  apply(m, 1, function(r) {
    r <- r[!is.na(r) & r != ""]
    if (length(r)) paste(r, collapse = sep) else NA_character_
  })
}

tf_constant <- function(x, args, ctx) rep(args$value, nrow(ctx$src))

tf_derive <- function(x, args, ctx) {
  fn <- derive_function(args$fn)
  fn(x, args, ctx)
}

# Table steps --------------------------------------------------------------------

step_filter <- function(df, args) {
  if (!args$field %in% names(df)) stop("Filter field ", args$field, " is not in the source.", call. = FALSE)
  v <- df[[args$field]]
  keep <- rep(TRUE, nrow(df))
  if (!is.null(args$keep)) keep <- keep & v %in% split_list(args$keep)
  if (!is.null(args$drop)) keep <- keep & !(v %in% split_list(args$drop))
  df[keep, , drop = FALSE]
}

step_pivot <- function(df, args) {
  out <- if (args$direction == "wider") {
    id <- if (is.null(args$id)) NULL else split_list(args$id)
    tidyr::pivot_wider(df, id_cols = dplyr::all_of(id), names_from = dplyr::all_of(args$names),
                       values_from = dplyr::all_of(args$values))
  } else {
    tidyr::pivot_longer(df, cols = dplyr::all_of(split_list(args$cols)),
                        names_to = args$names_to, values_to = args$values_to)
  }
  as.data.frame(out, stringsAsFactors = FALSE)
}

# Helpers ------------------------------------------------------------------------

split_list <- function(s) trimws(strsplit(s, ",", fixed = TRUE)[[1]])

# "Y:Yes,N:No" -> c(Y = "Yes", N = "No")
parse_value_map <- function(s) {
  parts <- split_list(s)
  kv <- strsplit(parts, ":", fixed = TRUE)
  if (any(lengths(kv) != 2)) stop("Recode map must look like 'Y:Yes,N:No'.", call. = FALSE)
  stats::setNames(trimws(vapply(kv, `[`, "", 2)), trimws(vapply(kv, `[`, "", 1)))
}

# Spec date format ("DD MON YYYY HH:MM:SS", "YYYY-MM-DD", "ISO") -> strptime formats.
date_formats <- function(fmt) {
  if (toupper(fmt) == "ISO") {
    return(c("%Y-%m-%dT%H:%M:%S", "%Y-%m-%dT%H:%M", "%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M", "%Y-%m-%d"))
  }
  f <- fmt
  for (p in list(c("HH:MM:SS", "%H:%M:%S"), c("HH:MM", "%H:%M"), c("YYYY", "%Y"),
                 c("MON", "%b"), c("MM", "%m"), c("DD", "%d"))) {
    f <- sub(p[1], p[2], f, fixed = TRUE)
  }
  f
}

valid_date_format <- function(fmt) {
  toupper(fmt) == "ISO" || !grepl("[A-Za-z]", gsub("%[a-zA-Z]", "", date_formats(fmt)))
}

parse_spec_date <- function(v, fmt, type, ctx) {
  v <- trimws(v)
  old <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "C")
  on.exit(Sys.setlocale("LC_TIME", old), add = TRUE)

  ts <- as.POSIXct(rep(NA_real_, length(v)), tz = "UTC")
  for (f in date_formats(fmt)) {
    todo <- is.na(ts) & !is.na(v)
    if (!any(todo)) break
    ts[todo] <- as.POSIXct(v[todo], format = f, tz = "UTC")
  }
  has_time <- grepl("%H", paste(date_formats(fmt), collapse = ""))

  out <- switch(type,
    date = as.Date(ts, tz = "UTC"),
    datetime = ts,
    text = {
      txt <- ifelse(is.na(ts), NA_character_,
                    format(ts, if (has_time) "%Y-%m-%dT%H:%M" else "%Y-%m-%d", tz = "UTC"))
      part <- is.na(ts) & !is.na(v)
      txt[part] <- partial_iso(v[part])
      txt
    }
  )
  bad <- !is.na(v) & is.na(out)
  if (any(bad)) ctx$issue(which(bad), v[bad], paste0("Date could not be parsed as ", fmt))
  out
}

# Partial dates to truncated ISO: "UN JUN 2026" -> "2026-06", "UN UNK 2026" -> "2026",
# "2026-06" -> "2026-06". Anything else -> NA.
partial_iso <- function(v) {
  out <- rep(NA_character_, length(v))
  iso <- grepl("^[0-9]{4}(-[0-9]{2})?$", v)
  out[iso] <- v[iso]
  rx <- "^(UN|UNK|--|[0-9]{1,2}) ([A-Za-z]{3}) ([0-9]{4})$"
  dm <- !iso & grepl(rx, v)
  if (any(dm)) {
    mon <- toupper(sub(rx, "\\2", v[dm]))
    yr <- sub(rx, "\\3", v[dm])
    mi <- match(mon, toupper(month.abb))
    out[dm] <- ifelse(is.na(mi), yr, sprintf("%s-%02d", yr, mi))
    out[dm][mon != "UNK" & is.na(mi)] <- NA_character_
  }
  out
}

# Registry of derive functions ------------------------------------------------------

derive_registry <- new.env(parent = emptyenv())

#' Register a derive function for the mapping spec
#'
#' A study registers named functions for anything the transform vocabulary
#' cannot express, then names them in the spec as `derive` with `fn=<name>`.
#'
#' @param name Name used in the spec.
#' @param fn `function(x, args, ctx)` returning one value per source row. `x`
#'   is a list of the source field vectors.
#' @return `name`, invisibly.
#' @export
register_derive <- function(name, fn) {
  stopifnot(is.character(name), length(name) == 1, is.function(fn))
  assign(name, fn, envir = derive_registry)
  invisible(name)
}

derive_function <- function(name) {
  if (!exists(name, envir = derive_registry, inherits = FALSE)) {
    stop("No derive function registered as '", name, "'.", call. = FALSE)
  }
  get(name, envir = derive_registry, inherits = FALSE)
}
