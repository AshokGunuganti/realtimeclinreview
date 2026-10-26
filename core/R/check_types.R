# Library check types. Each takes (ctx, params) and returns raw findings:
# SUBJID, FORMOID, RECID, VISITNUM, FIELD, VALUE, MESSAGE, DEPENDS.
#
# Several types take `when`, a list of conditions that must all hold for a
# record to be checked: list(field = "VISITNUM", op = ">", value = 2). Ops:
# ==, !=, <, <=, >, >=, in, not_in, missing, present.

ct_parse_log <- function(ctx, p) {
  iss <- ctx$issues
  if (is.null(iss)) return(NULL)
  iss <- iss[iss$TRANSFORM %in% unlist(p$transforms), , drop = FALSE]
  raw_findings(iss$SUBJID, iss$FORMOID, iss$RECID, NA, iss$VARIABLE, iss$VALUE, iss$DETAIL)
}

ct_subject_exists <- function(ctx, p) {
  subj <- form_table(ctx, p$subject_form)$SUBJID
  out <- lapply(setdiff(names(ctx$tables), p$subject_form), function(fm) {
    t <- ctx$tables[[fm]]
    t <- t[!is.na(t$SUBJID) & !t$SUBJID %in% subj, , drop = FALSE]
    raw_findings(t$SUBJID, fm, t$RECID, t$VISITNUM, "SUBJID", t$SUBJID,
                 paste0("Subject not on ", p$subject_form))
  })
  do.call(rbind, out)
}

ct_missing_value <- function(ctx, p) {
  t <- form_rows(ctx, p$form, p$when)
  out <- lapply(unlist(p$fields), function(f) {
    need_field(t, f, p$form)
    r <- t[is.na(t[[f]]), , drop = FALSE]
    msg <- if (is.null(p$message)) paste(f, "is missing") else p$message
    raw_findings(r$SUBJID, p$form, r$RECID, r$VISITNUM, f, NA, msg, depends = "missing")
  })
  do.call(rbind, out)
}

ct_date_order <- function(ctx, p) {
  t <- form_rows(ctx, p$form, p$when)
  ref <- form_table(ctx, p$ref_form)
  need_field(t, p$field, p$form)
  need_field(ref, p$ref_field, p$ref_form)
  rd <- as_day(ref[[p$ref_field]])
  first <- tapply(as.numeric(rd), ref$SUBJID, function(v) if (all(is.na(v))) NA_real_ else min(v, na.rm = TRUE))
  d <- as.numeric(as_day(t[[p$field]]))
  r <- unname(first[t$SUBJID])
  ok <- switch(p$op, after = d > r, on_or_after = d >= r, before = d < r, on_or_before = d <= r,
               stop("date_order op must be after, on_or_after, before or on_or_before.", call. = FALSE))
  bad <- !is.na(ok) & !ok
  t <- t[bad, , drop = FALSE]
  val <- format(as_day(t[[p$field]]))
  refval <- format(as.Date(r[bad], origin = "1970-01-01"))
  msg <- paste0(p$field, " ", val, " is not ", gsub("_", " ", p$op), " ", p$ref_form, " ", p$ref_field, " ", refval)
  raw_findings(t$SUBJID, p$form, t$RECID, t$VISITNUM, p$field, val, msg, depends = paste(val, refval))
}

ct_range <- function(ctx, p) {
  t <- form_rows(ctx, p$form, p$when)
  need_field(t, p$field, p$form)
  bound <- function(b) {
    if (is.null(b)) return(rep(NA_real_, nrow(t)))
    if (is.character(b) && b %in% names(t)) return(as.numeric(t[[b]]))
    rep(as.numeric(b), nrow(t))
  }
  v <- as.numeric(t[[p$field]])
  lo <- bound(p$min)
  hi <- bound(p$max)
  bad <- !is.na(v) & ((!is.na(lo) & v < lo) | (!is.na(hi) & v > hi))
  t <- t[bad, , drop = FALSE]
  rng <- paste0("[", ifelse(is.na(lo[bad]), "", lo[bad]), ", ", ifelse(is.na(hi[bad]), "", hi[bad]), "]")
  msg <- paste0(p$field, " ", v[bad], " is outside ", rng)
  raw_findings(t$SUBJID, p$form, t$RECID, t$VISITNUM, p$field, v[bad], msg, depends = paste(v[bad], rng))
}

ct_flag_when <- function(ctx, p) {
  t <- form_rows(ctx, p$form, p$when)
  need_field(t, p$field, p$form)
  wf <- unique(vapply(p$when, `[[`, "", "field"))
  dep <- if (nrow(t)) do.call(paste, unname(as.list(t[unique(c(p$field, wf))]))) else character()
  raw_findings(t$SUBJID, p$form, t$RECID, t$VISITNUM, p$field, t[[p$field]], rep(p$message, nrow(t)), depends = dep)
}

ct_reconcile <- function(ctx, p) {
  by <- if (is.null(p$by)) c("SUBJID", "VISITNUM") else unlist(p$by)
  a <- form_rows(ctx, p$form_a, p$when_a)
  b <- form_rows(ctx, p$form_b, p$when_b)
  if (!is.null(p$schedule_flag)) {
    s <- ctx$schedule
    if (is.null(s) || !p$schedule_flag %in% names(s)) {
      stop("Visit schedule has no column ", p$schedule_flag, ".", call. = FALSE)
    }
    a <- a[a$VISITNUM %in% s$VISITNUM[s[[p$schedule_flag]] == "Y"], , drop = FALSE]
  }
  for (f in by) {
    need_field(a, f, p$form_a)
    need_field(b, f, p$form_b)
  }
  ka <- do.call(paste, c(unname(as.list(a[by])), sep = "|"))
  kb <- do.call(paste, c(unname(as.list(b[by])), sep = "|"))
  if (!nrow(a)) ka <- character()
  if (!nrow(b)) kb <- character()
  m <- match(ka, kb)

  na <- is.na(m)
  out <- list(raw_findings(a$SUBJID[na], p$form_a, a$RECID[na], a$VISITNUM[na], p$form_b, ka[na],
                           paste0("No matching ", p$form_b, " record"), depends = ka[na]))
  if (isTRUE(p$both_ways)) {
    nb <- !kb %in% ka
    out[[2]] <- raw_findings(b$SUBJID[nb], p$form_b, b$RECID[nb], b$VISITNUM[nb], p$form_a, kb[nb],
                             paste0("No matching ", p$form_a, " record"), depends = kb[nb])
  }
  if (!is.null(p$date_a) && !is.null(p$date_b)) {
    need_field(a, p$date_a, p$form_a)
    need_field(b, p$date_b, p$form_b)
    tol <- if (is.null(p$tolerance)) 0 else as.numeric(p$tolerance)
    i <- which(!na)
    da <- as_day(a[[p$date_a]][i])
    db <- as_day(b[[p$date_b]][m[i]])
    gap <- abs(as.numeric(da - db))
    bad <- !is.na(gap) & gap > tol
    i <- i[bad]
    val <- paste(format(da[bad]), "vs", format(db[bad]))
    out[[length(out) + 1]] <- raw_findings(
      a$SUBJID[i], p$form_a, a$RECID[i], a$VISITNUM[i], p$date_a, val,
      paste0(p$date_a, " and ", p$form_b, " ", p$date_b, " differ by ", gap[bad], " day(s); tolerance ", tol),
      depends = val)
  }
  do.call(rbind, out)
}

# Helpers ---------------------------------------------------------------------------

raw_findings <- function(subjid, formoid, recid, visitnum, field, value, message, depends = NULL) {
  n <- length(subjid)
  if (!n) return(NULL)
  data.frame(SUBJID = as.character(subjid), FORMOID = rep_len(as.character(formoid), n),
             RECID = rep_len(as.character(recid), n), VISITNUM = rep_len(as.numeric(visitnum), n),
             FIELD = rep_len(as.character(field), n), VALUE = rep_len(as.character(value), n),
             MESSAGE = rep_len(as.character(message), n),
             DEPENDS = if (is.null(depends)) rep_len(as.character(value), n) else rep_len(as.character(depends), n),
             stringsAsFactors = FALSE)
}

form_table <- function(ctx, form) {
  t <- ctx$tables[[form]]
  if (is.null(t)) stop("Form ", form, " is not in the data.", call. = FALSE)
  t
}

form_rows <- function(ctx, form, when = NULL) {
  t <- form_table(ctx, form)
  t[eval_when(t, when, form), , drop = FALSE]
}

need_field <- function(t, field, form) {
  if (!field %in% names(t)) stop("Form ", form, " has no field ", field, ".", call. = FALSE)
}

eval_when <- function(t, when, form) {
  keep <- rep(TRUE, nrow(t))
  for (cond in when) {
    need_field(t, cond$field, form)
    v <- t[[cond$field]]
    val <- unlist(cond$value)
    if (!is.null(val) && !cond$op %in% c("missing", "present")) {
      if (is.numeric(v)) val <- as.numeric(val) else if (inherits(v, "Date")) val <- as.Date(val)
    }
    hit <- switch(cond$op,
      "==" = v == val, "!=" = v != val, "<" = v < val, "<=" = v <= val, ">" = v > val, ">=" = v >= val,
      "in" = v %in% val, "not_in" = !v %in% val, "missing" = is.na(v), "present" = !is.na(v),
      stop("Unknown condition op '", cond$op, "'.", call. = FALSE))
    keep <- keep & !is.na(hit) & hit
  }
  keep
}

# Date, POSIXct or ISO text (full or with time) to Date; partial dates become NA.
as_day <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXct")) return(as.Date(x, tz = "UTC"))
  x <- as.character(x)
  out <- as.Date(rep(NA_character_, length(x)))
  full <- !is.na(x) & grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", x)
  out[full] <- as.Date(substr(x[full], 1, 10))
  out
}

register_library_check_types <- function() {
  register_check_type("parse_log", ct_parse_log, required = "transforms", timing = "load")
  register_check_type("subject_exists", ct_subject_exists, required = "subject_form")
  register_check_type("missing_value", ct_missing_value, required = c("form", "fields"),
                      optional = c("when", "message"))
  register_check_type("date_order", ct_date_order, required = c("form", "field", "ref_form", "ref_field", "op"),
                      optional = "when")
  register_check_type("range", ct_range, required = c("form", "field"), optional = c("min", "max", "when"))
  register_check_type("flag_when", ct_flag_when, required = c("form", "field", "when", "message"))
  register_check_type("reconcile", ct_reconcile, required = c("form_a", "form_b"),
                      optional = c("by", "when_a", "when_b", "schedule_flag", "date_a", "date_b",
                                   "tolerance", "both_ways"))
}

.onLoad <- function(libname, pkgname) {
  register_library_check_types()
  register_library_derivation_types()
}
