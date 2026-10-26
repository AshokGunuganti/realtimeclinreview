# Library derivation types. Each takes (state, params) and returns state.
# Names follow ADaM conventions. Dates may be Date, POSIXct or ISO text;
# partial text dates count as missing unless imputed first (impute_date).

dv_reference_dates <- function(state, p) {
  base <- dv_table(state, p$subject_form)
  ids <- unique(base$SUBJID[!is.na(base$SUBJID)])
  state$adsl <- data.frame(STUDYID = base$STUDYID[match(ids, base$SUBJID)], SITEID = base$SITEID[match(ids, base$SUBJID)],
                           SUBJID = ids, stringsAsFactors = FALSE)
  for (d in p$dates) {
    pick <- if (is.null(d$pick)) "first" else d$pick
    if (!pick %in% c("first", "last")) stop("reference_dates pick must be first or last.", call. = FALSE)
    cand <- do.call(rbind, lapply(d$from, function(s) {
      t <- form_rows(list(tables = state$tables), s$form, s$when)
      data.frame(SUBJID = t$SUBJID, D = as_day(dv_field(t, s$field, s$form)),
                 SRC = paste0(s$form, ":", t$RECID), stringsAsFactors = FALSE)
    }))
    cand <- cand[!is.na(cand$D), , drop = FALSE]
    cand <- cand[order(cand$SUBJID, if (pick == "last") -as.numeric(cand$D) else as.numeric(cand$D)), , drop = FALSE]
    cand <- cand[!duplicated(cand$SUBJID), , drop = FALSE]
    m <- match(ids, cand$SUBJID)
    state <- add_adsl(state, stats::setNames(list(cand$D[m], cand$SRC[m]), c(d$name, paste0(d$name, "SRC"))))
  }
  state
}

dv_subject_status <- function(state, p) {
  ds <- form_rows(list(tables = state$tables), p$form, p$when)
  code <- dv_field(ds, p$field, p$form)
  dt <- as_day(dv_field(ds, p$date, p$form))
  o <- order(ds$SUBJID, -as.numeric(dt), na.last = TRUE)
  ds <- ds[o, , drop = FALSE]
  code <- code[o]
  dt <- dt[o]
  last <- !duplicated(ds$SUBJID)
  m <- match(state$adsl$SUBJID, ds$SUBJID[last])
  c_last <- code[last][m]
  status <- ifelse(is.na(m), "ONGOING",
                   ifelse(c_last %in% unlist(p$completed), "COMPLETED",
                          ifelse(c_last %in% unlist(p$screen_failure), "SCREEN FAILURE", "DISCONTINUED")))
  add_adsl(state, list(EOSSTT = status, EOSDT = dt[last][m],
                       DCSREAS = ifelse(status == "DISCONTINUED", c_last, NA_character_),
                       EOSSRC = ifelse(is.na(m), NA_character_, paste0(p$form, ":", ds$RECID[last][m]))))
}

dv_study_day <- function(state, p) {
  ref_var <- if (is.null(p$ref)) "TRTSDT" else p$ref
  for (it in p$items) {
    t <- dv_table(state, it$form)
    d <- as_day(dv_field(t, it$field, it$form))
    r <- adsl_ref(state, ref_var, t$SUBJID)
    dy <- as.integer(d - r) + ifelse(d >= r, 1L, 0L)
    state <- add_derived(state, it$form, stats::setNames(list(dy), it$to))
  }
  state
}

dv_period <- function(state, p) {
  before <- if (is.null(p$before)) "Screening" else p$before
  for (it in p$items) {
    t <- dv_table(state, it$form)
    d <- as_day(dv_field(t, it$field, it$form))
    lab <- ifelse(is.na(d), NA_character_, before)
    for (b in p$boundaries) {
      start <- adsl_ref(state, b$from, t$SUBJID) + (if (is.null(b$offset)) 0 else as.numeric(b$offset))
      hit <- !is.na(d) & !is.na(start) & d >= start
      lab[hit] <- b$name
    }
    to <- if (is.null(it$to)) "APERIODC" else it$to
    state <- add_derived(state, it$form, stats::setNames(list(lab), to))
  }
  state
}

dv_baseline <- function(state, p) {
  t <- dv_table(state, p$form)
  v <- as.numeric(dv_field(t, p$value, p$form))
  rule <- p$rule
  sfx <- if (is.null(p$suffix)) "" else p$suffix
  grp <- do.call(paste, c(list(t$SUBJID), unname(as.list(t[unlist(p$by)])), sep = "|"))
  ref <- adsl_ref(state, if (is.null(p$ref)) "TRTSDT" else p$ref, t$SUBJID)
  d <- if (is.null(p$date)) rep(as.Date(NA), nrow(t)) else as_day(dv_field(t, p$date, p$form))

  if (rule == "visit") {
    cand <- !is.na(v) & t$VISITNUM %in% as.numeric(p$visit)
    post <- !is.na(t$VISITNUM) & t$VISITNUM > as.numeric(p$visit)
  } else if (rule %in% c("last_before_ref", "mean_before_ref")) {
    if (is.null(p$date)) stop("Baseline rule ", rule, " needs a date field.", call. = FALSE)
    cand <- !is.na(v) & !is.na(d) & !is.na(ref) & d <= ref
    post <- !is.na(d) & !is.na(ref) & d > ref
  } else {
    stop("Baseline rule must be last_before_ref, mean_before_ref or visit.", call. = FALSE)
  }
  ablfl <- rep(FALSE, nrow(t))
  base <- rep(NA_real_, nrow(t))
  src <- rep(NA_character_, nrow(t))
  for (g in unique(grp[cand])) {
    i <- which(cand & grp == g)
    if (rule == "mean_before_ref") {
      sel <- i[order(as.numeric(d[i]))]          # sources listed in date order
      b <- mean(v[i])
    } else {
      sel <- i[order(-as.numeric(d[i]), -t$VISITNUM[i], t$RECID[i], na.last = TRUE)][1]
      b <- v[sel]
    }
    ablfl[sel] <- TRUE
    base[grp == g] <- b
    src[grp == g] <- paste(paste0(p$form, ":", t$RECID[sel]), collapse = ", ")
  }
  chg <- ifelse(post, v - base, NA_real_)
  pchg <- ifelse(post & !is.na(base) & base != 0, 100 * chg / base, NA_real_)
  cols <- list(ifelse(ablfl, "Y", NA_character_), base, src, yn(post), chg, pchg)
  add_derived(state, p$form, stats::setNames(cols, paste0(c("ABLFL", "BASE", "BASESRC", "APOSTFL", "CHG", "PCHG"), sfx)))
}

dv_analysis_visit <- function(state, p) {
  s <- state$schedule
  if (is.null(s)) stop("analysis_visit needs a visit schedule.", call. = FALSE)
  nominal <- if (is.null(p$nominal)) "NOMWK" else p$nominal
  window <- if (is.null(p$window)) "WINDOW" else p$window
  anchor <- if (is.null(p$anchor)) "ANCHOR" else p$anchor
  unit <- if (is.null(p$unit_days)) 7 else as.numeric(p$unit_days)
  for (col in c("VISITNUM", nominal, window, anchor)) {
    if (!col %in% names(s)) stop("Visit schedule has no column ", col, ".", call. = FALSE)
  }
  # Nominal times count from the first visit on the same anchor.
  first_nom <- tapply(s[[nominal]], s[[anchor]], min)
  s$.OFFSET <- unit * (s[[nominal]] - first_nom[s[[anchor]]])
  label <- if ("VISIT" %in% names(s)) as.character(s$VISIT) else as.character(s$VISITNUM)

  t <- dv_table(state, p$form)
  d <- as_day(dv_field(t, p$field, p$form))
  n <- nrow(t)
  targ <- matrix(NA_real_, n, nrow(s))
  for (j in seq_len(nrow(s))) {
    targ[, j] <- as.numeric(adsl_ref(state, s[[anchor]][j], t$SUBJID)) + s$.OFFSET[j]
  }
  diff <- as.numeric(d) - targ
  j <- apply(abs(diff), 1, function(r) if (all(is.na(r))) NA_integer_ else which.min(r))
  # Optionally keep records collected at a scheduled visit on that visit; only
  # unscheduled records are moved to the nearest nominal visit.
  if (isTRUE(p$keep_scheduled)) {
    sched_j <- match(t$VISITNUM, s$VISITNUM)
    keep <- !is.na(sched_j) & !is.na(d)
    j[keep] <- sched_j[keep]
  }
  dd <- diff[cbind(seq_len(n), ifelse(is.na(j), 1L, j))]
  dd[is.na(j)] <- NA
  inwin <- !is.na(j) & abs(dd) <= s[[window]][ifelse(is.na(j), 1L, j)]
  add_derived(state, p$form, list(AVISITN = s$VISITNUM[j], AVISIT = label[j], ATDIFF = as.integer(dd),
                                  AWINFL = ifelse(is.na(j), NA_character_, yn(inwin))))
}

dv_treatment_emergent <- function(state, p) {
  t <- dv_table(state, p$form)
  d <- as_day(dv_field(t, p$start, p$form))
  ref <- adsl_ref(state, if (is.null(p$ref)) "TRTSDT" else p$ref, t$SUBJID)
  te <- d >= ref
  if (!is.null(p$end_ref)) {
    end <- adsl_ref(state, p$end_ref, t$SUBJID) + (if (is.null(p$lag)) 0 else as.numeric(p$lag))
    te <- te & (is.na(end) | d <= end)
  }
  missing <- if (is.null(p$missing)) "Y" else p$missing
  te <- yn(te)
  te[is.na(ref)] <- "N"                                    # never dosed
  te[!is.na(ref) & is.na(d)] <- if (missing == "NA") NA_character_ else missing
  add_derived(state, p$form, list(TRTEMFL = te))
}

dv_on_treatment <- function(state, p) {
  t <- dv_table(state, p$form)
  d <- as_day(dv_field(t, p$field, p$form))
  ref <- adsl_ref(state, if (is.null(p$ref)) "TRTSDT" else p$ref, t$SUBJID)
  end <- if (is.null(p$end_ref)) rep(as.Date(NA), nrow(t)) else adsl_ref(state, p$end_ref, t$SUBJID)
  end <- end + (if (is.null(p$days)) 0 else as.numeric(p$days))
  on <- !is.na(d) & !is.na(ref) & d >= ref & (is.na(end) | d <= end)
  on[is.na(d)] <- NA
  add_derived(state, p$form, list(ONTRTFL = yn(on)))
}

dv_compliance <- function(state, p) {
  t <- dv_table(state, p$form)
  missed <- as.numeric(dv_field(t, p$missed, p$form))
  if (!is.null(p$prescribed)) {
    presc <- as.numeric(dv_field(t, p$prescribed, p$form))
  } else if (!is.null(p$per_day) && !is.null(p$start) && !is.null(p$end)) {
    days <- as.numeric(as_day(dv_field(t, p$end, p$form)) - as_day(dv_field(t, p$start, p$form))) + 1
    presc <- as.numeric(p$per_day) * days
  } else {
    stop("compliance needs prescribed, or per_day with start and end.", call. = FALSE)
  }
  appl <- presc - missed
  pct <- ifelse(!is.na(presc) & presc > 0, round(100 * appl / presc, if (is.null(p$digits)) 0 else as.numeric(p$digits)),
                NA_real_)
  add_derived(state, p$form, list(DOSAPPL = appl, COMPPCT = pct))
}

dv_pk_timing <- function(state, p) {
  t <- dv_table(state, p$form)
  v <- as.numeric(dv_field(t, p$value, p$form))
  cols <- list()
  if (!is.null(p$blq_flag)) {
    blq <- dv_field(t, p$blq_flag, p$form) %in% TRUE
    lloq <- if (is.null(p$lloq)) rep(NA_real_, nrow(t)) else as.numeric(dv_field(t, p$lloq, p$form))
    rule <- if (is.null(p$blq_rule)) "missing" else p$blq_rule
    v[blq] <- switch(rule, missing = NA_real_, zero = 0, half_lloq = lloq[blq] / 2, lloq = lloq[blq],
                     stop("blq_rule must be missing, zero, half_lloq or lloq.", call. = FALSE))
  }
  cols$AVAL <- v
  if (!is.null(p$time) && !is.null(p$last_dose)) {
    st <- as_time(dv_field(t, p$time, p$form))
    dose <- dv_table(state, p$last_dose$form)
    dt <- as_time(dv_field(dose, p$last_dose$field, p$last_dose$form))
    tsld <- rep(NA_real_, nrow(t))
    src <- rep(NA_character_, nrow(t))
    for (k in which(!is.na(st))) {
      i <- which(dose$SUBJID == t$SUBJID[k] & !is.na(dt) & dt <= st[k])
      if (length(i)) {
        i <- i[which.max(as.numeric(dt[i]))]
        tsld[k] <- as.numeric(difftime(st[k], dt[i], units = "hours"))
        src[k] <- paste0(p$last_dose$form, ":", dose$RECID[i])
      }
    }
    cols$TSLD <- round(tsld, 2)
    cols$TSLDSRC <- src
    if (!is.null(p$nominal_hours)) {
      win <- if (is.null(p$window_hours)) 0 else as.numeric(p$window_hours)
      cols$PKWINFL <- yn(abs(tsld - as.numeric(p$nominal_hours)) <= win)
    }
  }
  add_derived(state, p$form, cols)
}

dv_impute_date <- function(state, p) {
  t <- dv_table(state, p$form)
  x <- as.character(dv_field(t, p$field, p$form))
  rule <- if (is.null(p$rule)) "first" else p$rule
  out <- as_day(x)
  flag <- rep(NA_character_, length(x))
  ym <- is.na(out) & !is.na(x) & grepl("^[0-9]{4}-[0-9]{2}$", x)
  y <- is.na(out) & !is.na(x) & grepl("^[0-9]{4}$", x)
  first_ym <- as.Date(sprintf("%s-01", x[ym]))
  first_y <- as.Date(sprintf("%s-01-01", x[y]))
  if (rule == "first") {
    out[ym] <- first_ym
    out[y] <- first_y
  } else if (rule == "last") {
    out[ym] <- seq_month_end(first_ym)
    out[y] <- as.Date(sprintf("%s-12-31", x[y]))
  } else {
    stop("impute_date rule must be first or last.", call. = FALSE)
  }
  flag[ym] <- "D"
  flag[y] <- "M"
  # Start-date convention: a partial date in the same month (or year) as the
  # reference date is imputed no earlier than the reference date.
  if (rule == "first" && !is.null(p$not_before)) {
    ref <- adsl_ref(state, p$not_before, t$SUBJID)
    same <- (ym & format(ref, "%Y-%m") == x) | (y & format(ref, "%Y") == x)
    bump <- !is.na(same) & same & out < ref
    out[bump] <- ref[bump]
  }
  to <- if (is.null(p$to)) paste0(p$field, "_IMP") else p$to
  add_derived(state, p$form, stats::setNames(list(out, flag), c(to, paste0(to, "F"))))
}

dv_worst <- function(state, p) {
  t <- dv_table(state, p$form)
  keep <- eval_when(t, p$when, p$form)
  v <- dv_field(t, p$field, p$form)
  score <- if (!is.null(p$order)) match(v, unlist(p$order)) else as.numeric(v)
  if (identical(p$direction, "min")) score <- -score
  grp <- do.call(paste, c(list(t$SUBJID), unname(as.list(t[unlist(p$by)])), sep = "|"))
  ok <- keep & !is.na(score)
  o <- order(grp, -score, t$VISITNUM, t$RECID)
  o <- o[ok[o]]
  pick <- o[!duplicated(grp[o])]
  flag <- rep(NA_character_, nrow(t))
  flag[pick] <- "Y"
  add_derived(state, p$form, stats::setNames(list(flag), if (is.null(p$flag)) "WORSTFL" else p$flag))
}

dv_prior_concomitant <- function(state, p) {
  t <- dv_table(state, p$form)
  st <- as_day(dv_field(t, p$start, p$form))
  en <- as_day(dv_field(t, p$end, p$form))
  ongoing <- if (is.null(p$ongoing)) rep(FALSE, nrow(t)) else dv_field(t, p$ongoing, p$form) %in% c("Y", "Yes", TRUE)
  ref <- adsl_ref(state, if (is.null(p$ref)) "TRTSDT" else p$ref, t$SUBJID)
  end_ref <- if (is.null(p$end_ref)) rep(as.Date(NA), nrow(t)) else adsl_ref(state, p$end_ref, t$SUBJID)
  # Missing start is treated as before first dose; missing end without "ongoing" as unknown.
  prior <- is.na(st) | st < ref
  conmed <- (is.na(en) & ongoing) | (!is.na(en) & en >= ref)
  conmed[is.na(en) & !ongoing] <- NA
  conmed <- conmed & (is.na(end_ref) | is.na(st) | st <= end_ref)
  prior[is.na(ref)] <- NA
  conmed[is.na(ref)] <- NA
  add_derived(state, p$form, list(PREFL = yn(prior), CONCOMFL = yn(conmed)))
}

# Helpers ---------------------------------------------------------------------------

seq_month_end <- function(first) {
  if (!length(first)) return(first)
  nxt <- as.Date(vapply(first, function(f) as.numeric(seq(f, by = "month", length.out = 2)[2]), numeric(1)))
  nxt - 1
}

# POSIXct, Date or ISO text (date or datetime) to POSIXct UTC.
as_time <- function(x) {
  if (inherits(x, "POSIXct")) return(x)
  if (inherits(x, "Date")) return(as.POSIXct(format(x), tz = "UTC"))
  x <- as.character(x)
  out <- as.POSIXct(rep(NA_real_, length(x)), tz = "UTC")
  for (f in c("%Y-%m-%dT%H:%M:%S", "%Y-%m-%dT%H:%M", "%Y-%m-%d")) {
    todo <- is.na(out) & !is.na(x)
    out[todo] <- as.POSIXct(x[todo], format = f, tz = "UTC")
  }
  out
}

register_library_derivation_types <- function() {
  r <- register_derivation_type
  r("reference_dates", dv_reference_dates, required = c("subject_form", "dates"))
  r("subject_status", dv_subject_status, required = c("form", "field", "date"),
    optional = c("completed", "screen_failure", "when"))
  r("study_day", dv_study_day, required = "items", optional = "ref")
  r("period", dv_period, required = c("boundaries", "items"), optional = "before")
  r("baseline", dv_baseline, required = c("form", "value", "rule"),
    optional = c("date", "visit", "ref", "by", "suffix"))
  r("analysis_visit", dv_analysis_visit, required = c("form", "field"),
    optional = c("nominal", "window", "anchor", "unit_days", "keep_scheduled"))
  r("treatment_emergent", dv_treatment_emergent, required = c("form", "start"),
    optional = c("ref", "end_ref", "lag", "missing"))
  r("on_treatment", dv_on_treatment, required = c("form", "field"), optional = c("ref", "end_ref", "days"))
  r("compliance", dv_compliance, required = c("form", "missed"),
    optional = c("prescribed", "per_day", "start", "end", "digits"))
  r("pk_timing", dv_pk_timing, required = c("form", "value"),
    optional = c("blq_flag", "blq_rule", "lloq", "time", "last_dose", "nominal_hours", "window_hours"))
  r("impute_date", dv_impute_date, required = c("form", "field"), optional = c("to", "rule", "not_before"))
  r("worst", dv_worst, required = c("form", "field"), optional = c("direction", "order", "when", "by", "flag"))
  r("prior_concomitant", dv_prior_concomitant, required = c("form", "start", "end"),
    optional = c("ongoing", "ref", "end_ref"))
}
