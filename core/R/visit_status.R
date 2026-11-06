visit_status_levels <- c("Complete", "Out of window", "Overdue / not entered", "Missed", "Not yet due", "Discontinued")

#' Visit status as of a review date
#'
#' One row per subject in ADSL and scheduled visit, with a status computed
#' against the review date (never stored). First matching rule:
#'
#' 1. Subject discontinued and the visit's target date is after the end of
#'    study date: **Discontinued** (no visits expected after discontinuation).
#' 2. Visit entered and outside its window: **Out of window**.
#' 3. Visit entered: **Complete**.
#' 4. Target date plus window not yet passed: **Not yet due**.
#' 5. Visit recorded as not done: **Missed**.
#' 6. Otherwise: **Overdue / not entered**.
#'
#' Target dates count from the first visit on the same anchor (see the
#' `analysis_visit` derivation).
#'
#' @param tables Derived snapshot tables.
#' @param adsl ADSL from the derived snapshot (needs the anchor dates, and
#'   `EOSSTT`/`EOSDT` for rule 1).
#' @param schedule Visit schedule.
#' @param asof Review date.
#' @param form Visit form, one row per subject and visit.
#' @param date Visit date field.
#' @param not_done Optional list(field, value) marking a visit not done.
#' @param fallback Optional named list mapping an anchor to a fallback ADSL
#'   variable, e.g. `list(TRTSDT = "PLANDY1")` for subjects not yet dosed.
#' @param nominal,window,anchor,unit_days Schedule columns and unit, as in
#'   `analysis_visit`.
#' @return A data frame: `SUBJID`, `SITEID`, `VISITNUM`, `VISIT`, `TARGDT`,
#'   `VISDT`, `VISOFF`, `STATUS`, `DAYSOVER`, `RECID`.
#' @export
visit_status <- function(tables, adsl, schedule, asof, form, date, not_done = NULL, fallback = NULL,
                         nominal = "NOMWK", window = "WINDOW", anchor = "ANCHOR", unit_days = 7) {
  asof <- as.Date(asof)
  s <- schedule
  first_nom <- tapply(s[[nominal]], s[[anchor]], min)
  s$.OFFSET <- unit_days * (s[[nominal]] - first_nom[s[[anchor]]])
  label <- if ("VISIT" %in% names(s)) as.character(s$VISIT) else as.character(s$VISITNUM)

  g <- expand.grid(i = seq_len(nrow(adsl)), j = seq_len(nrow(s)))
  out <- data.frame(SUBJID = adsl$SUBJID[g$i], SITEID = adsl$SITEID[g$i], VISITNUM = s$VISITNUM[g$j],
                    VISIT = label[g$j], stringsAsFactors = FALSE)
  anch <- as.character(s[[anchor]][g$j])
  base <- rep(as.Date(NA), nrow(out))
  for (a in unique(anch)) {
    k <- anch == a
    d <- adsl[[a]][g$i[k]]
    if (!is.null(fallback[[a]])) d[is.na(d)] <- adsl[[fallback[[a]]]][g$i[k]][is.na(d)]
    base[k] <- d
  }
  out$TARGDT <- base + s$.OFFSET[g$j]
  win <- s[[window]][g$j]

  t <- tables[[form]]
  if (is.null(t)) stop("Form ", form, " is not in the data.", call. = FALSE)
  m <- match(paste(out$SUBJID, out$VISITNUM), paste(t$SUBJID, t$VISITNUM))
  out$RECID <- t$RECID[m]
  out$VISDT <- as_day(t[[date]])[m]
  nd <- if (is.null(not_done)) rep(FALSE, nrow(out)) else t[[not_done$field]][m] %in% unlist(not_done$value)
  known <- !is.na(m)
  entered <- known & !nd
  out$VISOFF <- as.integer(out$VISDT - out$TARGDT)
  oow <- entered & !is.na(out$VISOFF) & win > 0 & abs(out$VISOFF) > win

  eos <- if (!is.null(adsl$EOSSTT)) adsl$EOSSTT[g$i] %in% "DISCONTINUED" else rep(FALSE, nrow(out))
  eosdt <- if (!is.null(adsl$EOSDT)) adsl$EOSDT[g$i] else rep(as.Date(NA), nrow(out))
  disc <- eos & !is.na(out$TARGDT) & !is.na(eosdt) & out$TARGDT > eosdt
  notdue <- is.na(out$TARGDT) | out$TARGDT + win >= asof
  out$STATUS <- ifelse(disc, "Discontinued",
                ifelse(oow, "Out of window",
                ifelse(entered, "Complete",
                ifelse(notdue, "Not yet due",
                ifelse(known & nd, "Missed", "Overdue / not entered")))))
  out$DAYSOVER <- ifelse(out$STATUS == "Overdue / not entered", as.integer(asof - (out$TARGDT + win)), NA_integer_)
  out <- out[order(out$SUBJID, out$VISITNUM), c("SUBJID", "SITEID", "VISITNUM", "VISIT", "TARGDT", "VISDT", "VISOFF",
                                               "STATUS", "DAYSOVER", "RECID")]
  rownames(out) <- NULL
  out
}
