# Operational status from the EDC reports (queries, form status) and the
# collected data: query aging, site scorecard, lock readiness and coding
# review. Like visit status, every status here is derived against the review
# date and never stored; report dates after the review date are treated as
# not yet happened.

#' Query status as of a review date
#'
#' Status from the query dates: **Closed** if closed on or before `asof`,
#' **Answered** if answered, otherwise **Open**. Dates after `asof` are set to
#' missing, so the status on an earlier review date does not leak later
#' answers.
#'
#' @param queries Query report table from the snapshot (one row per query).
#' @param asof Review date.
#' @param mapping Optional compiled mapping. When given, `TARGET` holds the
#'   queried record as `FORMOID:RECID`, built from the target form's key
#'   fields; queries whose form key fields are not all in the report get `NA`.
#' @param form,opened,answered,closed Report columns: queried form, and open,
#'   answer and close dates.
#' @return `queries` with `STATUS`, `AGE` (days open, for unresolved queries),
#'   `DAYSTOCLOSE` (for closed queries) and `TARGET` added.
#' @export
query_status <- function(queries, asof, mapping = NULL, form = "QRYFORM", opened = "QRYOPDT",
                         answered = "QRYANSDT", closed = "QRYCLSDT") {
  asof <- as.Date(asof)
  q <- queries
  for (f in c(opened, answered, closed)) {
    d <- as_day(q[[f]])
    d[!is.na(d) & d > asof] <- NA
    q[[f]] <- d
  }
  q$STATUS <- ifelse(!is.na(q[[closed]]), "Closed", ifelse(!is.na(q[[answered]]), "Answered", "Open"))
  q$AGE <- ifelse(q$STATUS == "Closed", NA_real_, as.numeric(asof - q[[opened]]))
  q$DAYSTOCLOSE <- as.numeric(q[[closed]] - q[[opened]])
  q$TARGET <- NA_character_
  if (!is.null(mapping)) {
    for (f in intersect(unique(q[[form]]), names(mapping$forms))) {
      key <- mapping$forms[[f]]$key
      if (!all(key %in% names(q))) next
      k <- q[[form]] == f
      q$TARGET[k] <- paste0(f, ":", do.call(paste, c(unname(as.list(q[k, key, drop = FALSE])), sep = "|")))
    }
  }
  q
}

form_status_levels <- c("Locked", "Frozen", "Signed", "SDV complete", "Entered")

#' Form status as of a review date
#'
#' Status of each EDC form instance from the form status report: the latest
#' of SDV, PI signature, freeze and lock reached on or before `asof`.
#'
#' @param fstat Form status table from the snapshot.
#' @param asof Review date.
#' @param form,sdv,signed,frozen,locked Report columns.
#' @return `fstat` with dates after `asof` set to missing and `STATUS` added
#'   (Locked, Frozen, Signed, SDV complete or Entered).
#' @export
form_status <- function(fstat, asof, form = "FSFORM", sdv = "SDVDT", signed = "SIGNDT", frozen = "FREEZEDT",
                        locked = "LOCKDT") {
  asof <- as.Date(asof)
  x <- fstat
  for (f in c(sdv, signed, frozen, locked)) {
    d <- as_day(x[[f]])
    d[!is.na(d) & d > asof] <- NA
    x[[f]] <- d
  }
  x$STATUS <- ifelse(!is.na(x[[locked]]), "Locked", ifelse(!is.na(x[[frozen]]), "Frozen",
                ifelse(!is.na(x[[signed]]), "Signed", ifelse(!is.na(x[[sdv]]), "SDV complete", "Entered"))))
  x
}

#' Data entry lag
#'
#' Days from the clinical date of each record to its entry in the EDC.
#'
#' @param tables Snapshot tables.
#' @param form Form to measure, usually the visit form.
#' @param date Clinical date field, such as the visit date.
#' @return A data frame: `SUBJID`, `SITEID`, `RECID`, `LAG` (days), records
#'   with both dates only.
#' @export
entry_lag <- function(tables, form, date) {
  t <- tables[[form]]
  if (is.null(t)) stop("Form ", form, " is not in the data.", call. = FALSE)
  lag <- as.numeric(as_day(t$ENTRYDT) - as_day(t[[date]]))
  out <- data.frame(SUBJID = t$SUBJID, SITEID = t$SITEID, RECID = t$RECID, LAG = lag, stringsAsFactors = FALSE)
  out[!is.na(out$LAG), , drop = FALSE]
}

#' Site scorecard
#'
#' One row per site with enrollment, visit completion, entry timeliness,
#' query metrics and open findings, scored so the sites needing the most
#' attention sort first. Each input is optional; its columns are missing
#' when it is not given.
#'
#' Score: 2 if median entry lag is over target, 2 if any visit is overdue,
#' 1 each for a missed visit, fewer than 85% of due visits complete in
#' window, 3 or more unresolved queries, and an unresolved query older than
#' `query_age` days. Status: **Needs attention** (3 or more), **Watch** (1 or
#' 2), **On track** (0).
#'
#' @param adsl ADSL (one row per subject; `EOSSTT` counts discontinuations).
#' @param visits Output of [visit_status()].
#' @param lag Output of [entry_lag()].
#' @param queries Output of [query_status()].
#' @param findings Findings to count, already restricted to the checks that
#'   should count.
#' @param lag_target Entry lag target in days.
#' @param query_age Age in days at which an unresolved query counts against
#'   the site.
#' @return A data frame, one row per site.
#' @export
site_scorecard <- function(adsl, visits = NULL, lag = NULL, queries = NULL, findings = NULL, lag_target = 5,
                           query_age = 30) {
  sites <- sort(unique(adsl$SITEID))
  by_site <- function(x, f, empty = 0) {
    v <- vapply(sites, function(s) { r <- x[x$SITEID == s, , drop = FALSE]; if (nrow(r)) f(r) else empty }, numeric(1))
    unname(v)
  }
  out <- data.frame(SITEID = sites, stringsAsFactors = FALSE)
  out$SUBJECTS <- by_site(adsl, nrow)
  out$DISCONT <- by_site(adsl, function(r) sum(r$EOSSTT %in% "DISCONTINUED"))
  score <- rep(0, length(sites))
  if (!is.null(visits)) {
    due <- visits[!visits$STATUS %in% c("Not yet due", "Discontinued"), , drop = FALSE]
    out$VISDUE <- by_site(due, nrow)
    out$PCTWIN <- by_site(due, function(r) round(100 * mean(r$STATUS == "Complete")), NA_real_)
    out$OVERDUE <- by_site(due, function(r) sum(r$STATUS == "Overdue / not entered"))
    out$MISSED <- by_site(due, function(r) sum(r$STATUS == "Missed"))
    score <- score + 2 * (out$OVERDUE > 0) + (out$MISSED > 0) + (!is.na(out$PCTWIN) & out$PCTWIN < 85)
  }
  if (!is.null(lag)) {
    out$LAGMED <- by_site(lag, function(r) stats::median(r$LAG), NA_real_)
    out$PCTLAG <- by_site(lag, function(r) round(100 * mean(r$LAG <= lag_target)), NA_real_)
    score <- score + 2 * (!is.na(out$LAGMED) & out$LAGMED > lag_target)
  }
  if (!is.null(queries)) {
    open <- queries[queries$STATUS != "Closed", , drop = FALSE]
    out$QUERIES <- by_site(queries, nrow)
    if (!is.null(visits)) {
      entered <- by_site(visits, function(r) sum(r$STATUS %in% c("Complete", "Out of window")))
      out$QRATE <- ifelse(entered > 0, round(10 * out$QUERIES / entered, 1), NA_real_)
    }
    out$OPENQ <- by_site(open, nrow)
    out$OLDESTQ <- by_site(open, function(r) max(r$AGE), NA_real_)
    out$MEDCLOSE <- by_site(queries[queries$STATUS == "Closed", , drop = FALSE],
                            function(r) stats::median(r$DAYSTOCLOSE), NA_real_)
    score <- score + (out$OPENQ >= 3) + (!is.na(out$OLDESTQ) & out$OLDESTQ > query_age)
  }
  if (!is.null(findings)) out$FINDINGS <- by_site(findings, nrow)
  out$SCORE <- score
  out$STATUS <- ifelse(score >= 3, "Needs attention", ifelse(score >= 1, "Watch", "On track"))
  out <- out[order(-out$SCORE, out$SITEID), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Lock readiness
#'
#' One row per subject and form with data: records, form status counts from
#' the EDC form status report, unresolved queries and open findings.
#'
#' * **Locked**: every form instance is locked.
#' * **Ready to lock**: no unresolved queries or findings, and every form
#'   instance in the form status report is verified and signed.
#' * **Not ready**: anything else; `BLOCKERS` says why.
#'
#' A form with no rows in the form status report (vendor data) is ready when
#' it has no queries or findings.
#'
#' @param tables Snapshot tables.
#' @param forms Forms to include.
#' @param fstat Output of [form_status()].
#' @param queries Output of [query_status()].
#' @param findings Findings that block lock, already restricted to the
#'   checks that should count (usually not clinical review flags).
#' @param fstat_form,query_form Columns naming the form in each report.
#' @return A data frame: `SUBJID`, `SITEID`, `FORMOID`, `RECORDS`,
#'   `INSTANCES`, `SDV`, `SIGNED`, `FROZEN`, `LOCKED`, `OPENQ`, `FINDINGS`,
#'   `STATUS`, `BLOCKERS`.
#' @export
lock_readiness <- function(tables, forms, fstat = NULL, queries = NULL, findings = NULL, fstat_form = "FSFORM",
                           query_form = "QRYFORM") {
  rows <- do.call(rbind, lapply(forms, function(f) {
    t <- tables[[f]]
    if (is.null(t) || !nrow(t)) return(NULL)
    n <- table(t$SUBJID)
    data.frame(SUBJID = names(n), SITEID = t$SITEID[match(names(n), t$SUBJID)], FORMOID = f,
               RECORDS = as.integer(n), stringsAsFactors = FALSE)
  }))
  if (is.null(rows)) {
    return(data.frame(SUBJID = character(), SITEID = character(), FORMOID = character(), RECORDS = integer(),
                      INSTANCES = integer(), SDV = integer(), SIGNED = integer(), FROZEN = integer(),
                      LOCKED = integer(), OPENQ = integer(), FINDINGS = integer(), STATUS = character(),
                      BLOCKERS = character(), stringsAsFactors = FALSE))
  }
  key <- paste(rows$SUBJID, rows$FORMOID)
  count <- function(x, form_col, keep = rep(TRUE, nrow(x))) {
    if (is.null(x) || !nrow(x)) return(integer(length(key)))
    n <- table(paste(x$SUBJID, x[[form_col]])[keep])
    v <- as.integer(n[key])
    v[is.na(v)] <- 0L
    v
  }
  rows$INSTANCES <- count(fstat, fstat_form)
  rows$SDV <- count(fstat, fstat_form, !is.na(fstat$SDVDT))
  rows$SIGNED <- count(fstat, fstat_form, !is.na(fstat$SIGNDT))
  rows$FROZEN <- count(fstat, fstat_form, !is.na(fstat$FREEZEDT))
  rows$LOCKED <- count(fstat, fstat_form, !is.na(fstat$LOCKDT))
  rows$OPENQ <- count(queries, query_form, queries$STATUS != "Closed")
  rows$FINDINGS <- count(findings, "FORMOID")

  blockers <- function(i) {
    r <- rows[i, ]
    b <- c(if (r$OPENQ) sprintf("%d unresolved %s", r$OPENQ, if (r$OPENQ == 1) "query" else "queries"),
           if (r$FINDINGS) sprintf("%d open %s", r$FINDINGS, if (r$FINDINGS == 1) "finding" else "findings"),
           if (r$INSTANCES && r$SDV < r$INSTANCES) sprintf("SDV %d of %d", r$SDV, r$INSTANCES),
           if (r$INSTANCES && r$SIGNED < r$INSTANCES) sprintf("signed %d of %d", r$SIGNED, r$INSTANCES))
    paste(b, collapse = "; ")
  }
  rows$BLOCKERS <- vapply(seq_len(nrow(rows)), blockers, character(1))
  rows$STATUS <- ifelse(rows$INSTANCES > 0 & rows$LOCKED == rows$INSTANCES, "Locked",
                        ifelse(rows$BLOCKERS == "", "Ready to lock", "Not ready"))
  rows$BLOCKERS[rows$STATUS == "Locked"] <- ""
  rows <- rows[order(rows$SITEID, rows$SUBJID, match(rows$FORMOID, forms)), , drop = FALSE]
  rownames(rows) <- NULL
  rows[c(setdiff(names(rows), c("STATUS", "BLOCKERS")), "STATUS", "BLOCKERS")]
}

#' Lock readiness by subject
#'
#' Rolls [lock_readiness()] up to subjects. **Locked**: every form in the EDC
#' form status report is locked and the other forms (vendor data) are ready.
#' **Ready to lock**: no form is not ready. **Not ready**: anything else.
#'
#' @param x Output of [lock_readiness()].
#' @return A data frame: `SUBJID`, `SITEID`, `STATUS`, `READY` (forms ready
#'   or locked), `FORMS`, `OPENQ`, `FINDINGS`; not ready first.
#' @export
lock_by_subject <- function(x) {
  out <- data.frame(SUBJID = character(), SITEID = character(), STATUS = character(), READY = integer(),
                    FORMS = integer(), OPENQ = integer(), FINDINGS = integer(), stringsAsFactors = FALSE)
  if (!nrow(x)) return(out)
  out <- do.call(rbind, lapply(split(x, x$SUBJID), function(r) {
    edc <- r$INSTANCES > 0
    ready <- r$STATUS != "Not ready"
    st <- if (any(edc) && all(r$STATUS[edc] == "Locked") && all(ready)) "Locked" else
      if (all(ready)) "Ready to lock" else "Not ready"
    data.frame(SUBJID = r$SUBJID[1], SITEID = r$SITEID[1], STATUS = st, READY = sum(ready), FORMS = nrow(r),
               OPENQ = sum(r$OPENQ), FINDINGS = sum(r$FINDINGS), stringsAsFactors = FALSE)
  }))
  out <- out[order(match(out$STATUS, c("Not ready", "Ready to lock", "Locked")), out$SITEID, out$SUBJID), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Coding review
#'
#' Verbatim terms against their coded terms. Verbatims are compared after
#' trimming, collapsing spaces and upper-casing, so "Itchy skin" and
#' "ITCHY  SKIN" are the same term.
#'
#' Record flags, first match: **Not coded** (no coded term), **Inconsistent**
#' (the same verbatim has more than one coded term in the study), **Older
#' version** (coded with a dictionary version older than the latest in use).
#'
#' @param x Snapshot table of a coded log form (such as AE).
#' @param verbatim,coded Verbatim and coded term fields.
#' @param soc,version,date Optional system organ class, dictionary version and
#'   coding date fields.
#' @param asof Optional review date: a record whose coding date is after it
#'   counts as not coded yet.
#' @return A list: `records` (one row per record with `VERBATIM`, `CODED`,
#'   `SOC`, `VERSION`, `CODEDT` and `FLAG`) and `terms` (one row per verbatim
#'   with `RECORDS`, `SUBJECTS`, `CODED` listing each coded term with its
#'   count, and `CONSISTENT`).
#' @export
coding_review <- function(x, verbatim, coded, soc = NULL, version = NULL, date = NULL, asof = NULL) {
  col <- function(f) if (is.null(f)) rep(NA_character_, nrow(x)) else x[[f]]
  norm <- toupper(gsub("\\s+", " ", trimws(as.character(x[[verbatim]]))))
  cd <- as.character(x[[coded]])
  cd[!is.na(cd) & trimws(cd) == ""] <- NA
  rec <- data.frame(SUBJID = x$SUBJID, SITEID = x$SITEID, RECID = x$RECID, VERBATIM = norm, CODED = cd,
                    SOC = as.character(col(soc)), VERSION = as.character(col(version)),
                    CODEDT = if (is.null(date)) rep(as.Date(NA), nrow(x)) else as_day(x[[date]]),
                    stringsAsFactors = FALSE)
  if (!is.null(asof)) {
    later <- !is.na(rec$CODEDT) & rec$CODEDT > as.Date(asof)
    rec[later, c("CODED", "SOC", "VERSION")] <- NA
    rec$CODEDT[later] <- NA
  }
  n_coded <- tapply(rec$CODED, rec$VERBATIM, function(v) length(unique(stats::na.omit(v))))
  latest <- if (all(is.na(rec$VERSION))) NA_character_ else
    max(numeric_version(stats::na.omit(rec$VERSION[rec$VERSION != ""])))
  older <- !is.na(rec$VERSION) & rec$VERSION != "" & !is.na(latest) &
    vapply(rec$VERSION, function(v) tryCatch(numeric_version(v) < latest, error = function(e) FALSE), logical(1))
  rec$FLAG <- ifelse(is.na(rec$CODED), "Not coded", ifelse(n_coded[rec$VERBATIM] > 1, "Inconsistent",
                ifelse(older, "Older version", "")))
  rec <- rec[order(rec$VERBATIM, rec$SUBJID), , drop = FALSE]
  rownames(rec) <- NULL

  terms <- do.call(rbind, lapply(split(rec, rec$VERBATIM), function(r) {
    tab <- table(r$CODED)
    data.frame(VERBATIM = r$VERBATIM[1], RECORDS = nrow(r), SUBJECTS = length(unique(r$SUBJID)),
               CODED = paste(sprintf("%s (%d)", names(tab), as.integer(tab)), collapse = "; "),
               CONSISTENT = length(tab) <= 1, stringsAsFactors = FALSE)
  }))
  if (is.null(terms)) {
    terms <- data.frame(VERBATIM = character(), RECORDS = integer(), SUBJECTS = integer(), CODED = character(),
                        CONSISTENT = logical(), stringsAsFactors = FALSE)
  }
  terms <- terms[order(terms$CONSISTENT, terms$VERBATIM), , drop = FALSE]
  rownames(terms) <- NULL
  list(records = rec, terms = terms)
}
