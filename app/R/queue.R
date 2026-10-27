# Review queue: what a reviewer has to look at in the review window (since,
# review date], subject by subject. An item is a record of a clinical form that
# is new, changed or deleted in the window, or an open clinical-tier finding.
# A subject is reviewed when every one of its items has a current review mark.

# Records of clinical forms that are New, Changed or Deleted in the window.
# The hash is of the record's collected fields, the same hash record marks
# use everywhere (for a deleted record, its last live version).
record_changes <- function(study, since, asof, site = "All sites") {
  out <- lapply(clinical_forms(study), function(f) {
    cl <- rtcrcore::change_log(study$tables[[f]], since, asof)
    cl <- cl[cl$RECSTAT != "Unchanged", , drop = FALSE]
    if (!identical(site, "All sites")) cl <- cl[cl$SITEID %in% site, , drop = FALSE]
    if (!nrow(cl)) return(NULL)
    h <- rtcrcore::record_hashes(cl[setdiff(names(cl), c("RECSTAT", "PREVVER", "CHGVARS"))])
    data.frame(FORMOID = f, SUBJID = cl$SUBJID, SITEID = cl$SITEID, RECID = cl$RECID, RECSTAT = cl$RECSTAT,
               CHGVARS = cl$CHGVARS, VERSION = cl$VERSION, KEY = h$TARGET_KEY, DEPHASH = h$DEPHASH,
               stringsAsFactors = FALSE)
  })
  d <- do.call(rbind, out)
  if (is.null(d)) {
    d <- data.frame(FORMOID = character(), SUBJID = character(), SITEID = character(), RECID = character(),
                    RECSTAT = character(), CHGVARS = character(), VERSION = numeric(), KEY = character(),
                    DEPHASH = character(), stringsAsFactors = FALSE)
  }
  d
}

# Every item in the window with its review status, one row per item. With
# `keep` (keys of flags new or changed in the window), flags outside the
# window are left out unless their data changed after a review mark.
queue_items <- function(changes, findings, con, study_id, keep = NULL) {
  rec <- data.frame(TYPE = rep("record", nrow(changes)), SUBJID = changes$SUBJID, SITEID = changes$SITEID,
                    FORMOID = changes$FORMOID, KEY = changes$KEY, DEPHASH = changes$DEPHASH,
                    VERSION = changes$VERSION, RECSTAT = changes$RECSTAT, CHECKID = rep(NA_character_, nrow(changes)),
                    stringsAsFactors = FALSE)
  fnd <- data.frame(TYPE = rep("finding", nrow(findings)), SUBJID = findings$SUBJID, SITEID = findings$SITEID,
                    FORMOID = findings$FORMOID, KEY = findings$FINDINGKEY, DEPHASH = findings$DEPHASH,
                    VERSION = findings$VERSION, RECSTAT = rep(NA_character_, nrow(findings)), CHECKID = findings$CHECKID,
                    stringsAsFactors = FALSE)
  if (nrow(rec)) {
    m <- rtcrcore::apply_marks(con, study_id, data.frame(TARGET_KEY = rec$KEY, DEPHASH = rec$DEPHASH), "record")
    rec <- cbind(rec, m[c("REVIEW_STATUS", "MARK_USER", "MARK_AT")])
  }
  if (nrow(fnd)) {
    m <- rtcrcore::apply_marks(con, study_id, data.frame(FINDINGKEY = fnd$KEY, DEPHASH = fnd$DEPHASH), "finding")
    fnd <- cbind(fnd, m[c("REVIEW_STATUS", "MARK_USER", "MARK_AT")])
    if (!is.null(keep)) fnd <- fnd[fnd$KEY %in% keep | fnd$REVIEW_STATUS == "needs re-review", , drop = FALSE]
  }
  for (c in c("REVIEW_STATUS", "MARK_USER", "MARK_AT")) {
    if (!nrow(rec)) rec[[c]] <- character()
    if (!nrow(fnd)) fnd[[c]] <- character()
  }
  rbind(rec, fnd)
}

# One row per subject in the queue: what changed, open clinical flags and
# review status. Subjects still to review come first, flagged subjects and
# subjects with changes to priority forms before the rest.
subject_queue <- function(items, adsl, priority = character()) {
  cols <- c("SUBJID", "SITEID", "CHANGES", "FLAGS", "ITEMS", "REVIEWED", "STATUS", "LASTBY", "LASTAT")
  if (!nrow(items)) return(stats::setNames(data.frame(matrix(character(), 0, length(cols))), cols))
  by <- split(items, items$SUBJID)
  out <- do.call(rbind, lapply(by, function(x) {
    rec <- x[x$TYPE == "record", , drop = FALSE]
    lv <- unique(rec$FORMOID)
    lv <- lv[order(is.na(match(lv, priority)), match(lv, priority))]
    forms <- if (nrow(rec)) table(factor(rec$FORMOID, levels = lv)) else integer()
    chg <- if (length(forms)) paste(names(forms), forms, sep = " ", collapse = "; ") else ""
    flags <- unique(x$CHECKID[x$TYPE == "finding"])
    done <- x$REVIEW_STATUS == "reviewed"
    last <- order(x$MARK_AT, decreasing = TRUE, na.last = TRUE)[1]
    rank <- match(rec$FORMOID, priority)
    data.frame(SUBJID = x$SUBJID[1], SITEID = x$SITEID[1], CHANGES = chg, FLAGS = paste(flags, collapse = "; "),
               ITEMS = nrow(x), REVIEWED = sum(done), STATUS = if (all(done)) "Reviewed" else "To review",
               LASTBY = if (is.na(x$MARK_AT[last])) "" else x$MARK_USER[last],
               LASTAT = if (is.na(x$MARK_AT[last])) "" else x$MARK_AT[last],
               .PRIO = if (any(!is.na(rank))) min(rank, na.rm = TRUE) else Inf, .FLAG = length(flags) > 0,
               stringsAsFactors = FALSE)
  }))
  out <- out[order(out$STATUS != "To review", !out$.FLAG, out$.PRIO, out$SUBJID), , drop = FALSE]
  rownames(out) <- NULL
  out[cols]
}

# Next subject still to review after `current`, in queue order, wrapping round;
# NULL when none is left.
next_in_queue <- function(queue, current = NULL, step = 1, to_review = TRUE) {
  ids <- queue$SUBJID
  pool <- setdiff(if (to_review) ids[queue$STATUS == "To review"] else ids, current)
  if (!length(pool)) return(NULL)
  i <- if (is.null(current)) NA else match(current, ids)
  if (is.na(i)) return(if (step > 0) pool[1] else pool[length(pool)])
  after <- ids[seq_along(ids) > i]
  before <- ids[seq_along(ids) < i]
  walk <- if (step > 0) c(after, before) else c(rev(before), rev(after))
  walk[walk %in% pool][1]
}

# Mark every item of a subject not yet reviewed. Returns the number marked.
mark_subject <- function(ctx, items, subject, outcome = "no action", comment = NA_character_) {
  x <- items[items$SUBJID == subject & items$REVIEW_STATUS != "reviewed", , drop = FALSE]
  for (i in seq_len(nrow(x))) {
    rtcrcore::set_mark(ctx$con, ctx$study$id, x$TYPE[i], x$KEY[i], ctx$user, outcome, comment = comment,
                       dephash = x$DEPHASH[i], subjid = x$SUBJID[i], version = x$VERSION[i])
  }
  nrow(x)
}

# Study day of a date from first dose, no day 0.
study_day <- function(date, ref) {
  d <- as.numeric(as.Date(date) - as.Date(ref))
  ifelse(is.na(d), NA_real_, ifelse(d >= 0, d + 1, d))
}

# Keys of clinical flags that are new or changed in the window.
new_flag_keys <- function(data_since, data) {
  cmp <- rtcrcore::compare_findings(clinical_findings(data_since), clinical_findings(data))
  cmp$FINDINGKEY[cmp$FINDSTAT %in% c("New", "Changed")]
}

# Clinical-tier findings: the flags a medical reviewer looks at.
clinical_findings <- function(data, tiers = "clinical") {
  keep <- data$summary$ID[data$summary$TIER %in% unlist(tiers)]
  data$findings[data$findings$CHECKID %in% keep, , drop = FALSE]
}
