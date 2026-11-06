# Data pulls: the transfers the review data arrived in. They are the cuts
# loaded in the study database, else the extract folders, plus any schedule
# study.yml declares (`data_pulls: {start, every_days}` or `{dates: [...]}`)
# for history from before the database. A record came from the first pull on
# or after its date: ENTRYDT for when it first arrived, AVAILDT for its
# current version (for vendor data that is the vendor's transfer date).

data_pulls <- function(study, asof = study$extract_date) {
  cfg <- study$config$data_pulls
  parts <- list(if (length(cfg$dates)) as.Date(unlist(cfg$dates)),
                if (!is.null(cfg$start)) seq(as.Date(cfg$start), as.Date(asof), by = as.numeric(cfg$every_days %|na|% 7)),
                extract_dates(study), as.Date(study$extract_date))
  dates <- do.call(c, Filter(Negate(is.null), parts))                     # c() with a leading NULL drops the class
  dates <- sort(unique(dates[!is.na(dates) & dates <= as.Date(asof)]))
  data.frame(PULL = seq_along(dates), DATE = dates)
}

# Dates of the cuts or extract folders the study can load.
extract_dates <- function(study) {
  cfg <- study$config
  if (!is.null(cfg$study_db)) {
    cuts <- tryCatch({
      con <- rtcrcore::open_study_db(cfg$study_db, study$dir)
      on.exit(DBI::dbDisconnect(con))
      rtcrcore::list_cuts(con, study$id)
    }, error = function(e) NULL)
    return(if (is.null(cuts) || !nrow(cuts)) as.Date(character()) else as.Date(cuts$EXTRACT_DT))
  }
  root <- file.path(study$dir, if (is.null(cfg$extracts)) "extracts" else cfg$extracts)
  as.Date(list.dirs(root, full.names = FALSE, recursive = FALSE), optional = TRUE)
}

# The pull number each date arrived in: the first pull on or after it.
pull_of <- function(dates, pulls) {
  d <- as.Date(dates)
  vapply(seq_along(d), function(i) {
    if (is.na(d[i])) return(NA_integer_)
    hit <- which(pulls$DATE >= d[i])
    if (length(hit)) pulls$PULL[hit[1]] else NA_integer_
  }, 0L)
}

pull_label <- function(n, pulls, short = FALSE) {
  if (short) return(ifelse(is.na(n), "", paste0("P", n)))
  ifelse(is.na(n), "", paste0("Pull ", n, " \u00b7 ", format(pulls$DATE[match(n, pulls$PULL)], "%d %b %Y")))
}

# "Pull 38 - 09 Dec 2026", or "Pull 30 - <date>, updated in pull 38 - <date>"
# when the current version came in a later pull than the record itself.
record_pull_text <- function(entry, avail, pulls) {
  first <- pull_of(entry, pulls)
  now <- pull_of(avail, pulls)
  ifelse(is.na(now), pull_label(first, pulls),
         ifelse(is.na(first) | first == now, pull_label(now, pulls),
                paste0(pull_label(first, pulls), ", updated in p", substring(pull_label(now, pulls), 2))))
}

# The pulls that brought this subject's data, with their study days.
subject_pulls <- function(tables, forms, subj, pulls, trtsdt) {
  dates <- unlist(lapply(forms, function(f) {
    t <- tables[[f]]
    if (is.null(t)) return(NULL)
    t <- t[t$SUBJID == subj, , drop = FALSE]
    as.character(c(as.Date(t$ENTRYDT), as.Date(t$AVAILDT)))
  }))
  used <- sort(unique(stats::na.omit(pull_of(dates, pulls))))
  p <- pulls[pulls$PULL %in% used, , drop = FALSE]
  if (is.na(trtsdt) || !nrow(p)) return(data.frame(PULL = integer(), DATE = as.Date(character()), DAY = numeric()))
  p$DAY <- study_day(p$DATE, trtsdt)
  p
}
