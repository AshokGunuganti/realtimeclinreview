# Study database: weekly cuts. Each delivered cut is checked on intake and,
# if it passes, every raw row is kept as delivered (untrimmed text) and tagged
# with its cut. Cuts are never changed or removed, so a mapping fix can be
# rerun on every earlier cut and any review can be reproduced. The tables are
# append-only, like the review store, and live in the same database.

cut_tables <- c("cut", "cut_file", "raw_row")
manifest_file <- "manifest.csv"

#' Open (and if needed create) the study database
#'
#' One database holds the cuts and the review state. Configured by
#' `study_db` in study.yml, with the same fields as `review_store`
#' ([open_review_store()]).
#'
#' @inheritParams open_review_store
#' @return A DBI connection with the cut and review store schemas in place.
#' @export
open_study_db <- function(config, base_dir = ".") {
  con <- db_connect(config, base_dir, "study_db")
  init_review_store(con)
  init_cut_store(con)
  con
}

#' Create the cut tables
#'
#' `cut` has one row per delivered cut (loaded or rejected), `cut_file` one
#' row per file in it with its checksum and row counts, and `raw_row` every
#' row of every loaded file as a JSON array of text values, in the file's
#' column order. A set of files can be loaded only once per study. Safe to
#' run on every start.
#'
#' @param con A DBI connection.
#' @return `con`, invisibly.
#' @export
init_cut_store <- function(con) {
  id <- id_column(con)
  ref <- paste(if (is_postgres(con)) "BIGINT" else "INTEGER", "NOT NULL REFERENCES cut (CUTID)")
  ddl <- c(
    paste0("CREATE TABLE IF NOT EXISTS cut (CUTID ", id, ", STUDYID TEXT NOT NULL, LABEL TEXT NOT NULL, ",
           "EXTRACT_DT TEXT, FINGERPRINT TEXT NOT NULL, N_FILES INTEGER NOT NULL, N_ROWS INTEGER NOT NULL, ",
           "STATUS TEXT NOT NULL, MESSAGE TEXT, SOURCE_DIR TEXT, USERID TEXT NOT NULL, RECEIVED_AT TEXT NOT NULL)"),
    paste0("CREATE TABLE IF NOT EXISTS cut_file (CUTID ", ref, ", FILE TEXT NOT NULL, SOURCE TEXT, ",
           "MD5 TEXT, COLUMNS TEXT, MANIFEST_ROWS INTEGER, ROWS INTEGER, PROBLEM TEXT)"),
    paste0("CREATE TABLE IF NOT EXISTS raw_row (CUTID ", ref, ", FILE TEXT NOT NULL, ROWNUM INTEGER NOT NULL, ",
           "DATA TEXT NOT NULL)"),
    "CREATE INDEX IF NOT EXISTS cut_study ON cut (STUDYID, STATUS)",
    "CREATE UNIQUE INDEX IF NOT EXISTS cut_loaded_once ON cut (STUDYID, FINGERPRINT) WHERE STATUS = 'loaded'",
    "CREATE UNIQUE INDEX IF NOT EXISTS cut_file_key ON cut_file (CUTID, FILE)",
    "CREATE UNIQUE INDEX IF NOT EXISTS raw_row_key ON raw_row (CUTID, FILE, ROWNUM)",
    append_only_ddl(cut_tables, is_postgres(con))
  )
  for (s in ddl) db_exec(con, s)
  invisible(con)
}

#' Take in a delivered cut
#'
#' Checks the cut folder, then stores it. The checks: a `manifest.csv` with
#' `FILE` and `ROWS` columns lists the files; every listed file is present,
#' readable and has the listed number of rows; `STUDYID` in the manifest, if
#' given, is the study's; and every file the mapping reads is in the cut.
#' Files in the folder but not in the manifest are stored and noted. A cut
#' that fails a check is recorded as rejected with its problems and no data.
#' The same files (and manifest) are never recorded twice: a repeat of a
#' loaded cut is a duplicate, and a repeat of a rejected one is reported
#' against the first rejection.
#'
#' @param con Study database connection, from [open_study_db()].
#' @param dir Cut folder as delivered.
#' @param study Study identifier.
#' @param user User or service account taking in the cut.
#' @param mapping Optional mapping, from [compile_spec()]: its source files
#'   must be in the cut.
#' @param label Cut label; defaults to the folder name.
#' @return A list: `cut_id`, `status` (`loaded`, `rejected` or `duplicate`),
#'   `message`, `new` (whether this call recorded the cut) and `files` (one
#'   row per file with its problem, if any).
#' @export
intake_cut <- function(con, dir, study, user, mapping = NULL, label = basename(dir)) {
  need_user(user)
  if (!dir.exists(dir)) stop("Cut folder not found: ", dir, ".", call. = FALSE)
  checked <- check_cut(dir, study, mapping)
  files <- checked$files
  fingerprint <- cut_fingerprint(files, checked$manifest_md5)
  seen <- seen_cut(con, study, fingerprint)
  if (!is.null(seen)) return(c(seen, list(files = files)))
  status <- if (is.null(checked$message)) "loaded" else "rejected"
  now <- utc_now()
  stored <- tryCatch(DBI::dbWithTransaction(con, {
    id <- db_insert_id(con, "cut", list(
      STUDYID = study, LABEL = label, EXTRACT_DT = checked$extract_dt, FINGERPRINT = fingerprint,
      N_FILES = nrow(files), N_ROWS = as.integer(sum(files$ROWS, na.rm = TRUE)), STATUS = status,
      MESSAGE = if (is.null(checked$message)) NA_character_ else checked$message,
      SOURCE_DIR = normalizePath(dir), USERID = user, RECEIVED_AT = now), "CUTID")
    db_append(con, "cut_file", data.frame(CUTID = id, files[c("FILE", "SOURCE", "MD5", "COLUMNS", "MANIFEST_ROWS",
                                                               "ROWS", "PROBLEM")], stringsAsFactors = FALSE))
    if (status == "loaded") {
      for (fl in names(checked$data)) store_raw_rows(con, id, fl, checked$data[[fl]])
    }
    audit(con, study, paste0("cut ", status), paste0("cut:", id), NA_character_,
          as.character(jsonlite::toJSON(list(label = label, files = nrow(files), message = checked$message),
                                        auto_unbox = TRUE)), user, now)
    list(cut_id = id, status = status, message = checked$message, new = TRUE)
  }), error = function(e) {
    # Another pickup stored the same files first; the unique index stopped this one.
    seen <- seen_cut(con, study, fingerprint)
    if (is.null(seen)) stop(e)
    seen
  })
  c(stored, list(files = files))
}

# The earlier cut with this fingerprint, as an intake result, or NULL.
seen_cut <- function(con, study, fingerprint) {
  prev <- db_get(con, paste("SELECT CUTID, STATUS, MESSAGE FROM cut WHERE STUDYID = ? AND FINGERPRINT = ?",
                            "ORDER BY CUTID"), params = list(study, fingerprint))
  if (!nrow(prev)) return(NULL)
  loaded <- prev[prev$STATUS == "loaded", , drop = FALSE]
  if (nrow(loaded)) {
    return(list(cut_id = loaded$CUTID[1], status = "duplicate",
                message = paste0("Same files as cut ", loaded$CUTID[1], "; not stored again."), new = FALSE))
  }
  list(cut_id = prev$CUTID[1], status = "rejected",
       message = paste0("Already rejected as cut ", prev$CUTID[1], ": ", prev$MESSAGE[1]), new = FALSE)
}

#' Pick up new cuts from an inbox folder
#'
#' For a scheduled job: takes in every sub-folder of `inbox`, oldest name
#' first. Folders already recorded are reported again but not stored again.
#' A folder that cannot be taken in at all is reported with status `error`
#' and the rest carry on.
#'
#' @inheritParams intake_cut
#' @param inbox Folder that delivered cuts arrive in, one sub-folder per cut.
#' Write `manifest.csv` last when delivering, so a half-copied folder is not
#' taken in.
#' @return A data frame: `LABEL`, `CUTID`, `STATUS`, `NEW`, `MESSAGE`.
#' @export
pickup_cuts <- function(con, inbox, study, user = "pickup", mapping = NULL) {
  dirs <- sort(list.dirs(inbox, full.names = TRUE, recursive = FALSE))
  rows <- lapply(dirs, function(d) {
    r <- tryCatch(intake_cut(con, d, study, user, mapping), error = function(e) {
      list(cut_id = NA_integer_, status = "error", new = FALSE, message = conditionMessage(e))
    })
    data.frame(LABEL = basename(d), CUTID = r$cut_id, STATUS = r$status, NEW = r$new,
               MESSAGE = if (is.null(r$message)) NA_character_ else r$message, stringsAsFactors = FALSE)
  })
  if (!length(rows)) {
    return(data.frame(LABEL = character(), CUTID = integer(), STATUS = character(), NEW = logical(),
                      MESSAGE = character()))
  }
  do.call(rbind, rows)
}

#' Run the scheduled pickup for a study
#'
#' The entry point for a scheduled job, for example
#' `Rscript -e 'rtcrcore::run_pickup("/srv/studies/derm301")'`. Reads
#' study.yml, opens `study_db`, and takes in new cuts from `inbox` (the secure
#' folder cuts are delivered to; defaults to `extracts`), checked against the
#' study's mapping.
#'
#' @param dir Study folder (holds study.yml).
#' @param user Service account recorded on the cuts.
#' @return The [pickup_cuts()] summary, invisibly; each cut is also reported
#'   as a message.
#' @export
run_pickup <- function(dir, user = "pickup") {
  cfg <- yaml::read_yaml(file.path(dir, "study.yml"))
  if (is.null(cfg$study_db)) stop("study.yml has no study_db to load cuts into.", call. = FALSE)
  path <- function(f) if (grepl("^(/|[A-Za-z]:)", f)) f else file.path(dir, f)
  inbox <- path(if (!is.null(cfg$inbox)) cfg$inbox else if (!is.null(cfg$extracts)) cfg$extracts else "extracts")
  if (!dir.exists(inbox)) stop("Inbox folder not found: ", inbox, ".", call. = FALSE)
  con <- open_study_db(cfg$study_db, dir)
  on.exit(DBI::dbDisconnect(con))
  r <- pickup_cuts(con, inbox, cfg$study_id, user, compile_spec(path(cfg$mapping_spec)))
  for (i in seq_len(nrow(r))) {
    message(r$LABEL[i], ": ", r$STATUS[i], if (!is.na(r$MESSAGE[i])) paste0(" (", r$MESSAGE[i], ")"))
  }
  invisible(r)
}

#' Cuts in the study database
#'
#' @param con Study database connection.
#' @param study Study identifier.
#' @param status Cut statuses to list; `NULL` lists all.
#' @return The `cut` rows, oldest first.
#' @export
list_cuts <- function(con, study, status = "loaded") {
  cuts <- db_get(con, "SELECT * FROM cut WHERE STUDYID = ? ORDER BY CUTID", params = list(study))
  if (is.null(status)) cuts else cuts[cuts$STATUS %in% status, , drop = FALSE]
}

#' Read a stored cut back as source tables
#'
#' Gives the same result as [read_extract()] on the folder the cut came
#' from (values trimmed, blanks as `NA`), so the mapping runs unchanged on
#' any stored cut. With `raw = TRUE`, the values exactly as delivered.
#'
#' @param con Study database connection.
#' @param cut_id Cut ID, from [list_cuts()].
#' @param mapping Optional mapping: read only the files it uses.
#' @param raw Return values untrimmed, as delivered.
#' @return Named list of data frames, one per file, every column text.
#' @export
read_cut <- function(con, cut_id, mapping = NULL, raw = FALSE) {
  cut <- db_get(con, "SELECT STATUS FROM cut WHERE CUTID = ?", params = list(cut_id))
  if (!nrow(cut)) stop("No cut ", cut_id, " in the study database.", call. = FALSE)
  if (cut$STATUS != "loaded") stop("Cut ", cut_id, " was rejected on intake and holds no data.", call. = FALSE)
  files <- db_get(con, "SELECT FILE, COLUMNS FROM cut_file WHERE CUTID = ?", params = list(cut_id))
  files <- files[!is.na(files$COLUMNS), , drop = FALSE]
  wanted <- if (is.null(mapping)) files$FILE else mapping_files(mapping)
  missing <- setdiff(wanted, files$FILE)
  if (length(missing)) stop("Source file missing from cut ", cut_id, ": ", paste(missing, collapse = ", "), ".",
                            call. = FALSE)
  out <- list()
  for (fl in wanted) {
    cols <- jsonlite::fromJSON(files$COLUMNS[files$FILE == fl])
    data <- db_get(con, "SELECT DATA FROM raw_row WHERE CUTID = ? AND FILE = ? ORDER BY ROWNUM",
                   params = list(cut_id, fl))$DATA
    df <- rows_from_json(data, cols)
    out[[fl]] <- if (raw) df else clean_text(df)
  }
  out
}

# Files a mapping reads: form sources and lookup tables.
mapping_files <- function(mapping) {
  unique(c(
    vapply(mapping$forms, `[[`, "", "source"),
    unlist(lapply(mapping$forms, function(f) {
      lapply(f$fields, function(e) if (identical(e$transform, "lookup")) e$args$table)
    }))
  ))
}

# Runs the intake checks on a cut folder and reads its files.
check_cut <- function(dir, study, mapping) {
  man_path <- file.path(dir, manifest_file)
  present <- setdiff(list.files(dir), manifest_file)
  present <- present[!dir.exists(file.path(dir, present))]
  reject <- function(message, files = cut_file_rows(present)) {
    list(files = files, data = list(), extract_dt = NA_character_, manifest_md5 = NA_character_,
         message = message)
  }
  if (!file.exists(man_path)) return(reject(paste0("No ", manifest_file, " in the cut.")))
  man <- tryCatch(clean_text(read_raw_csv(man_path)), error = function(e) e)
  if (inherits(man, "error")) return(reject(paste0(manifest_file, " could not be read: ", conditionMessage(man))))
  if (!all(c("FILE", "ROWS") %in% names(man))) return(reject(paste0(manifest_file, " needs FILE and ROWS columns.")))

  problems <- character()
  if ("STUDYID" %in% names(man) && any(!is.na(man$STUDYID) & man$STUDYID != study)) {
    problems <- c(problems, paste0("Manifest is for study ", paste(unique(man$STUDYID), collapse = ", "),
                                   ", not ", study, "."))
  }
  listed <- man$FILE
  all_files <- union(listed, present)
  files <- cut_file_rows(all_files)
  data <- list()
  for (i in seq_along(all_files)) {
    fl <- all_files[i]
    m <- match(fl, listed)
    files$SOURCE[i] <- if (!is.na(m) && "SOURCE" %in% names(man)) man$SOURCE[m] else NA_character_
    files$MANIFEST_ROWS[i] <- if (is.na(m)) NA_integer_ else suppressWarnings(as.integer(man$ROWS[m]))
    if (!fl %in% present) {
      files$PROBLEM[i] <- "Listed in the manifest but not delivered"
      next
    }
    files$MD5[i] <- unname(tools::md5sum(file.path(dir, fl)))
    warned <- character()
    df <- tryCatch(withCallingHandlers(read_raw_csv(file.path(dir, fl)), warning = function(w) {
      warned <<- c(warned, conditionMessage(w))
      invokeRestart("muffleWarning")
    }), error = function(e) e)
    if (inherits(df, "error")) {
      files$PROBLEM[i] <- paste0("Could not be read: ", conditionMessage(df))
      next
    }
    if (length(warned)) files$NOTE[i] <- paste0("Read with warnings: ", paste(unique(warned), collapse = "; "))
    data[[fl]] <- df
    files$COLUMNS[i] <- as.character(jsonlite::toJSON(names(df)))
    files$ROWS[i] <- nrow(df)
    if (is.na(m)) {
      files$NOTE[i] <- "Delivered but not in the manifest"
    } else if (is.na(files$MANIFEST_ROWS[i]) || files$MANIFEST_ROWS[i] != nrow(df)) {
      files$PROBLEM[i] <- paste0("Manifest says ", man$ROWS[m], " rows; the file has ", nrow(df))
    }
  }
  needed <- if (is.null(mapping)) character() else setdiff(mapping_files(mapping), present)
  if (length(needed)) problems <- c(problems, paste0("Files the mapping reads are missing: ",
                                                     paste(needed, collapse = ", "), "."))
  bad <- !is.na(files$PROBLEM)
  if (any(bad)) problems <- c(problems, paste0(files$FILE[bad], ": ", files$PROBLEM[bad], "."))
  files$PROBLEM[!bad] <- files$NOTE[!bad]
  dt <- if ("EXTRACT_DT" %in% names(man)) unique(stats::na.omit(man$EXTRACT_DT)) else character()
  list(files = files[setdiff(names(files), "NOTE")], data = data,
       extract_dt = if (length(dt) == 1) dt else NA_character_,
       manifest_md5 = unname(tools::md5sum(man_path)),
       message = if (length(problems)) paste(problems, collapse = " ") else NULL)
}

cut_file_rows <- function(files) {
  n <- length(files)
  data.frame(FILE = files, SOURCE = rep(NA_character_, n), MD5 = rep(NA_character_, n),
             COLUMNS = rep(NA_character_, n), MANIFEST_ROWS = rep(NA_integer_, n), ROWS = rep(NA_integer_, n),
             PROBLEM = rep(NA_character_, n), NOTE = rep(NA_character_, n), stringsAsFactors = FALSE)
}

# Reads a delimited file as UTF-8 text, exactly as delivered: no trimming,
# blanks kept as empty strings.
read_raw_csv <- function(path) {
  utils::read.csv(path, colClasses = "character", na.strings = character(0), check.names = FALSE,
                  stringsAsFactors = FALSE, encoding = "UTF-8")
}

# Trims every value and turns blanks into NA, as the mapping expects.
clean_text <- function(df) {
  df[] <- lapply(df, function(v) { v <- trimws(v); v[v == ""] <- NA_character_; v })
  df
}

read_text_csv <- function(path) clean_text(read_raw_csv(path))

# One hash over the manifest and every delivered file's name and checksum.
cut_fingerprint <- function(files, manifest_md5) {
  got <- files[!is.na(files$MD5), , drop = FALSE]
  lines <- c(paste0(manifest_file, ":", manifest_md5), sort(paste(got$FILE, got$MD5, sep = ":")))
  tmp <- tempfile()
  on.exit(unlink(tmp))
  writeLines(lines, tmp)
  unname(tools::md5sum(tmp))
}

store_raw_rows <- function(con, cut_id, file, df) {
  if (!nrow(df)) return(invisible())
  m <- as.matrix(df)
  data <- vapply(seq_len(nrow(m)), function(i) {
    as.character(jsonlite::toJSON(unname(m[i, ]), na = "null"))
  }, "")
  db_append(con, "raw_row", data.frame(CUTID = cut_id, FILE = file, ROWNUM = seq_len(nrow(m)), DATA = data,
                                       stringsAsFactors = FALSE))
}

rows_from_json <- function(data, cols) {
  if (!length(data)) {
    return(as.data.frame(stats::setNames(replicate(length(cols), character(), simplify = FALSE), cols),
                         check.names = FALSE, stringsAsFactors = FALSE))
  }
  m <- jsonlite::fromJSON(paste0("[", paste(data, collapse = ","), "]"), simplifyVector = TRUE)
  m <- matrix(as.character(m), nrow = length(data), ncol = length(cols))
  df <- as.data.frame(m, stringsAsFactors = FALSE)
  names(df) <- cols
  df
}
