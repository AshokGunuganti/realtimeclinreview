# Review state: the record of sponsor oversight. Review marks, sign-offs, the
# audit trail and the access log are GxP electronic records, so the store is
# insert-only: a changed or cleared mark is a new row, the current mark is the
# latest row, and database triggers reject UPDATE and DELETE on every table.
# Written through DBI: SQLite for demos and single users, Postgres for teams.

review_outcomes <- c("no action", "query raised", "escalated")
review_store_schema_version <- 1L
review_tables <- c("store_info", "review_mark", "signoff", "audit_trail", "access_log")

#' Open (and if needed create) a review store
#'
#' @param config The `review_store` section of study.yml: `driver`
#'   (`sqlite` or `postgres`) plus `path` for SQLite, or connection fields
#'   (`dbname`, `host`, `port`, `user`, `password`) for Postgres. A relative
#'   SQLite path is resolved against `base_dir`.
#' @param base_dir Folder that relative paths are resolved against, usually
#'   the study folder.
#' @return A DBI connection with the schema in place.
#' @export
open_review_store <- function(config, base_dir = ".") {
  driver <- if (is.null(config$driver)) "sqlite" else config$driver
  con <- switch(driver,
    sqlite = {
      if (!requireNamespace("RSQLite", quietly = TRUE)) stop("The sqlite review store needs RSQLite.", call. = FALSE)
      path <- config$path
      if (is.null(path)) stop("review_store needs a path for sqlite.", call. = FALSE)
      if (!grepl("^(/|[A-Za-z]:)", path) && path != ":memory:") path <- file.path(base_dir, path)
      DBI::dbConnect(RSQLite::SQLite(), path)
    },
    postgres = {
      if (!requireNamespace("RPostgres", quietly = TRUE)) stop("The postgres review store needs RPostgres.", call. = FALSE)
      args <- config[intersect(names(config), c("dbname", "host", "port", "user", "password"))]
      do.call(DBI::dbConnect, c(list(getExportedValue("RPostgres", "Postgres")()), args))
    },
    stop("review_store driver must be sqlite or postgres.", call. = FALSE))
  init_review_store(con)
  con
}

#' Create the review store schema
#'
#' Creates the tables and the triggers that make them append-only, if they do
#' not exist. Safe to run on every start.
#'
#' In production also grant the application role only `INSERT` and `SELECT`
#' on these tables, so append-only holds even if a trigger is dropped.
#'
#' @param con A DBI connection.
#' @return `con`, invisibly.
#' @export
init_review_store <- function(con) {
  pg <- is_postgres(con)
  id <- if (pg) "BIGSERIAL PRIMARY KEY" else "INTEGER PRIMARY KEY AUTOINCREMENT"
  ddl <- c(
    "CREATE TABLE IF NOT EXISTS store_info (KEY TEXT NOT NULL, VALUE TEXT NOT NULL, SET_AT TEXT NOT NULL)",
    paste0("CREATE TABLE IF NOT EXISTS review_mark (MARKID ", id, ", STUDYID TEXT NOT NULL, ",
           "TARGET_TYPE TEXT NOT NULL, TARGET_KEY TEXT NOT NULL, SUBJID TEXT, ACTION TEXT NOT NULL, ",
           "OUTCOME TEXT, COMMENT TEXT, DEPHASH TEXT, VERSION REAL, USERID TEXT NOT NULL, MARKED_AT TEXT NOT NULL)"),
    paste0("CREATE TABLE IF NOT EXISTS signoff (SIGNOFFID ", id, ", STUDYID TEXT NOT NULL, ",
           "REVIEW_TYPE TEXT NOT NULL, ASOF TEXT NOT NULL, SCOPE_HASH TEXT NOT NULL, SCOPE_N INTEGER NOT NULL, ",
           "USERID TEXT NOT NULL, FULL_NAME TEXT NOT NULL, MEANING TEXT NOT NULL, AUTH_AT TEXT NOT NULL, ",
           "SIGNED_AT TEXT NOT NULL)"),
    paste0("CREATE TABLE IF NOT EXISTS audit_trail (AUDITID ", id, ", STUDYID TEXT NOT NULL, ",
           "EVENT TEXT NOT NULL, TARGET TEXT NOT NULL, OLD_VALUE TEXT, NEW_VALUE TEXT, USERID TEXT NOT NULL, ",
           "EVENT_AT TEXT NOT NULL)"),
    paste0("CREATE TABLE IF NOT EXISTS access_log (ACCESSID ", id, ", STUDYID TEXT NOT NULL, ",
           "ACTION TEXT NOT NULL, OBJECT TEXT NOT NULL, DETAIL TEXT, USERID TEXT NOT NULL, ACCESSED_AT TEXT NOT NULL)"),
    "CREATE INDEX IF NOT EXISTS review_mark_target ON review_mark (STUDYID, TARGET_TYPE, TARGET_KEY)"
  )
  if (pg) {
    ddl <- c(ddl, paste0(
      "CREATE OR REPLACE FUNCTION rtcr_append_only() RETURNS trigger AS $$ BEGIN ",
      "RAISE EXCEPTION '% is append-only', TG_TABLE_NAME; END; $$ LANGUAGE plpgsql"))
    for (t in review_tables) {
      ddl <- c(ddl, paste0("DROP TRIGGER IF EXISTS ", t, "_append_only ON ", t),
               paste0("CREATE TRIGGER ", t, "_append_only BEFORE UPDATE OR DELETE ON ", t,
                      " FOR EACH ROW EXECUTE FUNCTION rtcr_append_only()"))
    }
  } else {
    for (t in review_tables) {
      for (op in c("UPDATE", "DELETE")) {
        ddl <- c(ddl, paste0("CREATE TRIGGER IF NOT EXISTS ", t, "_no_", tolower(op), " BEFORE ", op, " ON ", t,
                             " BEGIN SELECT RAISE(ABORT, '", t, " is append-only'); END"))
      }
    }
  }
  for (s in ddl) DBI::dbExecute(con, s)
  info <- DBI::dbGetQuery(con, "SELECT VALUE FROM store_info WHERE KEY = 'schema_version'")
  if (!nrow(info)) {
    DBI::dbAppendTable(con, "store_info", data.frame(KEY = "schema_version", VALUE = as.character(review_store_schema_version),
                                                     SET_AT = utc_now(), stringsAsFactors = FALSE))
  } else if (as.integer(info$VALUE[nrow(info)]) != review_store_schema_version) {
    stop("Review store schema version ", info$VALUE[nrow(info)], " needs migrating to ", review_store_schema_version, ".",
         call. = FALSE)
  }
  invisible(con)
}

#' Set or clear a review mark
#'
#' Marks a record (`target_type = "record"`, key `FORMOID:RECID`) or a finding
#' (`target_type = "finding"`, key `FINDINGKEY`). Each call adds a mark row
#' and an audit trail row in one transaction.
#'
#' @param con Review store connection.
#' @param study Study identifier.
#' @param target_type `"record"` or `"finding"`.
#' @param target_key Record or finding key.
#' @param user User ID (from the app's single sign-on).
#' @param outcome `"no action"`, `"query raised"` or `"escalated"`.
#' @param comment Optional comment.
#' @param dephash Hash of the values the mark depends on: the finding's
#'   `DEPHASH`, or the record's hash from [record_hashes()].
#' @param subjid,version Subject and record version, for listings.
#' @param expect_markid Optional ID of the mark the user was looking at. If
#'   someone else has marked the target since, the call fails instead of
#'   silently overwriting their mark.
#' @return The new mark's ID, invisibly.
#' @export
set_mark <- function(con, study, target_type, target_key, user, outcome, comment = NA_character_,
                     dephash = NA_character_, subjid = NA_character_, version = NA_real_, expect_markid = NULL) {
  if (!outcome %in% review_outcomes) {
    stop("outcome must be one of ", paste(review_outcomes, collapse = ", "), ".", call. = FALSE)
  }
  write_mark(con, study, target_type, target_key, user, "set", outcome, comment, dephash, subjid, version, expect_markid)
}

#' @rdname set_mark
#' @export
clear_mark <- function(con, study, target_type, target_key, user, comment = NA_character_, expect_markid = NULL) {
  write_mark(con, study, target_type, target_key, user, "clear", NA_character_, comment, NA_character_,
             NA_character_, NA_real_, expect_markid)
}

write_mark <- function(con, study, target_type, target_key, user, action, outcome, comment, dephash, subjid,
                       version, expect_markid) {
  if (!target_type %in% c("record", "finding")) stop("target_type must be record or finding.", call. = FALSE)
  need_user(user)
  DBI::dbWithTransaction(con, {
    prev <- latest_mark(con, study, target_type, target_key)
    if (!is.null(expect_markid) && !identical(as.numeric(expect_markid), as.numeric(prev$MARKID[1]))) {
      stop("This ", target_type, " was marked by someone else since you opened it; reload and try again.",
           call. = FALSE)
    }
    if (action == "clear" && (!nrow(prev) || prev$ACTION == "clear")) {
      stop("There is no mark to clear.", call. = FALSE)
    }
    now <- utc_now()
    DBI::dbAppendTable(con, "review_mark", data.frame(
      STUDYID = study, TARGET_TYPE = target_type, TARGET_KEY = target_key, SUBJID = subjid, ACTION = action,
      OUTCOME = outcome, COMMENT = comment, DEPHASH = dephash, VERSION = version, USERID = user, MARKED_AT = now,
      stringsAsFactors = FALSE))
    id <- latest_mark(con, study, target_type, target_key)$MARKID
    old <- if (nrow(prev) && prev$ACTION == "set") mark_json(prev$OUTCOME, prev$COMMENT) else NA_character_
    new <- if (action == "set") mark_json(outcome, comment) else NA_character_
    audit(con, study, paste0("mark ", action), paste0(target_type, ":", target_key), old, new, user, now)
    invisible(id)
  })
}

latest_mark <- function(con, study, target_type, target_key) {
  DBI::dbGetQuery(con, paste(
    "SELECT * FROM review_mark WHERE STUDYID = ? AND TARGET_TYPE = ? AND TARGET_KEY = ?",
    "ORDER BY MARKID DESC LIMIT 1"), params = list(study, target_type, target_key))
}

#' Current review marks
#'
#' The latest mark per target, leaving out cleared ones.
#'
#' @inheritParams set_mark
#' @param target_type Optional: `"record"` or `"finding"`.
#' @return A data frame of mark rows.
#' @export
current_marks <- function(con, study, target_type = NULL) {
  q <- paste(
    "SELECT m.* FROM review_mark m JOIN (SELECT TARGET_TYPE, TARGET_KEY, MAX(MARKID) AS MARKID FROM review_mark",
    "WHERE STUDYID = ? GROUP BY TARGET_TYPE, TARGET_KEY) last ON m.MARKID = last.MARKID",
    "WHERE m.ACTION = 'set'", if (!is.null(target_type)) "AND m.TARGET_TYPE = ?", "ORDER BY m.MARKID")
  DBI::dbGetQuery(con, q, params = c(list(study), if (!is.null(target_type)) list(target_type)))
}

#' Attach review marks to findings or records
#'
#' A mark counts as current only while the values it depends on are
#' unchanged: when the item's hash differs from the hash stored with the mark,
#' the item needs review again. Its old mark is still shown.
#'
#' @inheritParams set_mark
#' @param items Data frame with one row per finding (`FINDINGKEY`, `DEPHASH`,
#'   from [run_checks()]) or record (`TARGET_KEY`, `DEPHASH`, from
#'   [record_hashes()]).
#' @return `items` with `MARKID`, `MARK_OUTCOME`, `MARK_COMMENT`, `MARK_USER`,
#'   `MARK_AT` and `REVIEW_STATUS` (`reviewed`, `needs re-review` or
#'   `not reviewed`).
#' @export
apply_marks <- function(con, study, items, target_type = c("finding", "record")) {
  target_type <- match.arg(target_type)
  key <- if (target_type == "finding") items$FINDINGKEY else items$TARGET_KEY
  if (is.null(key)) stop("items need ", if (target_type == "finding") "FINDINGKEY" else "TARGET_KEY", ".", call. = FALSE)
  mk <- current_marks(con, study, target_type)
  m <- match(key, mk$TARGET_KEY)
  items$MARKID <- mk$MARKID[m]
  items$MARK_OUTCOME <- mk$OUTCOME[m]
  items$MARK_COMMENT <- mk$COMMENT[m]
  items$MARK_USER <- mk$USERID[m]
  items$MARK_AT <- mk$MARKED_AT[m]
  same <- !is.na(m) & !is.na(mk$DEPHASH[m]) & mk$DEPHASH[m] == items$DEPHASH
  items$REVIEW_STATUS <- ifelse(is.na(m), "not reviewed", ifelse(same, "reviewed", "needs re-review"))
  items
}

#' Hashes of records for review marks
#'
#' One hash per record of the fields [change_log()] compares, so a mark on a
#' record resets only when collected data changes, not when only bookkeeping
#' (version, update date) moves.
#'
#' @param x A CDM table, usually a snapshot from [as_of()].
#' @return A data frame: `TARGET_KEY` (`FORMOID:RECID`), `SUBJID`, `VERSION`,
#'   `DEPHASH`.
#' @export
record_hashes <- function(x) {
  vars <- setdiff(names(x), not_compared)
  txt <- if (nrow(x)) do.call(paste, c(lapply(x[vars], function(v) {
    v <- if (inherits(v, "POSIXct")) format(v, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC") else as.character(v)
    ifelse(is.na(v), "\x01NA", v)
  }), sep = "\x1f")) else character()
  data.frame(TARGET_KEY = paste0(x$FORMOID, ":", x$RECID), SUBJID = x$SUBJID, VERSION = x$VERSION,
             DEPHASH = vapply(txt, rlang::hash, character(1), USE.NAMES = FALSE), stringsAsFactors = FALSE)
}

#' Sign off a review cycle
#'
#' Records an electronic signature for one review cycle, for example the
#' weekly medical review of an extract: user, printed name, UTC time, the
#' meaning of the signature and a hash of the scope reviewed. The app must
#' re-authenticate the user (single sign-on) immediately before calling, and
#' pass the time of that authentication.
#'
#' @inheritParams set_mark
#' @param review_type Review type, e.g. `"medical"` or `"data management"`.
#' @param asof Review date the cycle covers.
#' @param scope Data frame of what was reviewed, with a key and a hash column
#'   (findings: `FINDINGKEY`, `DEPHASH`; records: `TARGET_KEY`, `DEPHASH`).
#' @param full_name Printed name of the signer.
#' @param meaning Meaning of the signature, e.g. `"Reviewed and approved"`.
#' @param auth_at Time (POSIXct) the user re-authenticated.
#' @param max_auth_age Seconds a re-authentication stays valid.
#' @return The sign-off's scope hash, invisibly.
#' @export
sign_off <- function(con, study, review_type, asof, scope, user, full_name, meaning, auth_at, max_auth_age = 300) {
  need_user(user)
  for (a in c("review_type", "full_name", "meaning")) {
    v <- get(a)
    if (!is.character(v) || length(v) != 1 || !nzchar(trimws(v))) stop(a, " is required.", call. = FALSE)
  }
  if (!inherits(auth_at, "POSIXct")) stop("auth_at must be the POSIXct time of re-authentication.", call. = FALSE)
  age <- as.numeric(difftime(Sys.time(), auth_at, units = "secs"))
  if (is.na(age) || age < 0 || age > max_auth_age) {
    stop("Re-authenticate before signing (last authentication is too old).", call. = FALSE)
  }
  h <- scope_hash(scope)
  now <- utc_now()
  DBI::dbWithTransaction(con, {
    DBI::dbAppendTable(con, "signoff", data.frame(
      STUDYID = study, REVIEW_TYPE = review_type, ASOF = format(as.Date(asof)), SCOPE_HASH = h, SCOPE_N = nrow(scope),
      USERID = user, FULL_NAME = full_name, MEANING = meaning, AUTH_AT = utc_format(auth_at), SIGNED_AT = now,
      stringsAsFactors = FALSE))
    audit(con, study, "sign-off", paste0(review_type, ":", format(as.Date(asof))), NA_character_,
          jsonlite::toJSON(list(scope_hash = h, scope_n = nrow(scope), meaning = meaning, full_name = full_name),
                           auto_unbox = TRUE), user, now)
  })
  invisible(h)
}

#' Hash of a review scope
#'
#' Order-independent hash of the keys and hashes reviewed, so a sign-off can
#' later be checked against the data it covered.
#'
#' @param scope Data frame with a key column (`FINDINGKEY` or `TARGET_KEY`)
#'   and `DEPHASH`.
#' @return A single hash string.
#' @export
scope_hash <- function(scope) {
  key <- if (!is.null(scope$FINDINGKEY)) scope$FINDINGKEY else scope$TARGET_KEY
  if (is.null(key) || is.null(scope$DEPHASH)) stop("scope needs a key column and DEPHASH.", call. = FALSE)
  o <- order(key)
  rlang::hash(paste(key[o], scope$DEPHASH[o], sep = "=", collapse = "\n"))
}

#' Sign-offs
#'
#' @inheritParams set_mark
#' @param review_type,user Optional filters.
#' @return Sign-off rows, newest first.
#' @export
signoffs <- function(con, study, review_type = NULL, user = NULL) {
  q <- "SELECT * FROM signoff WHERE STUDYID = ?"
  p <- list(study)
  if (!is.null(review_type)) { q <- paste(q, "AND REVIEW_TYPE = ?"); p <- c(p, review_type) }
  if (!is.null(user)) { q <- paste(q, "AND USERID = ?"); p <- c(p, user) }
  DBI::dbGetQuery(con, paste(q, "ORDER BY SIGNOFFID DESC"), params = p)
}

#' Default "since" date for the change log
#'
#' The review date of the last sign-off for this review type (by this user,
#' if given), or `fallback` (usually the previous extract) if there is none.
#'
#' @inheritParams signoffs
#' @param fallback Date to use when nothing has been signed off.
#' @return A Date.
#' @export
review_since <- function(con, study, review_type, user = NULL, fallback = NULL) {
  s <- signoffs(con, study, review_type, user)
  if (nrow(s)) return(as.Date(s$ASOF[1]))
  if (is.null(fallback)) stop("No sign-off for ", review_type, " and no fallback date.", call. = FALSE)
  as.Date(fallback)
}

#' Audit trail and access log
#'
#' @inheritParams set_mark
#' @return Rows in the order they were written.
#' @export
audit_trail <- function(con, study) {
  DBI::dbGetQuery(con, "SELECT * FROM audit_trail WHERE STUDYID = ? ORDER BY AUDITID", params = list(study))
}

#' @rdname audit_trail
#' @param action `"view"` or `"export"`.
#' @param object What was viewed or exported, e.g. a screen or file name.
#' @param detail Optional detail, e.g. filters or the data cut.
#' @export
log_access <- function(con, study, user, action, object, detail = NA_character_) {
  need_user(user)
  if (!action %in% c("view", "export")) stop("action must be view or export.", call. = FALSE)
  DBI::dbAppendTable(con, "access_log", data.frame(STUDYID = study, ACTION = action, OBJECT = object, DETAIL = detail,
                                                   USERID = user, ACCESSED_AT = utc_now(), stringsAsFactors = FALSE))
  invisible(NULL)
}

#' @rdname audit_trail
#' @export
access_log <- function(con, study) {
  DBI::dbGetQuery(con, "SELECT * FROM access_log WHERE STUDYID = ? ORDER BY ACCESSID", params = list(study))
}

#' Compare findings between two runs
#'
#' @param old,new Findings from two [run_checks()] runs, e.g. the last
#'   sign-off's review date and today.
#' @return `new` findings plus resolved ones from `old`, with `FINDSTAT`:
#'   `New`, `Changed` (dependent values changed), `Persisting` or `Resolved`.
#' @export
compare_findings <- function(old, new) {
  m <- match(new$FINDINGKEY, old$FINDINGKEY)
  new$FINDSTAT <- ifelse(is.na(m), "New", ifelse(old$DEPHASH[m] == new$DEPHASH, "Persisting", "Changed"))
  gone <- old[!old$FINDINGKEY %in% new$FINDINGKEY, , drop = FALSE]
  gone$FINDSTAT <- rep("Resolved", nrow(gone))
  out <- rbind(new, gone[names(new)])
  rownames(out) <- NULL
  out
}

# Helpers ----------------------------------------------------------------------------

audit <- function(con, study, event, target, old, new, user, at) {
  DBI::dbAppendTable(con, "audit_trail", data.frame(
    STUDYID = study, EVENT = event, TARGET = target, OLD_VALUE = as.character(old), NEW_VALUE = as.character(new),
    USERID = user, EVENT_AT = at, stringsAsFactors = FALSE))
}

mark_json <- function(outcome, comment) {
  x <- list(outcome = outcome)
  if (!is.na(comment)) x$comment <- comment
  as.character(jsonlite::toJSON(x, auto_unbox = TRUE))
}

need_user <- function(user) {
  if (!is.character(user) || length(user) != 1 || is.na(user) || !nzchar(trimws(user))) {
    stop("A user ID is required.", call. = FALSE)
  }
}

is_postgres <- function(con) inherits(con, "PqConnection")

utc_format <- function(t) format(t, "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")

utc_now <- function() utc_format(Sys.time())
