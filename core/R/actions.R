# Study team, meetings and action items: the follow-up side of the weekly
# review. Like the rest of the review store every table is insert-only: a
# change adds a row, the current state is the latest row per key, and every
# change is also written to the audit trail.
#
# Action owners come from the study team list and need not have a login. An
# owner with a login closes their own items; an item whose owner has no
# login can be closed on their behalf, but only in a meeting and with a note.

action_tables <- c("team_member", "meeting", "meeting_attendee", "action_item", "action_update")
action_statuses <- c("open", "in progress", "closed")
action_targets <- c("record", "finding", "subject", "site", "study")
meeting_statuses <- c("open", "closed")

#' Create the team, meeting and action tables
#'
#' Called by [init_review_store()]; safe to run on every start.
#'
#' @param con A DBI connection.
#' @return `con`, invisibly.
#' @export
init_action_store <- function(con) {
  id <- id_column(con)
  ddl <- c(
    paste0("CREATE TABLE IF NOT EXISTS team_member (ROWID_ ", id, ", STUDYID TEXT NOT NULL, MEMBERID TEXT NOT NULL, ",
           "NAME TEXT NOT NULL, ROLE TEXT, EMAIL TEXT, LOGIN TEXT, ACTIVE INTEGER NOT NULL, ",
           "USERID TEXT NOT NULL, CHANGED_AT TEXT NOT NULL)"),
    paste0("CREATE TABLE IF NOT EXISTS meeting (ROWID_ ", id, ", STUDYID TEXT NOT NULL, MEETINGID INTEGER NOT NULL, ",
           "MEETING_DT TEXT NOT NULL, CUTID INTEGER, STATUS TEXT NOT NULL, MINUTES TEXT, ",
           "USERID TEXT NOT NULL, CHANGED_AT TEXT NOT NULL)"),
    paste0("CREATE TABLE IF NOT EXISTS meeting_attendee (ROWID_ ", id, ", STUDYID TEXT NOT NULL, ",
           "MEETINGID INTEGER NOT NULL, MEMBERID TEXT NOT NULL, PRESENT INTEGER NOT NULL, ",
           "USERID TEXT NOT NULL, CHANGED_AT TEXT NOT NULL)"),
    paste0("CREATE TABLE IF NOT EXISTS action_item (ROWID_ ", id, ", STUDYID TEXT NOT NULL, ACTIONID TEXT NOT NULL, ",
           "TITLE TEXT NOT NULL, TARGET_TYPE TEXT NOT NULL, TARGET_KEY TEXT, AREA TEXT, MEETINGID INTEGER, ",
           "USERID TEXT NOT NULL, CREATED_AT TEXT NOT NULL)"),
    paste0("CREATE TABLE IF NOT EXISTS action_update (ROWID_ ", id, ", STUDYID TEXT NOT NULL, ACTIONID TEXT NOT NULL, ",
           "STATUS TEXT NOT NULL, OWNER TEXT NOT NULL, DUE_DT TEXT NOT NULL, NOTE TEXT, MEETINGID INTEGER, ",
           "ON_BEHALF INTEGER NOT NULL, USERID TEXT NOT NULL, UPDATED_AT TEXT NOT NULL)"),
    "CREATE UNIQUE INDEX IF NOT EXISTS action_item_key ON action_item (STUDYID, ACTIONID)",
    "CREATE INDEX IF NOT EXISTS action_update_key ON action_update (STUDYID, ACTIONID)",
    "CREATE INDEX IF NOT EXISTS team_member_key ON team_member (STUDYID, MEMBERID)",
    "CREATE INDEX IF NOT EXISTS meeting_key ON meeting (STUDYID, MEETINGID)",
    append_only_ddl(action_tables, is_postgres(con))
  )
  for (s in ddl) db_exec(con, s)
  invisible(con)
}

# Study team ------------------------------------------------------------------

#' Add or change a study team member
#'
#' Team members are the people actions can be assigned to. A member without
#' a `login` can own actions but not sign in; their items are closed on their
#' behalf in a meeting. Setting `active = FALSE` keeps the member's history
#' but stops new actions being assigned to them.
#'
#' @inheritParams set_mark
#' @param member_id Short stable ID, e.g. `"dm"` or `"cra-113"`.
#' @param name Name shown in the app.
#' @param role Study role, e.g. `"Data manager"`.
#' @param email,login Optional email and app login (user ID).
#' @param active Whether new actions can be assigned to the member.
#' @return `member_id`, invisibly.
#' @export
set_team_member <- function(con, study, member_id, name, user, role = NA_character_, email = NA_character_,
                            login = NA_character_, active = TRUE) {
  need_user(user)
  need_text(member_id, "member_id")
  need_text(name, "name")
  prev <- current_member(con, study, member_id)
  now <- utc_now()
  row <- data.frame(STUDYID = study, MEMBERID = member_id, NAME = name, ROLE = role, EMAIL = email,
                    LOGIN = login, ACTIVE = as.integer(isTRUE(active)), USERID = user, CHANGED_AT = now,
                    stringsAsFactors = FALSE)
  fields <- c("NAME", "ROLE", "EMAIL", "LOGIN", "ACTIVE")
  DBI::dbWithTransaction(con, {
    db_append(con, "team_member", row)
    audit(con, study, if (nrow(prev)) "team member changed" else "team member added", paste0("member:", member_id),
          if (nrow(prev)) row_json(prev[fields]) else NA_character_, row_json(row[fields]), user, now)
  })
  invisible(member_id)
}

#' Study team list
#'
#' @inheritParams set_mark
#' @param active_only Only members new actions can be assigned to.
#' @return One row per member (their latest details), by name.
#' @export
team_members <- function(con, study, active_only = TRUE) {
  rows <- db_get(con, "SELECT * FROM team_member WHERE STUDYID = ? ORDER BY ROWID_", params = list(study))
  cur <- latest_by(rows, "MEMBERID")
  if (active_only) cur <- cur[cur$ACTIVE == 1, , drop = FALSE]
  cur <- cur[order(cur$NAME), setdiff(names(cur), "ROWID_"), drop = FALSE]
  rownames(cur) <- NULL
  cur
}

#' Add team members listed in study.yml
#'
#' Adds each member of `team` (a list of `id`, `name`, `role`, `email`,
#' `login`) that is not yet in the team list; existing members are left as
#' they are, so changes made in the app are kept.
#'
#' @inheritParams set_mark
#' @param team The `team` section of study.yml.
#' @return IDs of the members added, invisibly.
#' @export
seed_team <- function(con, study, team, user) {
  have <- team_members(con, study, active_only = FALSE)$MEMBERID
  opt <- function(x) if (is.null(x)) NA_character_ else x
  added <- character()
  for (m in team) {
    if (is.null(m$id) || m$id %in% have) next
    set_team_member(con, study, m$id, m$name, user, role = opt(m$role), email = opt(m$email), login = opt(m$login))
    added <- c(added, m$id)
  }
  invisible(added)
}

current_member <- function(con, study, member_id) {
  db_get(con, "SELECT * FROM team_member WHERE STUDYID = ? AND MEMBERID = ? ORDER BY ROWID_ DESC LIMIT 1",
         params = list(study, member_id))
}

# Meetings --------------------------------------------------------------------

#' Open a weekly review meeting
#'
#' @inheritParams set_mark
#' @param meeting_date Date of the meeting.
#' @param cut_id The cut the meeting reviews, from [list_cuts()], if any.
#' @return The new meeting's number (1, 2, ... per study), invisibly.
#' @export
open_meeting <- function(con, study, meeting_date, user, cut_id = NA_integer_) {
  need_user(user)
  meeting_date <- need_date(meeting_date, "meeting_date")
  DBI::dbWithTransaction(con, {
    db_lock(con, study)
    n <- db_get(con, "SELECT MAX(MEETINGID) AS n FROM meeting WHERE STUDYID = ?", params = list(study))$n
    id <- if (is.na(n)) 1L else as.integer(n) + 1L
    now <- utc_now()
    db_append(con, "meeting", data.frame(STUDYID = study, MEETINGID = id, MEETING_DT = format(meeting_date),
                                         CUTID = as.integer(cut_id), STATUS = "open", MINUTES = NA_character_,
                                         USERID = user, CHANGED_AT = now, stringsAsFactors = FALSE))
    audit(con, study, "meeting opened", paste0("meeting:", id), NA_character_,
          row_json(list(date = format(meeting_date), cut = cut_id)), user, now)
    invisible(id)
  })
}

#' Save a meeting's minutes, or close or reopen it
#'
#' @inheritParams set_mark
#' @param meeting_id Meeting number, from [open_meeting()].
#' @param minutes New minutes text; `NULL` keeps the current minutes.
#' @param status `"open"` or `"closed"`; `NULL` keeps the current status.
#' @return `meeting_id`, invisibly.
#' @export
save_meeting <- function(con, study, meeting_id, user, minutes = NULL, status = NULL) {
  need_user(user)
  if (!is.null(status) && !status %in% meeting_statuses) {
    stop("status must be one of ", paste(meeting_statuses, collapse = ", "), ".", call. = FALSE)
  }
  DBI::dbWithTransaction(con, {
    db_lock(con, study)
    prev <- current_meeting(con, study, meeting_id)
    new <- prev
    if (!is.null(minutes)) new$MINUTES <- minutes
    if (!is.null(status)) new$STATUS <- status
    new$USERID <- user
    new$CHANGED_AT <- utc_now()
    db_append(con, "meeting", new[setdiff(names(new), "ROWID_")])
    audit(con, study, "meeting saved", paste0("meeting:", meeting_id), row_json(prev[c("STATUS", "MINUTES")]),
          row_json(new[c("STATUS", "MINUTES")]), user, new$CHANGED_AT)
  })
  invisible(meeting_id)
}

#' Record who attended a meeting
#'
#' @inheritParams save_meeting
#' @param member_id Team member, from [team_members()].
#' @param present Whether they attended.
#' @return `member_id`, invisibly.
#' @export
set_attendance <- function(con, study, meeting_id, member_id, user, present = TRUE) {
  need_user(user)
  current_meeting(con, study, meeting_id)
  need_member(con, study, member_id, active = FALSE)
  now <- utc_now()
  DBI::dbWithTransaction(con, {
    db_append(con, "meeting_attendee", data.frame(STUDYID = study, MEETINGID = as.integer(meeting_id),
                                                  MEMBERID = member_id, PRESENT = as.integer(isTRUE(present)),
                                                  USERID = user, CHANGED_AT = now, stringsAsFactors = FALSE))
    audit(con, study, "attendance", paste0("meeting:", meeting_id), NA_character_,
          row_json(list(member = member_id, present = isTRUE(present))), user, now)
  })
  invisible(member_id)
}

#' Meetings and their attendees
#'
#' @inheritParams set_mark
#' @return `meetings()`: one row per meeting (its latest state), newest
#'   first. `meeting_attendees()`: the members marked present, with names.
#' @export
meetings <- function(con, study) {
  rows <- db_get(con, "SELECT * FROM meeting WHERE STUDYID = ? ORDER BY ROWID_", params = list(study))
  cur <- latest_by(rows, "MEETINGID")
  cur <- cur[order(-cur$MEETINGID), setdiff(names(cur), "ROWID_"), drop = FALSE]
  rownames(cur) <- NULL
  cur
}

#' @rdname meetings
#' @inheritParams save_meeting
#' @export
meeting_attendees <- function(con, study, meeting_id) {
  rows <- db_get(con, "SELECT * FROM meeting_attendee WHERE STUDYID = ? AND MEETINGID = ? ORDER BY ROWID_",
                 params = list(study, as.integer(meeting_id)))
  cur <- latest_by(rows, "MEMBERID")
  cur <- cur[cur$PRESENT == 1, , drop = FALSE]
  team <- team_members(con, study, active_only = FALSE)
  m <- match(cur$MEMBERID, team$MEMBERID)
  out <- data.frame(MEMBERID = cur$MEMBERID, NAME = team$NAME[m], ROLE = team$ROLE[m], stringsAsFactors = FALSE)
  out <- out[order(out$NAME), , drop = FALSE]
  rownames(out) <- NULL
  out
}

current_meeting <- function(con, study, meeting_id) {
  m <- db_get(con, "SELECT * FROM meeting WHERE STUDYID = ? AND MEETINGID = ? ORDER BY ROWID_ DESC LIMIT 1",
              params = list(study, as.integer(meeting_id)))
  if (!nrow(m)) stop("No meeting ", meeting_id, " for ", study, ".", call. = FALSE)
  m
}

# Action items ----------------------------------------------------------------

#' Raise an action item
#'
#' @inheritParams set_mark
#' @param title What needs doing.
#' @param owner Team member who owns it, from [team_members()].
#' @param due Due date.
#' @param target_type What it is attached to: `"record"`, `"finding"`,
#'   `"subject"`, `"site"` or `"study"`.
#' @param target_key The record key, finding key, subject ID or site ID;
#'   not needed for `"study"`.
#' @param area Review area, e.g. `"Dosing"`.
#' @param meeting_id Meeting it was raised in, if any.
#' @param note Optional first note.
#' @return The new action's ID (`A-001`, `A-002`, ...), invisibly.
#' @export
raise_action <- function(con, study, title, owner, due, user, target_type = "study", target_key = NA_character_,
                         area = NA_character_, meeting_id = NA_integer_, note = NA_character_) {
  need_user(user)
  need_text(title, "title")
  due <- need_date(due, "due")
  if (!target_type %in% action_targets) {
    stop("target_type must be one of ", paste(action_targets, collapse = ", "), ".", call. = FALSE)
  }
  if (target_type != "study" && (is.na(target_key) || !nzchar(trimws(target_key)))) {
    stop("An action on a ", target_type, " needs its key.", call. = FALSE)
  }
  need_member(con, study, owner, active = TRUE)
  if (!is.na(meeting_id)) current_meeting(con, study, meeting_id)
  DBI::dbWithTransaction(con, {
    db_lock(con, study)
    ids <- db_get(con, "SELECT ACTIONID FROM action_item WHERE STUDYID = ?", params = list(study))$ACTIONID
    n <- suppressWarnings(max(c(0L, as.integer(sub("^A-", "", ids))), na.rm = TRUE))
    id <- sprintf("A-%03d", n + 1L)
    now <- utc_now()
    db_append(con, "action_item", data.frame(STUDYID = study, ACTIONID = id, TITLE = title, TARGET_TYPE = target_type,
                                             TARGET_KEY = if (target_type == "study") NA_character_ else target_key,
                                             AREA = area, MEETINGID = as.integer(meeting_id), USERID = user,
                                             CREATED_AT = now, stringsAsFactors = FALSE))
    db_append(con, "action_update", data.frame(STUDYID = study, ACTIONID = id, STATUS = "open", OWNER = owner,
                                               DUE_DT = format(due), NOTE = note, MEETINGID = as.integer(meeting_id),
                                               ON_BEHALF = 0L, USERID = user, UPDATED_AT = now,
                                               stringsAsFactors = FALSE))
    audit(con, study, "action raised", paste0("action:", id), NA_character_,
          row_json(list(title = title, owner = owner, due = format(due), target = target_type, key = target_key)),
          user, now)
    invisible(id)
  })
}

#' Update an action item
#'
#' Adds a note, and optionally changes the status, owner or due date. Anyone
#' may add a note or mark an action in progress. Only the owner closes an
#' action: the user must be the owner's login. When the owner has no login,
#' someone else may close it on their behalf (`on_behalf = TRUE`), but only in
#' an open meeting and with a note. Changing the owner or due date, or
#' reopening a closed action, needs the owner, or an open meeting and a note.
#'
#' @inheritParams raise_action
#' @param action_id Action ID, from [raise_action()].
#' @param note What happened; required unless the status, owner or due date
#'   changes.
#' @param status `"open"`, `"in progress"` or `"closed"`; `NULL` keeps it.
#' @param owner,due New owner or due date; `NULL` keeps them.
#' @param on_behalf Closing for an owner who has no login; only with
#'   `status = "closed"`.
#' @return `action_id`, invisibly.
#' @export
update_action <- function(con, study, action_id, user, note = NA_character_, status = NULL, owner = NULL,
                          due = NULL, meeting_id = NA_integer_, on_behalf = FALSE) {
  need_user(user)
  if (!is.null(status) && !status %in% action_statuses) {
    stop("status must be one of ", paste(action_statuses, collapse = ", "), ".", call. = FALSE)
  }
  new_due_in <- if (is.null(due)) NULL else format(need_date(due, "due"))
  has_note <- !is.na(note) && nzchar(trimws(note))
  DBI::dbWithTransaction(con, {
    db_lock(con, study)
    prev <- current_action(con, study, action_id)
    new_status <- if (is.null(status)) prev$STATUS else status
    new_owner <- if (is.null(owner)) prev$OWNER else owner
    new_due <- if (is.null(new_due_in)) prev$DUE_DT else new_due_in
    if (new_owner != prev$OWNER) need_member(con, study, new_owner, active = TRUE)
    if (!is.na(meeting_id)) need_open_meeting(con, study, meeting_id)
    if (new_status == prev$STATUS && new_owner == prev$OWNER && new_due == prev$DUE_DT && !has_note) {
      stop("Nothing to update: add a note or change the status, owner or due date.", call. = FALSE)
    }
    closing <- new_status == "closed" && prev$STATUS != "closed"
    if (isTRUE(on_behalf) && !closing) stop("on_behalf applies only when closing an action.", call. = FALSE)
    if (closing) need_close_right(con, study, prev$OWNER, user, has_note, meeting_id, on_behalf)
    reopening <- prev$STATUS == "closed" && new_status != "closed"
    if (new_owner != prev$OWNER || new_due != prev$DUE_DT || reopening) {
      need_owner_or_meeting(con, study, prev$OWNER, user, has_note, meeting_id)
    }
    now <- utc_now()
    db_append(con, "action_update", data.frame(STUDYID = study, ACTIONID = action_id, STATUS = new_status,
                                               OWNER = new_owner, DUE_DT = new_due, NOTE = note,
                                               MEETINGID = as.integer(meeting_id),
                                               ON_BEHALF = as.integer(isTRUE(on_behalf)), USERID = user,
                                               UPDATED_AT = now, stringsAsFactors = FALSE))
    audit(con, study, "action updated", paste0("action:", action_id), row_json(prev[c("STATUS", "OWNER", "DUE_DT")]),
          row_json(list(STATUS = new_status, OWNER = new_owner, DUE_DT = new_due, NOTE = note,
                        ON_BEHALF = isTRUE(on_behalf))), user, now)
  })
  invisible(action_id)
}

#' The action log
#'
#' @inheritParams set_mark
#' @param asof Date that decides which open items are overdue.
#' @return One row per action with its current state: `ACTIONID`, `TITLE`,
#'   `STATUS`, `OWNER`, `OWNER_NAME`, `DUE_DT`, `OVERDUE`, `TARGET_TYPE`,
#'   `TARGET_KEY`, `AREA`, `RAISED_MEETING`, `RAISED_AT`, `LAST_NOTE`,
#'   `UPDATED_AT` and `CLOSED_AT`; overdue first, then open by due date,
#'   then closed.
#' @export
action_log <- function(con, study, asof = Sys.Date()) {
  items <- db_get(con, "SELECT * FROM action_item WHERE STUDYID = ? ORDER BY ROWID_", params = list(study))
  ups <- db_get(con, "SELECT * FROM action_update WHERE STUDYID = ? ORDER BY ROWID_", params = list(study))
  cur <- latest_by(ups, "ACTIONID")[match(items$ACTIONID, latest_by(ups, "ACTIONID")$ACTIONID), , drop = FALSE]
  noted <- latest_by(ups[!is.na(ups$NOTE) & nzchar(ups$NOTE), , drop = FALSE], "ACTIONID")
  closed <- latest_by(ups[ups$STATUS == "closed", , drop = FALSE], "ACTIONID")
  team <- team_members(con, study, active_only = FALSE)
  due <- as.Date(cur$DUE_DT)
  is_closed <- cur$STATUS == "closed"
  out <- data.frame(
    ACTIONID = items$ACTIONID, TITLE = items$TITLE, STATUS = cur$STATUS, OWNER = cur$OWNER,
    OWNER_NAME = team$NAME[match(cur$OWNER, team$MEMBERID)], DUE_DT = due,
    OVERDUE = !is_closed & due < as.Date(asof), TARGET_TYPE = items$TARGET_TYPE, TARGET_KEY = items$TARGET_KEY,
    AREA = items$AREA, RAISED_MEETING = items$MEETINGID, RAISED_AT = items$CREATED_AT,
    LAST_NOTE = noted$NOTE[match(items$ACTIONID, noted$ACTIONID)], UPDATED_AT = cur$UPDATED_AT,
    CLOSED_AT = ifelse(is_closed, closed$UPDATED_AT[match(items$ACTIONID, closed$ACTIONID)], NA_character_),
    stringsAsFactors = FALSE)
  out <- out[order(!out$OVERDUE, out$STATUS == "closed", out$DUE_DT, out$ACTIONID), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' An action item's history
#'
#' @inheritParams update_action
#' @return Every update, oldest first, with the owner's name.
#' @export
action_history <- function(con, study, action_id) {
  current_action(con, study, action_id)
  ups <- db_get(con, "SELECT * FROM action_update WHERE STUDYID = ? AND ACTIONID = ? ORDER BY ROWID_",
                params = list(study, action_id))
  team <- team_members(con, study, active_only = FALSE)
  ups$OWNER_NAME <- team$NAME[match(ups$OWNER, team$MEMBERID)]
  ups[c("UPDATED_AT", "USERID", "STATUS", "OWNER", "OWNER_NAME", "DUE_DT", "NOTE", "MEETINGID", "ON_BEHALF")]
}

current_action <- function(con, study, action_id) {
  a <- db_get(con, "SELECT * FROM action_update WHERE STUDYID = ? AND ACTIONID = ? ORDER BY ROWID_ DESC LIMIT 1",
              params = list(study, action_id))
  if (!nrow(a)) stop("No action ", action_id, " for ", study, ".", call. = FALSE)
  a
}

# Only the owner closes; an owner without a login is closed for in a meeting, with a note.
need_close_right <- function(con, study, owner, user, has_note, meeting_id, on_behalf) {
  m <- current_member(con, study, owner)
  login <- if (nrow(m)) m$LOGIN else NA_character_
  if (!is.na(login) && nzchar(login)) {
    if (!identical(login, user)) stop("Only the owner (", m$NAME, ") can close this action.", call. = FALSE)
    if (isTRUE(on_behalf)) stop("The owner has a login and closes the action themselves.", call. = FALSE)
    return(invisible())
  }
  if (!isTRUE(on_behalf) || is.na(meeting_id) || !has_note) {
    stop("The owner has no login: close it on their behalf in a meeting, with a note.", call. = FALSE)
  }
  invisible()
}

# Owner or reschedule changes and reopening: the owner, or an open meeting with a note.
need_owner_or_meeting <- function(con, study, owner, user, has_note, meeting_id) {
  m <- current_member(con, study, owner)
  if (nrow(m) && !is.na(m$LOGIN) && identical(m$LOGIN, user)) return(invisible())
  if (is.na(meeting_id) || !has_note) {
    stop("Only the owner can reassign, reschedule or reopen this action; otherwise do it in a meeting, ",
         "with a note.", call. = FALSE)
  }
  invisible()
}

need_open_meeting <- function(con, study, meeting_id) {
  m <- current_meeting(con, study, meeting_id)
  if (m$STATUS != "open") stop("Meeting ", meeting_id, " is closed.", call. = FALSE)
  invisible(m)
}

need_member <- function(con, study, member_id, active) {
  m <- current_member(con, study, member_id)
  if (!nrow(m)) stop("No team member ", member_id, "; add them to the study team first.", call. = FALSE)
  if (active && m$ACTIVE != 1) stop(m$NAME, " is no longer active on the study team.", call. = FALSE)
  invisible(m)
}

need_text <- function(x, what) {
  if (!is.character(x) || length(x) != 1 || is.na(x) || !nzchar(trimws(x))) stop(what, " is required.", call. = FALSE)
}

need_date <- function(x, what) {
  d <- tryCatch(as.Date(x), error = function(e) as.Date(NA))
  if (length(d) != 1 || is.na(d)) stop(what, " must be a date.", call. = FALSE)
  d
}

# Latest row per key, from rows ordered oldest first.
latest_by <- function(rows, key) {
  if (!nrow(rows)) return(rows)
  rows[!duplicated(rows[[key]], fromLast = TRUE), , drop = FALSE]
}

row_json <- function(x) as.character(jsonlite::toJSON(as.list(x), auto_unbox = TRUE, na = "null"))
