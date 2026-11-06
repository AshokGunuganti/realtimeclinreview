skip_if_not_installed("RSQLite")

store <- function(env = parent.frame()) {
  con <- open_review_store(list(driver = "sqlite", path = ":memory:"))
  withr::defer(DBI::dbDisconnect(con), envir = env)
  con
}

# A team: a data manager with a login and a site CRA without one.
with_team <- function(con) {
  set_team_member(con, "S1", "dm", "Dana Data", "admin", role = "Data manager", login = "dana")
  set_team_member(con, "S1", "cra113", "Site 113 CRA", "admin", role = "CRA")
  con
}

test_that("team members are added, changed and retired without losing history", {
  con <- with_team(store())
  set_team_member(con, "S1", "cra113", "Sam Site", "admin", role = "CRA")
  set_team_member(con, "S1", "dm", "Dana Data", "admin", role = "Data manager", login = "dana", active = FALSE)

  all <- team_members(con, "S1", active_only = FALSE)
  expect_equal(all$MEMBERID, c("dm", "cra113"))
  expect_equal(all$NAME[all$MEMBERID == "cra113"], "Sam Site")
  expect_equal(team_members(con, "S1")$MEMBERID, "cra113")
  expect_equal(sum(audit_trail(con, "S1")$EVENT == "team member changed"), 2)
  expect_equal(nrow(team_members(con, "OTHER")), 0)
})

test_that("seeding from study.yml adds only members not yet on the team", {
  con <- with_team(store())
  set_team_member(con, "S1", "dm", "Dana D.", "admin", login = "dana")
  added <- seed_team(con, "S1", list(list(id = "dm", name = "Dana Data"), list(id = "mm", name = "Max Medic",
                                                                           role = "Medical monitor")), "setup")
  expect_equal(added, "mm")
  team <- team_members(con, "S1")
  expect_equal(team$NAME[team$MEMBERID == "dm"], "Dana D.")
  expect_equal(team$ROLE[team$MEMBERID == "mm"], "Medical monitor")
})

test_that("actions get sequential IDs and show in the log with their owner", {
  con <- with_team(store())
  a1 <- raise_action(con, "S1", "Confirm coding date", "dm", "2026-12-31", "dana",
                     target_type = "record", target_key = "AE:101-002|2", area = "Data quality")
  a2 <- raise_action(con, "S1", "Retrain site 113", "cra113", "2027-01-20", "dana", target_type = "site",
                     target_key = "113")
  expect_equal(c(a1, a2), c("A-001", "A-002"))

  log <- action_log(con, "S1", asof = "2027-01-07")
  expect_equal(log$ACTIONID, c("A-001", "A-002"))
  expect_equal(log$OVERDUE, c(TRUE, FALSE))
  expect_equal(log$OWNER_NAME, c("Dana Data", "Site 113 CRA"))
  expect_equal(log$STATUS, c("open", "open"))
  expect_equal(log$TARGET_KEY, c("AE:101-002|2", "113"))
})

test_that("raising an action checks its owner, date and target", {
  con <- with_team(store())
  expect_error(raise_action(con, "S1", "X", "nobody", "2027-01-01", "dana"), "add them to the study team")
  expect_error(raise_action(con, "S1", "X", "dm", "soon", "dana"), "due must be a date")
  expect_error(raise_action(con, "S1", "X", "dm", "2027-01-01", "dana", target_type = "site"), "needs its key")
  expect_error(raise_action(con, "S1", "X", "dm", "2027-01-01", "dana", target_type = "planet"), "target_type")
  expect_error(raise_action(con, "S1", "", "dm", "2027-01-01", "dana"), "title is required")
  set_team_member(con, "S1", "dm", "Dana Data", "admin", active = FALSE)
  expect_error(raise_action(con, "S1", "X", "dm", "2027-01-01", "dana"), "no longer active")
})

test_that("updates keep a history; the log shows the latest state and note", {
  con <- with_team(store())
  id <- raise_action(con, "S1", "Chase queries", "dm", "2027-01-10", "dana", note = "Raised in meeting")
  update_action(con, "S1", id, "dana", note = "Called the site", status = "in progress")
  update_action(con, "S1", id, "dana", due = "2027-01-17")
  update_action(con, "S1", id, "dana", owner = "cra113", note = "Site CRA to follow up")

  h <- action_history(con, "S1", id)
  expect_equal(h$STATUS, c("open", "in progress", "in progress", "in progress"))
  expect_equal(h$DUE_DT, c("2027-01-10", "2027-01-10", "2027-01-17", "2027-01-17"))
  log <- action_log(con, "S1", asof = "2027-01-12")
  expect_equal(log$OWNER, "cra113")
  expect_equal(log$LAST_NOTE, "Site CRA to follow up")
  expect_false(log$OVERDUE)
  expect_error(update_action(con, "S1", id, "dana"), "Nothing to update")
  expect_error(update_action(con, "S1", "A-999", "dana", note = "x"), "No action A-999")
})

test_that("only the owner closes an action they own with a login", {
  con <- with_team(store())
  id <- raise_action(con, "S1", "Confirm coding", "dm", "2027-01-10", "max")
  expect_error(update_action(con, "S1", id, "max", status = "closed", note = "done"), "Only the owner \\(Dana Data\\)")
  update_action(con, "S1", id, "dana", status = "closed", note = "Coded")
  log <- action_log(con, "S1", asof = "2027-02-01")
  expect_equal(log$STATUS, "closed")
  expect_false(log$OVERDUE)
  expect_false(is.na(log$CLOSED_AT))
})

test_that("an owner without a login is closed for only in a meeting, with a note", {
  con <- with_team(store())
  id <- raise_action(con, "S1", "Retrain site 113", "cra113", "2027-01-10", "dana")
  m <- open_meeting(con, "S1", "2027-01-07", "dana")
  expect_error(update_action(con, "S1", id, "dana", status = "closed", note = "Done"), "in a meeting")
  expect_error(update_action(con, "S1", id, "dana", note = "Spoke to CRA", meeting_id = m, on_behalf = TRUE),
               "only when closing")
  expect_error(update_action(con, "S1", id, "dana", status = "closed", meeting_id = m, on_behalf = TRUE),
               "with a note")
  update_action(con, "S1", id, "dana", status = "closed", note = "CRA confirmed training", meeting_id = m,
                on_behalf = TRUE)
  h <- action_history(con, "S1", id)
  expect_equal(h$ON_BEHALF[nrow(h)], 1)
  expect_equal(h$MEETINGID[nrow(h)], m)
})

test_that("only the owner reassigns, reschedules or reopens, unless in a meeting with a note", {
  con <- with_team(store())
  id <- raise_action(con, "S1", "Confirm coding", "dm", "2027-01-10", "dana")
  set_team_member(con, "S1", "max", "Max Medic", "admin", login = "max")
  # The bypass the rule closes: take the action over, then close it.
  expect_error(update_action(con, "S1", id, "max", owner = "max"), "Only the owner can reassign")
  expect_error(update_action(con, "S1", id, "max", due = "2027-02-01", note = "later"), "Only the owner can reassign")
  m <- open_meeting(con, "S1", "2027-01-07", "dana")
  update_action(con, "S1", id, "max", owner = "max", note = "Agreed in the meeting", meeting_id = m)
  expect_equal(action_log(con, "S1")$OWNER, "max")

  update_action(con, "S1", id, "max", status = "closed", note = "Done")
  expect_error(update_action(con, "S1", id, "dana", status = "open", note = "Not done"), "reopen")
  update_action(con, "S1", id, "max", status = "open", note = "Found another case")
  expect_equal(action_log(con, "S1")$STATUS, "open")
  expect_equal(update_action(con, "S1", id, "dana", note = "Anyone may add a note"), id)
})

test_that("a closed meeting cannot be used to act for an owner", {
  con <- with_team(store())
  id <- raise_action(con, "S1", "Retrain site 113", "cra113", "2027-01-10", "dana")
  m <- open_meeting(con, "S1", "2027-01-07", "dana")
  save_meeting(con, "S1", m, "dana", status = "closed")
  expect_error(update_action(con, "S1", id, "dana", status = "closed", note = "Done", meeting_id = m,
                             on_behalf = TRUE), "Meeting 1 is closed")
  expect_equal(action_log(con, "S1")$STATUS, "open")
})

test_that("meetings are numbered, keep minutes and attendees, and close", {
  con <- with_team(store())
  m1 <- open_meeting(con, "S1", "2026-12-31", "dana")
  m2 <- open_meeting(con, "S1", "2027-01-07", "dana", cut_id = 12L)
  set_attendance(con, "S1", m2, "dm", "dana")
  set_attendance(con, "S1", m2, "cra113", "dana")
  set_attendance(con, "S1", m2, "cra113", "dana", present = FALSE)
  save_meeting(con, "S1", m2, "dana", minutes = "Reviewed open actions.")
  save_meeting(con, "S1", m2, "dana", status = "closed")

  mt <- meetings(con, "S1")
  expect_equal(mt$MEETINGID, c(m2, m1))
  expect_equal(mt$STATUS, c("closed", "open"))
  expect_equal(mt$MINUTES[1], "Reviewed open actions.")
  expect_equal(mt$CUTID[1], 12)
  expect_equal(meeting_attendees(con, "S1", m2)$NAME, "Dana Data")
  expect_error(save_meeting(con, "S1", 99, "dana", minutes = "x"), "No meeting 99")
  expect_error(set_attendance(con, "S1", m2, "ghost", "dana"), "add them to the study team")
})

test_that("action and meeting tables are append-only", {
  con <- with_team(store())
  raise_action(con, "S1", "X", "dm", "2027-01-01", "dana")
  open_meeting(con, "S1", "2027-01-07", "dana")
  for (t in c("team_member", "meeting", "action_item", "action_update")) {
    expect_error(DBI::dbExecute(con, paste("UPDATE", t, "SET STUDYID = 'X'")), "append-only")
    expect_error(DBI::dbExecute(con, paste("DELETE FROM", t)), "append-only")
  }
})

test_that("an empty study has an empty action log", {
  con <- store()
  log <- action_log(con, "S1")
  expect_equal(nrow(log), 0)
  expect_true(all(c("ACTIONID", "OVERDUE", "OWNER_NAME") %in% names(log)))
})
