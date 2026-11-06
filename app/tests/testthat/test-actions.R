skip_if_not(dir.exists(study_dir), "example study not available")
skip_if_not_installed("RSQLite")

team_ctx <- function(env = parent.frame()) {
  ctx <- test_ctx(env = env)
  rtcrcore::set_team_member(ctx$con, ctx$study$id, "dm", "Dana Data", "setup", role = "Data manager", login = "tester")
  rtcrcore::set_team_member(ctx$con, ctx$study$id, "cra113", "Site 113 CRA", "setup", role = "CRA")
  ctx
}

test_that("actions fall into overdue, due soon, later and recently closed groups", {
  ctx <- team_ctx()
  s <- ctx$study$id
  # Closing is stamped with the real time, so the review date here is today.
  today <- Sys.Date()
  rtcrcore::raise_action(ctx$con, s, "Late one", "cra113", today - 11, "tester", target_type = "site", target_key = "113")
  rtcrcore::raise_action(ctx$con, s, "Soon", "dm", today + 4, "tester")
  rtcrcore::raise_action(ctx$con, s, "Later", "dm", today + 30, "tester")
  done <- rtcrcore::raise_action(ctx$con, s, "Done", "dm", today + 2, "tester")
  rtcrcore::update_action(ctx$con, s, done, "tester", status = "closed", note = "Fixed")

  g <- action_groups(rtcrcore::action_log(ctx$con, s, asof = today), today)
  expect_equal(g$overdue$rows$TITLE, "Late one")
  expect_match(g$overdue$summary, "Site 113 CRA · oldest 11 days late")
  expect_equal(g$soon$rows$TITLE, "Soon")
  expect_equal(g$later$rows$TITLE, "Later")
  expect_equal(g$closed$rows$TITLE, "Done")
  expect_equal(g$closed$label, "Closed in the last 14 days")
})

test_that("the site filter keeps that site's actions and study-wide ones", {
  ctx <- team_ctx()
  s <- ctx$study$id
  adsl <- shiny::isolate(ctx$data()$adsl)
  subj113 <- adsl$SUBJID[adsl$SITEID == "113"][1]
  other <- adsl$SUBJID[adsl$SITEID != "113"][1]
  rtcrcore::raise_action(ctx$con, s, "Study", "dm", "2027-01-10", "tester")
  rtcrcore::raise_action(ctx$con, s, "Site", "dm", "2027-01-10", "tester", target_type = "site", target_key = "113")
  rtcrcore::raise_action(ctx$con, s, "Subj", "dm", "2027-01-10", "tester", target_type = "subject", target_key = subj113)
  rtcrcore::raise_action(ctx$con, s, "Other", "dm", "2027-01-10", "tester", target_type = "record",
                         target_key = paste0("AE:", other, "|1"))
  kept <- site_actions(rtcrcore::action_log(ctx$con, s), "113", adsl)
  expect_setequal(kept$TITLE, c("Study", "Site", "Subj"))
})

test_that("raising an action from the screen adds it to the log", {
  ctx <- team_ctx()
  shiny::testServer(actions_server, args = list(ctx = ctx, params = list()), {
    expect_equal(nrow(log()), 0)
    session$setInputs(new = 1)
    session$setInputs(n_title = "Chase site 105 queries", n_owner = "cra113", n_due = Sys.Date() + 3,
                      n_type = "site", n_key = "105", n_area = "Data quality", n_note = "", create = 1)
    expect_equal(log()$TITLE, "Chase site 105 queries")
    expect_equal(log()$AREA, "Data quality")
    expect_equal(nrow(groups()$soon$rows), 1)
  })
})

test_that("the owner closes an action from its panel; others cannot", {
  ctx <- team_ctx()
  # Not `id`: inside testServer that is the module's own ID.
  mine <- rtcrcore::raise_action(ctx$con, ctx$study$id, "Confirm coding", "dm", "2027-01-10", "tester")
  other <- rtcrcore::raise_action(ctx$con, ctx$study$id, "Retrain", "cra113", "2027-01-10", "tester")
  shiny::testServer(actions_server, args = list(ctx = ctx, params = list()), {
    session$setInputs(pick = mine)
    session$setInputs(note = "Coded in EDC", status = "closed", due = as.Date("2027-01-10"), owner = "dm", save = 1)
    expect_equal(log()$STATUS[log()$ACTIONID == mine], "closed")

    session$setInputs(pick = other)
    session$setInputs(note = "Done", status = "closed", due = as.Date("2027-01-10"), owner = "cra113", save = 2)
    expect_equal(log()$STATUS[log()$ACTIONID == other], "open")
  })
})

test_that("the study team screen adds and changes members", {
  ctx <- test_ctx()
  shiny::testServer(team_server, args = list(ctx = ctx, params = list()), {
    session$setInputs(id = "mm", name = "Max Medic", role = "Medical monitor", email = "", login = "",
                      active = TRUE, save = 1)
    expect_equal(team()$NAME, "Max Medic")
    session$setInputs(name = "Max M.", save = 2)
    expect_equal(team()$NAME, "Max M.")
    expect_true(is.na(team()$LOGIN))
  })
})

test_that("the example study seeds its team and shows the action screens", {
  con <- rtcrcore::open_review_store(list(driver = "sqlite", path = ":memory:"))
  on.exit(DBI::dbDisconnect(con))
  st <- example_study()
  rtcrcore::seed_team(con, st$id, st$config$team, "study.yml")
  expect_equal(nrow(rtcrcore::team_members(con, st$id)), length(st$config$team))
  displays <- resolve_displays(st$config$displays, review_cache(st)(st$extract_date))
  expect_true(all(c("actions", "team") %in% names(displays)))
  html <- as.character(app_ui(st, displays, st$config$app, c("101", "113")))
  expect_match(html, "Action items")
  expect_match(html, "Study team")
})
