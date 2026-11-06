skip_if_not(dir.exists(study_dir), "example study not available")
skip_if_not_installed("RSQLite")

profile_params <- function() {
  p <- Filter(function(d) d$id == "profile", example_study()$config$displays)[[1]]$params
  p
}

lane_for <- function(type, form, ctx) {
  lane <- Filter(function(l) l$type == type && l$form == form, profile_params()$lanes)[[1]]
  rd <- shiny::isolate(ctx$data())
  lane_data(lane, rd$tables, "102-002", rd$adsl, ctx$study$schedule, ctx$study$thresholds)
}

test_that("study days count first dose as Day 1, with no day 0", {
  expect_equal(study_day(c("2026-04-28", "2026-04-29", "2026-04-27", "2026-07-21T10:21"), "2026-04-28"),
               c(1, 2, -1, 85))
})

test_that("each lane type gives the subject's points on the study-day scale", {
  ctx <- test_ctx()
  v <- lane_for("visits", "SV", ctx)
  expect_equal(nrow(v$points), 8)
  expect_true(v$points$MISSED[v$points$LABEL == "Wk 20"])
  expect_equal(v$points$DAY[v$points$LABEL == "Wk 20"], 141)              # placed at its scheduled day

  e <- lane_for("events", "AE", ctx)
  expect_equal(e$points$DAY, c(13, 80, 158, 178))
  expect_equal(e$points$ONGOING, c(FALSE, TRUE, TRUE, FALSE))
  expect_equal(e$points$LEVEL[2], "MODERATE")

  m <- lane_for("series", "MCSS", ctx)
  expect_equal(m$ref, 15.9)                                               # baseline from the BASE column
  expect_equal(m$points$VALUE[m$points$DAY == 170], 4.6)

  pk <- lane_for("series", "PK", ctx)
  expect_equal(pk$ref, 0.1)                                               # the lloq study threshold
  expect_equal(sum(pk$points$BELOW), 1)

  d <- lane_for("intervals", "DIARY", ctx)
  expect_equal(d$ref, 80)
  expect_equal(d$points$VALUE, c(55, 57, 59, 50, 53))
  expect_error(lane_data(list(type = "pie", form = "AE"), list(), "x", data.frame(), data.frame(), list()),
               "lane type must be one of")
})

test_that("the timeline draws every lane, the missed visit and the reference lines", {
  ctx <- test_ctx()
  lanes <- lapply(c("visits", "events", "series", "intervals"), function(type) {
    form <- c(visits = "SV", events = "AE", series = "MCSS", intervals = "DIARY")[[type]]
    lane_for(type, form, ctx)
  })
  html <- as.character(timeline_svg(lanes, 248, 170))
  expect_match(html, "^<svg class=\"tl\"")
  for (s in c("Visits", "Adverse events", "mCSS total", "Dosing compliance", "Wk 20 missed", "ongoing at last contact",
              "baseline 15.9", "target 80%", "review date", "55%")) {
    expect_match(html, s, fixed = TRUE)
  }
  expect_match(as.character(timeline_svg(list(), 1, 1)), "No dated records")
})

test_that("the profile header shows facts with labels and what needs a look", {
  ctx <- test_ctx(since = "2026-12-24")
  shiny::testServer(profile_server, args = list(ctx = ctx, params = profile_params()), {
    session$setInputs(subject = "102-002")
    html <- output$header$html
    for (s in c("102-002", "Sex", "Age", "Treatment", "First dose (Day 1)", "Needs a look", "Missed visit",
                "Low dosing compliance ×5", "nothing new since 24 Dec 2026")) {
      expect_match(html, s, fixed = TRUE)
    }
    expect_false(grepl("New or changed AE", html))                        # info findings are not flags
    expect_match(output$timeline$html, "mCSS total")
  })
})

test_that("forms start collapsed, with a summary, flags and labelled records", {
  ctx <- test_ctx()
  shiny::testServer(profile_server, args = list(ctx = ctx, params = profile_params()), {
    session$setInputs(subject = "102-002")
    html <- output$forms$html
    expect_match(html, "accordion")
    expect_false(grepl("accordion-collapse collapse show", html))
    expect_match(html, "Adverse events")
    expect_match(html, "4 records")
    expect_match(html, "Preferred term")                                   # labels, not AEDECOD
    expect_false(grepl(">AEDECOD<", html))
    expect_match(html, "Flagged by")
    expect_match(html, "EX-COMP")
  })
})

test_that("reviewers hide lanes and forms, and add a measure of their own", {
  ctx <- test_ctx()
  shiny::testServer(profile_server, args = list(ctx = ctx, params = profile_params()), {
    session$setInputs(subject = "102-002")
    expect_match(output$timeline$html, "PK concentration")
    keys <- names(all_lanes())
    session$setInputs(lanes = setdiff(keys, keys[6]))                     # hide the PK lane
    expect_false(grepl("PK concentration", output$timeline$html))
    session$setInputs(lanes = character())
    expect_match(output$timeline$html, "No timeline lanes chosen")

    session$setInputs(lanes = keys, add_form = "MCSS", add_field = "CHG", add = 1)
    expect_equal(added()[[1]]$date, "PHOTODT")                            # placed like the study's own mCSS lane
    expect_true("added1" %in% names(shown_lanes()))
    session$setInputs(lanes = c(keys, "added1"))
    expect_match(output$timeline$html, "Change from baseline")

    session$setInputs(forms_on = c("AE", "PK"))
    html <- output$forms$html
    expect_match(html, "Adverse events")
    expect_false(grepl("Dosing diary", html))
  })
  st <- example_study()
  expect_true("CHG" %in% measure_fields(shiny::isolate(ctx$data())$tables$MCSS, st))
  expect_false("VISITNUM" %in% measure_fields(shiny::isolate(ctx$data())$tables$MCSS, st))
})

test_that("the source view shows the subject's rows as they arrived, marking external data", {
  ctx <- test_ctx()
  st <- example_study()
  pk <- source_rows(st, "PK", "102-002")
  expect_equal(nrow(pk), 3)
  expect_true(all(vapply(pk, is.character, logical(1))))
  shiny::testServer(profile_server, args = list(ctx = ctx, params = profile_params()), {
    session$setInputs(subject = "102-002", view = "source")
    html <- output$forms$html
    expect_match(html, "source file vendor_pk.csv")
    expect_match(html, "External data")
    expect_match(html, "source file edc_ae.csv")
    expect_match(html, "PCORRES")                                         # original column names
    expect_false(grepl("accordion-collapse collapse show", html))
  })
})

test_that("data pulls come from the schedule and the extracts, and each record names its pull", {
  st <- example_study()
  p <- data_pulls(st)
  expect_equal(nrow(p), 10)
  expect_equal(p$DATE[1], as.Date("2026-01-14"))
  expect_equal(tail(p$DATE, 1), as.Date("2026-12-31"))                  # the loaded extract is a pull too
  expect_equal(pull_of(as.Date(c("2025-12-02", "2026-01-14", "2026-01-15", NA)), p), c(1L, 1L, 2L, NA))
  expect_equal(record_pull_text(as.Date("2026-01-15"), as.Date("2026-01-20"), p), "Pull 2 \u00b7 25 Feb 2026")
  expect_equal(record_pull_text(as.Date("2026-01-15"), as.Date("2026-03-01"), p),
               "Pull 2 \u00b7 25 Feb 2026, updated in pull 3 \u00b7 08 Apr 2026")
  expect_true(all(data_pulls(st, "2026-06-30")$DATE <= as.Date("2026-06-30")))   # none after the review date

  weekly <- st
  weekly$config$data_pulls <- list(start = "2025-12-03", every_days = 7)
  w <- data_pulls(weekly)
  expect_equal(diff(w$DATE[1:3]), as.difftime(c(7, 7), units = "days"))
})

test_that("the timeline marks the pulls that brought the subject's data, and forms name each record's pull", {
  ctx <- test_ctx(since = "2026-12-24")
  shiny::testServer(profile_server, args = list(ctx = ctx, params = profile_params()), {
    session$setInputs(subject = "102-002")
    html <- output$timeline$html
    expect_match(html, "Data pulls")
    expect_match(html, ">P[0-9]+<")
    session$setInputs(forms_on = "AE")
    forms <- output$forms$html
    expect_match(forms, "Data pull")
    expect_match(forms, 'class="pull-chip">Pull [0-9]+ \u00b7 ')
  })
  sp <- subject_pulls(shiny::isolate(ctx$data())$tables, "AE", "102-002", data_pulls(ctx$study), as.Date("2026-04-28"))
  expect_equal(sp$PULL, c(4L, 6L, 8L))                                   # the two October AEs came in one pull
  expect_true(all(sp$DAY > 1))
})
