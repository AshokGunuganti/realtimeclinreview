suppressPackageStartupMessages({ library(shiny); library(dplyr) })
src <- read_sources(src_dir); rv <- derive_review(src, load_schedule(APP_CFG$schedule_file))

mk_ctrl <- function(asof, site = "All sites", pop = "all") list(
  asof = reactiveVal(asof), since = reactive(asof - 14), site = reactive(site), pop = reactive(pop),
  alert = reactive(1.5), cthr = reactive(80), about = reactive(data.frame(Item = "t", Value = "t")))

render_all <- function(output, ids) for (o in ids) expect_no_error(output[[o]], message = o)

for (case in list(list(d = as.Date("2026-10-22"), site = "All sites", pop = "all"),
                  list(d = as.Date("2026-11-19"), site = "105", pop = "itt"),
                  list(d = as.Date("2026-01-05"), site = "All sites", pop = "all"))) {
  label <- paste(case$d, case$site, case$pop)
  ctrl <- mk_ctrl(case$d, case$site, case$pop)
  snap <- reactive(review_snapshot(rv, ctrl$asof(), case$site, case$pop))

  test_that(paste("What's new renders:", label), {
    reg <- reactiveValues()
    testServer(whatsnew_server, args = list(snap = snap, ctrl = ctrl, registry = reg), {
      render_all(output, c("kpis", "att"))
      expect_true("Changes" %in% names(reactiveValuesToList(reg)))
    })
  })
  test_that(paste("Efficacy renders:", label), {
    testServer(efficacy_server, args = list(snap = snap, ctrl = ctrl, registry = reactiveValues()), {
      session$setInputs(vis = "8"); render_all(output, c("kpis"))
      for (o in c("p_iga", "p_mcss", "p_dist")) tryCatch(output[[o]], error = function(e)
        expect_true(inherits(e, "shiny.silent.error") || grepl("No ", conditionMessage(e)), info = o))
    })
  })
  test_that(paste("Safety renders:", label), {
    testServer(safety_server, args = list(snap = snap, ctrl = ctrl, registry = reactiveValues()), {
      render_all(output, "kpis")
      for (o in c("p_ae", "p_pk")) tryCatch(output[[o]], error = function(e) expect_true(grepl("No ", conditionMessage(e)), info = o))
    })
  })
  test_that(paste("Data quality renders:", label), {
    testServer(quality_server, args = list(snap = snap, ctrl = ctrl, registry = reactiveValues()), {
      render_all(output, "kpis")
      for (o in c("p_status", "p_q")) tryCatch(output[[o]], error = function(e) expect_true(grepl("No ", conditionMessage(e)), info = o))
    })
  })
  test_that(paste("Sites renders:", label), {
    testServer(sites_server, args = list(snap = snap, ctrl = ctrl, registry = reactiveValues()), {
      render_all(output, "kpis")
      for (o in c("p_lag", "p_comp")) tryCatch(output[[o]], error = function(e) expect_true(grepl("No ", conditionMessage(e)), info = o))
    })
  })
  test_that(paste("Profile renders:", label), {
    sel <- reactiveVal(NULL)
    testServer(profile_server, args = list(snap = snap, ctrl = ctrl, selected = sel), {
      session$flushReact()
      expect_false(is.null(sel()))
      render_all(output, c("note", "kpis"))
      for (o in c("p_m", "p_c")) tryCatch(output[[o]], error = function(e) expect_true(grepl("No ", conditionMessage(e)), info = o))
      first <- sel(); session$setInputs(nxt = 1); expect_false(identical(sel(), first))
    })
  })
  test_that(paste("Checks render:", label), {
    testServer(checks_server, args = list(src = src, rv = rv, snap = snap, ctrl = ctrl, registry = reactiveValues()), {
      render_all(output, "kpis")
    })
  })
}

test_that("workbook writer produces a valid xlsx with every sheet", {
  s <- review_snapshot(rv, as.Date("2026-10-22")); f <- tempfile(fileext = ".xlsx")
  write_review_xlsx(list(Changes = change_log(s, as.Date("2026-10-08")), `Site performance` = site_summary(s),
                         Issues = issue_listing(s)), f, data.frame(Item = "a", Value = "b"))
  expect_equal(openxlsx::getSheetNames(f), c("About", "Changes", "Site performance", "Issues"))
  expect_gt(nrow(openxlsx::read.xlsx(f, "Site performance")), 5)
})
