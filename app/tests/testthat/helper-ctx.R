study_dir <- test_path("..", "..", "..", "studies", "example")

example_study <- local({
  memo <- NULL
  function() {
    if (is.null(memo)) memo <<- rtcrcore::load_study(study_dir)
    memo
  }
})

# The context rtcr_app() gives every module, with fixed dates and an in-memory store.
test_ctx <- function(asof = "2026-12-31", since = "2026-10-22", site = "All sites", env = parent.frame()) {
  study <- example_study()
  con <- rtcrcore::open_review_store(list(driver = "sqlite", path = ":memory:"))
  withr::defer(DBI::dbDisconnect(con), envir = env)
  cache <- review_cache(study)
  rev <- shiny::reactiveVal(0)
  picked <- shiny::reactiveVal(NULL)
  panel <- shiny::reactiveVal(NULL)
  ctx <- list(study = study, con = con, user = "tester",
              asof = shiny::reactive(as.Date(asof)), since = shiny::reactive(as.Date(since)),
              site = shiny::reactive(site), marks_rev = rev, bump_marks = function() rev(rev() + 1),
              selected_subject = picked, panel_subject = panel,
              goto_subject = function(id) panel(id), open_profile = function(id) picked(id),
              goto_site = function(id) NULL, timeline = list(list(form = "AE", day = "ASTDY", label = "AETERM")))
  ctx$data <- shiny::reactive(filter_site(cache(asof), site))
  ctx$data_since <- shiny::reactive(filter_site(cache(since), site))
  ctx$record_hashes <- function(form) record_hash_table(study, form, as.Date(asof))
  ctx$queries <- shiny::reactive(query_table(study, ctx$data(), as.Date(asof)))
  ctx$changes <- shiny::reactive(record_changes(study, as.Date(since), as.Date(asof), site))
  ctx$new_flags <- shiny::reactive(new_flag_keys(ctx$data_since(), ctx$data()))
  ctx$review_items <- shiny::reactive({
    rev()
    queue_items(ctx$changes(), clinical_findings(ctx$data()), con, study$id, keep = ctx$new_flags())
  })
  ctx$review_queue <- shiny::reactive(subject_queue(ctx$review_items(), ctx$data()$adsl, c("AE", "PK", "MCSS")))
  ctx
}
