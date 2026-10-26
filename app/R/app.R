#' Review app for a study
#'
#' Loads the study, opens its review store and builds one screen per display
#' configured in study.yml (`displays`), leaving out screens whose data the
#' study does not have.
#'
#' Every module receives the same context `ctx`: `study`, `con` (review
#' store), `user`, reactives `asof()`, `since()`, `site()`, `data()` and
#' `data_since()` (review data from [rtcrcore::review_data()], filtered to the
#' site), `record_hashes(form)` (hashes for record marks), `marks_rev()` and
#' `bump_marks()` (refresh after marking) and `goto_subject(id)`.
#'
#' @param study_dir Study folder.
#' @param extract Extract folder name; defaults to the latest.
#' @param user User ID when the hosting platform does not supply one
#'   (`session$user`); defaults to the `USER` environment variable.
#' @param store Review store config; defaults to study.yml `review_store`.
#' @return A Shiny app object.
#' @export
rtcr_app <- function(study_dir, extract = NULL, user = NULL, store = NULL) {
  study <- rtcrcore::load_study(study_dir, extract)
  con <- rtcrcore::open_review_store(if (is.null(store)) study$config$review_store else store, base_dir = study_dir)
  cache <- review_cache(study)
  displays <- resolve_displays(study$config$displays, cache(study$extract_date))
  app_cfg <- if (is.null(study$config$app)) list() else study$config$app
  sites <- sort(unique(cache(study$extract_date)$adsl$SITEID))
  profile_id <- names(Filter(function(d) d$module == "subject_profile", displays))[1]

  ui <- app_ui(study, displays, app_cfg, sites)

  server <- function(input, output, session) {
    who <- if (!is.null(session$user)) session$user else if (!is.null(user)) user else Sys.getenv("USER", "unknown")
    since0 <- tryCatch(rtcrcore::review_since(con, study$id, review_type(app_cfg), who,
                                              fallback = study$extract_date - 7),
                       error = function(e) study$extract_date - 7)
    shiny::updateDateInput(session, "since", value = since0)
    marks_rev <- shiny::reactiveVal(0)
    selected_subject <- shiny::reactiveVal(NULL)

    asof <- shiny::reactive(as.Date(shiny::req(input$asof)))
    ctx <- list(
      study = study, con = con, user = who, asof = asof,
      since = shiny::reactive(min(as.Date(shiny::req(input$since)), asof())),
      site = shiny::reactive(shiny::req(input$site)),
      marks_rev = marks_rev, bump_marks = function() marks_rev(marks_rev() + 1),
      selected_subject = selected_subject,
      goto_subject = function(id) {
        selected_subject(id)
        if (!is.na(profile_id)) bslib::nav_select("tabs", profile_id)
      }
    )
    ctx$data <- shiny::reactive(filter_site(cache(asof()), ctx$site()))
    ctx$data_since <- shiny::reactive(filter_site(cache(ctx$since()), ctx$site()))
    ctx$record_hashes <- function(form) record_hash_table(study, form, asof())

    # lapply, not for: each module must capture its own display's params (lazy evaluation).
    lapply(displays, function(d) d$def$server(d$id, ctx, d$params))

    # Deep links: ?tab=<display id> opens a screen, ?subject=<id> opens the profile.
    shiny::observeEvent(session$clientData$url_search, {
      q <- shiny::parseQueryString(session$clientData$url_search)
      if (!is.null(q$subject)) ctx$goto_subject(q$subject)
      else if (!is.null(q$tab) && q$tab %in% names(displays)) bslib::nav_select("tabs", q$tab)
    }, once = TRUE)
    shiny::observeEvent(input$goto_subject, ctx$goto_subject(input$goto_subject))
    shiny::observeEvent(input$tabs, rtcrcore::log_access(con, study$id, who, "view", input$tabs,
                                                         paste("as of", format(shiny::isolate(input$asof)))))
  }

  shiny::onStop(function() DBI::dbDisconnect(con))
  shiny::shinyApp(ui, server)
}

app_ui <- function(study, displays, app_cfg, sites) {
  panels <- lapply(displays, function(d) bslib::nav_panel(d$title, value = d$id, d$def$ui(d$id, d$params, study)))
  do.call(bslib::page_navbar, c(list(
    id = "tabs", title = paste(study$id, "| Data review"), fillable = FALSE,
    theme = bslib::bs_theme(version = 5, primary = "#2a78d6"),
    header = shiny::tags$head(shiny::tags$style(shiny::HTML(app_css))),
    sidebar = bslib::sidebar(
      width = 250,
      shiny::dateInput("asof", "Review date", value = study$extract_date, max = study$extract_date),
      shiny::dateInput("since", "Changes since", value = study$extract_date - 7, max = study$extract_date),
      shiny::selectInput("site", "Site", c("All sites", sites)),
      shiny::p(class = "note", sprintf("Extract %s. Changes since defaults to your last %s sign-off.",
                                       format(study$extract_date), review_type(app_cfg)))
    )), unname(panels), list(
    bslib::nav_spacer(),
    bslib::nav_item(shiny::tags$span(class = "note", if (isTRUE(app_cfg$simulated)) "Simulated data" else ""))
  )))
}

#' @rdname rtcr_app
#' @param ... Passed to [rtcr_app()].
#' @export
run_app <- function(study_dir, ...) shiny::runApp(rtcr_app(study_dir, ...))

review_type <- function(app_cfg) if (is.null(app_cfg$review_type)) "medical" else app_cfg$review_type

# Review data is computed once per review date and reused.
review_cache <- function(study) {
  memo <- new.env(parent = emptyenv())
  function(d) {
    key <- format(as.Date(d))
    if (is.null(memo[[key]])) memo[[key]] <- rtcrcore::review_data(study, d)
    memo[[key]]
  }
}

filter_site <- function(rd, site) {
  if (is.null(site) || identical(site, "All sites")) return(rd)
  rd$tables <- lapply(rd$tables, function(t) t[t$SITEID %in% site, , drop = FALSE])
  rd$adsl <- rd$adsl[rd$adsl$SITEID %in% site, , drop = FALSE]
  rd$findings <- rd$findings[rd$findings$SITEID %in% site, , drop = FALSE]
  rd$summary$N <- vapply(rd$summary$ID, function(id) sum(rd$findings$CHECKID == id), integer(1))
  rd$summary$RESULT <- ifelse(rd$summary$N == 0, "pass", rd$summary$SEVERITY)
  rd
}

# Record marks hash the collected fields only (never derived ones), so the same
# record has the same hash on every screen.
record_hash_table <- function(study, form, asof) {
  rtcrcore::record_hashes(rtcrcore::as_of(study$tables[[form]], asof))
}
