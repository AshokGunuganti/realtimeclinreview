#' Review app for a study
#'
#' Loads the study, opens its review store and builds one screen per display
#' configured in study.yml (`displays`), leaving out screens whose data the
#' study does not have. Displays with the same `group` share a menu.
#'
#' The review date, "changes since" date, site and a subject search sit in a
#' strip at the top of every screen. Clicking a subject anywhere opens it in a
#' side panel over the current screen.
#'
#' Every module receives the same context `ctx`: `study`, `con` (review
#' store), `user`, reactives `asof()`, `since()`, `site()`, `data()` and
#' `data_since()` (review data from [rtcrcore::review_data()], filtered to the
#' site), `queries()` (query status from the query report named by study.yml
#' `app: queries`, or `NULL`), `changes()` (records new, changed or deleted in
#' the window), `review_items()` and `review_queue()` (the review queue),
#' `record_hashes(form)` (hashes for record marks), `marks_rev()` and
#' `bump_marks()` (refresh after marking), `goto_subject(id)` (opens the
#' subject panel), `open_profile(id)` and `goto_site(id)`.
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
  subjects <- sort(unique(cache(study$extract_date)$adsl$SUBJID))
  profile <- Filter(function(d) d$module == "subject_profile", displays)
  profile_id <- if (length(profile)) names(profile)[1] else NA
  timeline <- if (length(profile)) profile[[1]]$params$timeline else list()

  ui <- app_ui(study, displays, app_cfg, sites, subjects)

  server <- function(input, output, session) {
    who <- if (!is.null(session$user)) session$user else if (!is.null(user)) user else Sys.getenv("USER", "unknown")
    since0 <- tryCatch(rtcrcore::review_since(con, study$id, review_type(app_cfg), who,
                                              fallback = study$extract_date - 7),
                       error = function(e) study$extract_date - 7)
    shiny::updateDateInput(session, "since", value = since0)
    marks_rev <- shiny::reactiveVal(0)
    selected_subject <- shiny::reactiveVal(NULL)
    panel_subject <- shiny::reactiveVal(NULL)

    asof <- shiny::reactive(as.Date(shiny::req(input$asof)))
    ctx <- list(
      study = study, con = con, user = who, asof = asof,
      since = shiny::reactive(min(as.Date(shiny::req(input$since)), asof())),
      site = shiny::reactive(shiny::req(input$site)),
      marks_rev = marks_rev, bump_marks = function() marks_rev(marks_rev() + 1),
      selected_subject = selected_subject, panel_subject = panel_subject,
      open_profile = function(id) {
        selected_subject(id)
        # Root session: called from inside modules, whose session would namespace "tabs".
        if (!is.na(profile_id)) bslib::nav_select("tabs", profile_id, session = session)
      },
      goto_site = function(id) shiny::updateSelectInput(session, "site", selected = id)
    )
    ctx$data <- shiny::reactive(filter_site(cache(asof()), ctx$site()))
    ctx$data_since <- shiny::reactive(filter_site(cache(ctx$since()), ctx$site()))
    ctx$record_hashes <- function(form) record_hash_table(study, form, asof())
    ctx$queries <- shiny::reactive(query_table(study, ctx$data(), asof()))
    ctx$changes <- shiny::reactive(record_changes(study, ctx$since(), asof(), ctx$site()))
    ctx$new_flags <- shiny::reactive(new_flag_keys(ctx$data_since(), ctx$data()))
    ctx$review_items <- shiny::reactive({
      marks_rev()
      queue_items(ctx$changes(), clinical_findings(ctx$data()), con, study$id, keep = ctx$new_flags())
    })
    ctx$review_queue <- shiny::reactive(subject_queue(ctx$review_items(), ctx$data()$adsl, unlist(app_cfg$priority)))

    ctx$timeline <- timeline
    panel <- subject_panel_server("panel", ctx, timeline)
    ctx$goto_subject <- panel$open

    # lapply, not for: each module must capture its own display's params (lazy evaluation).
    lapply(displays, function(d) d$def$server(d$id, ctx, d$params))

    load_id <- names(Filter(function(d) d$module == "data_load", displays))[1]
    output$load_banner <- shiny::renderUI({
      msg <- load_banner(study, ctx$data())
      if (is.null(msg)) return(NULL)
      shiny::div(class = "load-banner", role = "status", shiny::span(msg),
                 if (!is.na(load_id)) shiny::actionLink("goto_load", "Details"))
    })
    shiny::observeEvent(input$goto_load, bslib::nav_select("tabs", load_id))
    shiny::observeEvent(input$nav, bslib::nav_select("tabs", input$nav))
    output$page_title <- shiny::renderText({
      d <- displays[[shiny::req(input$tabs)]]
      if (is.null(d)) "" else d$title
    })
    shiny::observeEvent(input$tabs, session$sendCustomMessage("navactive", input$tabs))
    output$n_review <- shiny::renderText({
      n <- sum(ctx$review_queue()$STATUS == "To review")
      if (n) n else ""
    })
    # Deep links: ?tab=<display id> opens a screen, ?subject=<id> opens the subject panel.
    shiny::observeEvent(session$clientData$url_search, {
      q <- shiny::parseQueryString(session$clientData$url_search)
      if (!is.null(q$tab) && q$tab %in% names(displays)) bslib::nav_select("tabs", q$tab)
      if (!is.null(q$subject)) ctx$goto_subject(q$subject)
    }, once = TRUE)
    shiny::observeEvent(input$goto_subject, ctx$goto_subject(input$goto_subject))
    shiny::observeEvent(input$goto_site, {
      ctx$goto_site(input$goto_site)
      sites_id <- names(Filter(function(d) d$module == "site_scorecard", displays))[1]
      if (!is.na(sites_id)) bslib::nav_select("tabs", sites_id)
    })
    shiny::observeEvent(input$find, {
      if (nzchar(input$find)) {
        ctx$goto_subject(input$find)
        shiny::updateSelectizeInput(session, "find", selected = "")
      }
    })
    output$workbook <- shiny::downloadHandler(
      filename = function() paste0(study$id, "_review_", format(asof(), "%Y%m%d"), ".xlsx"),
      content = function(file) {
        review_workbook(ctx$data(), study, file, about_table(ctx), con)
        rtcrcore::log_access(con, study$id, who, "export", basename(file),
                             paste("review workbook as of", format(asof()), "site", ctx$site()))
      })
    shiny::observeEvent(input$tabs, rtcrcore::log_access(con, study$id, who, "view", input$tabs,
                                                         paste("as of", format(shiny::isolate(input$asof)))))
  }

  shiny::onStop(function() DBI::dbDisconnect(con))
  shiny::shinyApp(ui, server)
}

app_ui <- function(study, displays, app_cfg, sites, subjects = character()) {
  # Sidebar: every screen one click away, grouped under plain headings in the
  # order the groups first appear; ungrouped screens head the list.
  groups <- vapply(displays, function(d) if (is.null(d$group)) "" else d$group, "")
  link <- function(d) {
    shiny::tags$a(class = "side-link", href = paste0("?tab=", d$id), `data-value` = d$id,
                  onclick = "rtcrNav(this); return false;", shiny::span(d$title),
                  if (d$module == "review_queue") shiny::span(class = "side-count", shiny::textOutput("n_review", inline = TRUE)))
  }
  nav <- lapply(unique(groups), function(g) {
    shiny::tagList(if (nzchar(g)) shiny::div(class = "side-group", g), unname(lapply(displays[groups == g], link)))
  })
  sidebar <- bslib::sidebar(
    width = 236, open = list(desktop = "open", mobile = "closed"), class = "side",
    shiny::div(class = "side-brand", study$id),
    shiny::tags$nav(`aria-label` = "Screens", class = "side-nav", nav),
    shiny::div(class = "side-foot", paste("Extract", format(study$extract_date)),
               if (isTRUE(app_cfg$simulated)) shiny::tagList(shiny::br(), "Simulated data")))
  head <- shiny::div(
    class = "page-head",
    shiny::h1(class = "page-title", shiny::textOutput("page_title", inline = TRUE)),
    shiny::div(
      class = "ctx-strip",
      shiny::dateInput("asof", "Review date", value = study$extract_date, max = study$extract_date, width = "128px"),
      shiny::dateInput("since", "Changes since", value = study$extract_date - 7, max = study$extract_date,
                       width = "128px"),
      shiny::selectInput("site", "Site", c("All sites", sites), width = "120px"),
      shiny::selectizeInput("find", "Find subject", c("", subjects), width = "140px",
                            options = list(placeholder = "Subject ID")),
      shiny::downloadButton("workbook", "Review workbook", class = "btn-sm btn-outline-primary", icon = NULL)))
  panels <- lapply(displays, function(d) bslib::nav_panel_hidden(value = d$id, d$def$ui(d$id, d$params, study)))
  bslib::page_sidebar(
    title = NULL, fillable = FALSE, sidebar = sidebar, window_title = paste(study$id, "data review"),
    theme = bslib::bs_theme(version = 5, primary = "#2459c9", fg = "#16181d", bg = "#ffffff",
                            base_font = bslib::font_google("IBM Plex Sans", local = FALSE),
                            code_font = bslib::font_google("IBM Plex Mono", local = FALSE),
                            "border-radius" = "6px", "font-size-base" = "0.9rem"),
    shiny::tags$head(shiny::tags$style(shiny::HTML(app_css))),
    head,
    shiny::uiOutput("load_banner"),
    do.call(bslib::navset_hidden, c(list(id = "tabs"), unname(panels))),
    subject_panel_ui("panel"),
    shiny::tags$script(shiny::HTML(app_js))
  )
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

# Query status from the query report named by study.yml `app: queries`.
query_table <- function(study, data, asof) {
  q <- study$config$app$queries
  t <- if (!is.null(q)) data$tables[[q]]
  if (is.null(t)) NULL else rtcrcore::query_status(t, asof, study$mapping)
}

# Forms holding clinical data, leaving out operational reports (FORMTYPE report).
clinical_forms <- function(study) {
  Filter(function(f) !identical(study$mapping$forms[[f]]$type, "report"), names(study$tables))
}

# Findings from checks whose tier is not excluded. The default leaves data
# issues: no clinical review flags and no programmer (structural) checks.
findings_by_tier <- function(data, exclude = c("clinical", "structural")) {
  keep <- data$summary$ID[!data$summary$TIER %in% unlist(exclude)]
  data$findings[data$findings$CHECKID %in% keep, , drop = FALSE]
}

# Record marks hash the collected fields only (never derived ones), so the same
# record has the same hash on every screen.
record_hash_table <- function(study, form, asof) {
  rtcrcore::record_hashes(rtcrcore::as_of(study$tables[[form]], asof))
}
