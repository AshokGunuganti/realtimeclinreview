# Site scorecard: visit completion, entry timeliness, queries and open
# findings per site, worst first, with query aging below. Params: visits (the
# visit grid params: form, date, not_done, fallback), lag (form and date for
# entry lag; defaults to visits), query_age, exclude_tiers (check tiers not
# counted as findings; default clinical), form_status (form status report, for
# SDV on the site page).
#
# With Site set to All sites the screen is the scorecard; with one site it is
# that site's page: its subjects, overdue visits and open items. Clicking a
# site in the scorecard sets Site for every screen.

sites_ui <- function(id, params, study) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("head")),
    shiny::uiOutput(ns("kpis")), shiny::br(),
    shiny::conditionalPanel(
      "input.site == 'All sites'",
      table_card_ui(ns("site"), params$title,
                    paste("Sites needing the most attention first. Entry lag: days from visit to EDC entry.",
                          "Queries per 10 entered visits. Click a site ID to open the site's page.")),
      bslib::card(fill = FALSE, bslib::card_header("Median entry lag by site"),
                  plotly::plotlyOutput(ns("lag"), height = "320px"),
                  shiny::p(class = "note", "Dashed line: entry lag target."))),
    shiny::conditionalPanel(
      "input.site != 'All sites'",
      table_card_ui(ns("subj"), "Subjects", paste("Next visit due, overdue visits and open items per subject.",
                                                  "Click a subject to open it.")),
      table_card_ui(ns("over"), "Overdue visits", "Past target date plus window and not entered.")),
    table_card_ui(ns("qry"), "Unresolved queries", "Open or answered on the review date, oldest first.")
  )
}

sites_server <- function(id, ctx, params) {
  shiny::moduleServer(id, function(input, output, session) {
    v <- params$visits
    lg <- if (is.null(params$lag)) list(form = v$form, date = v$date) else params$lag
    target <- if (is.null(ctx$study$thresholds$entry_lag_target)) 5 else ctx$study$thresholds$entry_lag_target
    age <- if (is.null(params$query_age)) 30 else params$query_age
    excl <- if (is.null(params$exclude_tiers)) c("clinical", "structural") else params$exclude_tiers

    lag <- shiny::reactive(rtcrcore::entry_lag(ctx$data()$tables, lg$form, lg$date))
    visits <- shiny::reactive({
      rd <- ctx$data()
      rtcrcore::visit_status(rd$tables, rd$adsl, ctx$study$schedule, ctx$asof(), v$form, v$date,
                             not_done = v$not_done, fallback = v$fallback)
    })
    score <- shiny::reactive({
      rd <- ctx$data()
      rtcrcore::site_scorecard(rd$adsl, visits(), lag(), ctx$queries(), findings_by_tier(rd, excl),
                               lag_target = target, query_age = age)
    })
    one <- shiny::reactive(!identical(ctx$site(), "All sites"))
    fstat <- shiny::reactive({
      t <- if (!is.null(params$form_status)) ctx$data()$tables[[params$form_status]]
      if (is.null(t)) NULL else rtcrcore::form_status(t, ctx$asof())
    })
    output$head <- shiny::renderUI({
      if (!one()) return(NULL)
      s <- score()
      shiny::div(class = "site-head",
                 shiny::h4(paste("Site", ctx$site())), shiny::HTML(badge(s$STATUS[1])),
                 shiny::span(class = "note", sprintf("%d subjects, %d discontinued", s$SUBJECTS[1], s$DISCONT[1])),
                 shiny::tags$a(class = "sid", onclick = "Shiny.setInputValue('goto_site','All sites',{priority:'event'})",
                               shiny::HTML("&lsaquo; All sites")))
    })
    output$kpis <- shiny::renderUI({
      s <- score()
      l <- lag()$LAG
      q <- ctx$queries()
      open <- if (is.null(q)) NULL else q[q$STATUS != "Closed", , drop = FALSE]
      lag_kpi <- kpi("Median entry lag", if (length(l)) sprintf("%g days", stats::median(l)) else "\u2013",
                     if (length(l)) sprintf("%.0f%% within %g days", 100 * mean(l <= target), target))
      qry_kpi <- kpi("Unresolved queries", if (is.null(open)) "\u2013" else nrow(open),
                     if (!is.null(open) && nrow(open)) sprintf("oldest %g days", max(open$AGE)))
      fnd_kpi <- kpi("Open findings", if (is.null(s$FINDINGS)) "\u2013" else sum(s$FINDINGS), "from data checks")
      tiles <- if (one()) {
        vs <- visits()
        fs <- fstat()
        list(kpi("Overdue visits", sum(vs$STATUS == "Overdue / not entered"), sprintf("%d missed", sum(vs$STATUS == "Missed"))),
             if (is.null(fs)) fnd_kpi else kpi("Forms awaiting SDV", sum(fs$STATUS == "Entered"), sprintf("of %d", nrow(fs))),
             qry_kpi, lag_kpi)
      } else {
        list(kpi("Sites", nrow(s), sprintf("%d need attention", sum(s$STATUS == "Needs attention"))), lag_kpi, qry_kpi,
             fnd_kpi)
      }
      do.call(bslib::layout_columns, c(list(fill = FALSE, col_widths = bslib::breakpoints(sm = 6, lg = 3)), tiles))
    })
    tbl <- shiny::reactive({
      s <- score()
      cols <- c(Site = "SITEID", Status = "STATUS", Subjects = "SUBJECTS", Discontinued = "DISCONT",
                `Visits due` = "VISDUE", `% complete in window` = "PCTWIN", Overdue = "OVERDUE", Missed = "MISSED",
                `Median entry lag (d)` = "LAGMED", `% within target` = "PCTLAG", Queries = "QUERIES",
                `Queries / 10 visits` = "QRATE", Unresolved = "OPENQ", `Oldest unresolved (d)` = "OLDESTQ",
                `Median days to close` = "MEDCLOSE", `Open findings` = "FINDINGS")
      cols <- cols[cols %in% names(s)]
      x <- stats::setNames(s[unname(cols)], names(cols))
      x$Site <- site_link(x$Site)
      x$Status <- badge(x$Status)
      x
    })
    table_card_server("site", tbl, ctx, "Site scorecard", page_length = 20, selection = "none")
    output$lag <- plotly::renderPlotly({
      s <- score()
      s <- s[!is.na(s$LAGMED), , drop = FALSE]
      shiny::validate(shiny::need(nrow(s) > 0, "No visits entered yet."))
      s <- s[order(s$LAGMED), , drop = FALSE]
      p <- plotly::plot_ly(s, y = ~factor(SITEID, levels = SITEID), x = ~LAGMED, type = "bar", orientation = "h",
                           marker = list(color = ifelse(s$LAGMED > target, "#d03b3b", "#2a78d6")),
                           text = ~sprintf("Site %s: median %g days, %g%% within target", SITEID, LAGMED, PCTLAG),
                           hoverinfo = "text", textposition = "none")
      plotly::layout(p, xaxis = list(title = "Median days from visit to entry"), yaxis = list(title = "Site"),
                     shapes = list(list(type = "line", x0 = target, x1 = target, yref = "paper", y0 = 0, y1 = 1,
                                        line = list(color = "#52514e", dash = "dash"))), bargap = 0.3)
    })
    queries <- shiny::reactive({
      q <- ctx$queries()
      if (is.null(q)) return(data.frame(Subject = character()))
      q <- q[q$STATUS != "Closed", , drop = FALSE]
      q <- q[order(-q$AGE), , drop = FALSE]
      data.frame(Subject = sid_link(q$SUBJID), Site = q$SITEID, Query = q$RECID, Form = q$QRYFORM,
                 Folder = q$FOLDER, Text = htmltools::htmlEscape(q$QRYTEXT), Status = badge(q$STATUS),
                 Opened = format(q$QRYOPDT), `Age (d)` = q$AGE, check.names = FALSE, stringsAsFactors = FALSE)
    })
    table_card_server("qry", queries, ctx, "Unresolved queries", page_length = 15)
    subj <- shiny::reactive({
      site_subjects(ctx$data()$adsl, visits(), ctx$queries(), fstat(), findings_by_tier(ctx$data(), excl))
    })
    table_card_server("subj", subj, ctx, "Site subjects", page_length = 25, selection = "none")
    overdue <- shiny::reactive({
      x <- visits()
      x <- x[x$STATUS == "Overdue / not entered", , drop = FALSE]
      x <- x[order(-x$DAYSOVER), , drop = FALSE]
      data.frame(Subject = sid_link(x$SUBJID), Visit = x$VISIT, `Target date` = format(x$TARGDT),
                 `Days overdue` = x$DAYSOVER, check.names = FALSE, stringsAsFactors = FALSE)
    })
    table_card_server("over", overdue, ctx, "Overdue visits", selection = "none")
  })
}

# One row per subject of a site: next visit due, overdue and missed visits,
# unresolved queries, forms awaiting SDV and open data findings.
site_subjects <- function(adsl, visits, queries = NULL, fstat = NULL, findings = NULL) {
  ids <- adsl$SUBJID
  nxt <- visits[visits$STATUS == "Not yet due", , drop = FALSE]
  nxt <- nxt[order(nxt$SUBJID, nxt$TARGDT), , drop = FALSE]
  nxt <- nxt[!duplicated(nxt$SUBJID), , drop = FALSE]
  k <- match(ids, nxt$SUBJID)
  over <- visits[visits$STATUS == "Overdue / not entered", , drop = FALSE]
  x <- data.frame(Subject = sid_link(ids), Status = tools::toTitleCase(tolower(adsl$EOSSTT)),
                  `Next visit` = ifelse(is.na(k), "", paste(nxt$VISIT[k], format(nxt$TARGDT[k], "%d %b %Y"), sep = " \u00b7 ")),
                  Overdue = vapply(ids, function(s) paste(over$VISIT[over$SUBJID == s], collapse = ", "), ""),
                  Missed = count_by(visits$SUBJID[visits$STATUS == "Missed"], ids),
                  check.names = FALSE, stringsAsFactors = FALSE)
  if (!is.null(queries)) {
    open <- queries[queries$STATUS != "Closed", , drop = FALSE]
    x$`Unresolved queries` <- count_by(open$SUBJID, ids)
    oldest <- vapply(ids, function(s) { a <- open$AGE[open$SUBJID == s]; if (length(a)) max(a) else NA_real_ }, 0)
    x$`Oldest query (d)` <- oldest
  }
  if (!is.null(fstat)) x$`Awaiting SDV` <- count_by(fstat$SUBJID[fstat$STATUS == "Entered"], ids)
  if (!is.null(findings)) x$`Open findings` <- count_by(findings$SUBJID, ids)
  rownames(x) <- NULL
  x
}
