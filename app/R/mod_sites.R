# Site scorecard: visit completion, entry timeliness, queries and open
# findings per site, worst first, with query aging below. Params: visits (the
# visit grid params: form, date, not_done, fallback), lag (form and date for
# entry lag; defaults to visits), query_age, exclude_tiers (check tiers not
# counted as findings; default clinical).

sites_ui <- function(id, params, study) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("kpis")), shiny::br(),
    table_card_ui(ns("site"), params$title,
                  paste("Sites needing the most attention first. Entry lag: days from visit to EDC entry.",
                        "Queries per 10 entered visits. Click a site to list only its unresolved queries.")),
    bslib::card(fill = FALSE, bslib::card_header("Median entry lag by site"),
                plotly::plotlyOutput(ns("lag"), height = "320px"),
                shiny::p(class = "note", "Dashed line: entry lag target.")),
    table_card_ui(ns("qry"), "Unresolved queries", "Open or answered on the review date, oldest first.")
  )
}

sites_server <- function(id, ctx, params) {
  shiny::moduleServer(id, function(input, output, session) {
    v <- params$visits
    lg <- if (is.null(params$lag)) list(form = v$form, date = v$date) else params$lag
    target <- if (is.null(ctx$study$thresholds$entry_lag_target)) 5 else ctx$study$thresholds$entry_lag_target
    age <- if (is.null(params$query_age)) 30 else params$query_age
    excl <- if (is.null(params$exclude_tiers)) "clinical" else params$exclude_tiers

    lag <- shiny::reactive(rtcrcore::entry_lag(ctx$data()$tables, lg$form, lg$date))
    score <- shiny::reactive({
      rd <- ctx$data()
      visits <- rtcrcore::visit_status(rd$tables, rd$adsl, ctx$study$schedule, ctx$asof(), v$form, v$date,
                                       not_done = v$not_done, fallback = v$fallback)
      rtcrcore::site_scorecard(rd$adsl, visits, lag(), ctx$queries(), findings_by_tier(rd, excl),
                               lag_target = target, query_age = age)
    })
    output$kpis <- shiny::renderUI({
      s <- score()
      l <- lag()$LAG
      q <- ctx$queries()
      open <- if (is.null(q)) NULL else q[q$STATUS != "Closed", , drop = FALSE]
      do.call(bslib::layout_columns, c(list(fill = FALSE, col_widths = bslib::breakpoints(sm = 6, lg = 3)), list(
        kpi("Sites", nrow(s), sprintf("%d need attention", sum(s$STATUS == "Needs attention"))),
        kpi("Median entry lag", if (length(l)) sprintf("%g days", stats::median(l)) else "\u2013",
            if (length(l)) sprintf("%.0f%% within %g days", 100 * mean(l <= target), target)),
        kpi("Unresolved queries", if (is.null(open)) "\u2013" else nrow(open),
            if (!is.null(open) && nrow(open)) sprintf("oldest %g days", max(open$AGE))),
        kpi("Open findings", if (is.null(s$FINDINGS)) "\u2013" else sum(s$FINDINGS), "from data checks"))))
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
      x$Status <- badge(x$Status)
      x
    })
    picked <- table_card_server("site", tbl, ctx, "Site scorecard", page_length = 20)
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
      p <- picked()
      if (!is.null(p)) q <- q[q$SITEID == p$Site, , drop = FALSE]
      q <- q[order(-q$AGE), , drop = FALSE]
      data.frame(Subject = sid_link(q$SUBJID), Site = q$SITEID, Query = q$RECID, Form = q$QRYFORM,
                 Folder = q$FOLDER, Text = htmltools::htmlEscape(q$QRYTEXT), Status = badge(q$STATUS),
                 Opened = format(q$QRYOPDT), `Age (d)` = q$AGE, check.names = FALSE, stringsAsFactors = FALSE)
    })
    table_card_server("qry", queries, ctx, "Unresolved queries", page_length = 15)
  })
}
