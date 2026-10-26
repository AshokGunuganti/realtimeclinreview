# =============================================================================
#  Data quality: visit status grid, query aging, issue listing (DR-001, DR-016)
# =============================================================================

quality_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("kpis")), shiny::br(),
    bslib::layout_columns(fill = FALSE, col_widths = c(7, 5),
      bslib::card(fill = FALSE, bslib::card_header("Visit status by subject"),
        legend_row(as.list(STATUS_COLS)),
        plotly::plotlyOutput(ns("p_status"), height = "auto")),
      bslib::card(fill = FALSE, bslib::card_header("Unresolved queries by site and age"),
        plotly::plotlyOutput(ns("p_q"), height = "330px"),
        shiny::p(class = "note", "Open plus answered-but-not-closed queries, by days since opened."))),
    table_card_ui(ns("iss"), "Issue listing"),
    shiny::div(style = "display:none", table_card_ui(ns("vs"), "Visit status"))
  )
}

quality_server <- function(id, snap, ctrl, registry) {
  shiny::moduleServer(id, function(input, output, session) {
    output$kpis <- shiny::renderUI({
      s <- snap(); v <- s$visit |> dplyr::filter(!STATUS %in% c("Not yet due", "Discontinued"))
      op <- s$qry |> dplyr::filter(STATUS != "Closed")
      bslib::layout_columns(fill = FALSE, col_widths = bslib::breakpoints(sm = 6, lg = 3),
        kpi("Visits complete in window", if (nrow(v)) sprintf("%.0f%%", 100 * mean(v$STATUS == "Complete")) else "–",
            sprintf("%d of %d due", sum(v$STATUS == "Complete"), nrow(v))),
        kpi("Overdue / not entered", sum(v$STATUS == "Overdue / not entered"), sprintf("%d missed", sum(v$STATUS == "Missed")),
            if (any(v$STATUS == "Overdue / not entered")) "warn" else ""),
        kpi("Unresolved queries", nrow(op), sprintf("%d open, %d answered", sum(op$STATUS == "Open"), sum(op$STATUS == "Answered"))),
        kpi("Median query age", if (nrow(op)) sprintf("%.0f days", stats::median(op$AGEDAYS)) else "–",
            sprintf("%d older than %d days", sum(op$AGEDAYS > APP_CFG$query_old_days), APP_CFG$query_old_days)))
    })

    output$p_status <- plotly::renderPlotly({
      v <- snap()$visit
      shiny::validate(shiny::need(nrow(v) > 0, "No subjects."))
      ids <- sort(unique(v$SUBJID)); vis <- unique(v$VISIT[order(v$VISITNUM)])
      z <- matrix(NA, length(ids), length(vis)); txt <- matrix("", length(ids), length(vis))
      ix <- cbind(match(v$SUBJID, ids), match(v$VISIT, vis))
      z[ix] <- match(v$STATUS, names(STATUS_COLS))
      txt[ix] <- sprintf("%s<br>%s<br>%s%s", v$SUBJID, v$VISIT, v$STATUS,
                         ifelse(is.na(v$DAYSOVER), ifelse(v$STATUS == "Out of window", sprintf(" (%+d days)", v$VISOFF), ""),
                                sprintf(" (%d days)", v$DAYSOVER)))
      k <- length(STATUS_COLS); cs <- list()
      for (i in seq_len(k)) { cs[[length(cs) + 1]] <- list((i - 1) / k, unname(STATUS_COLS[i])); cs[[length(cs) + 1]] <- list(i / k, unname(STATUS_COLS[i])) }
      plotly::plot_ly(x = vis, y = ids, z = z, type = "heatmap", text = txt, hoverinfo = "text", colorscale = cs,
                      zmin = 0.5, zmax = k + 0.5, xgap = 2, ygap = 2, showscale = FALSE, height = max(320, 18 * length(ids) + 80)) |>
        pl_style(NULL, NULL, legend = FALSE) |>
        plotly::layout(yaxis = list(autorange = "reversed", tickfont = list(size = 9), showgrid = FALSE),
                       xaxis = list(side = "top", tickangle = -30, tickfont = list(size = 10), showgrid = FALSE),
                       margin = list(t = 60))
    })

    output$p_q <- plotly::renderPlotly({
      q <- snap()$qry |> dplyr::filter(STATUS != "Closed")
      shiny::validate(shiny::need(nrow(q) > 0, "No unresolved queries."))
      lev <- c("0-7 days", "8-14 days", "15-30 days", "> 30 days")
      d <- q |> dplyr::mutate(b = base::cut(AGEDAYS, c(-Inf, 7, 14, 30, Inf), labels = lev)) |>
        dplyr::count(SITEID, b) |> tidyr::complete(SITEID, b, fill = list(n = 0L))
      ord <- d |> dplyr::group_by(SITEID) |> dplyr::summarise(t = sum(n)) |> dplyr::arrange(t) |> dplyr::pull(SITEID)
      p <- plotly::plot_ly()
      for (i in seq_along(lev)) {
        dd <- d |> dplyr::filter(b == lev[i])
        p <- p |> plotly::add_bars(data = dd, y = ~factor(SITEID, levels = ord), x = ~n, orientation = "h", name = lev[i],
                                   marker = list(color = PAL$seq[i], line = list(color = PAL$surface, width = 2)),
                                   text = ~sprintf("Site %s<br>%s: %d", SITEID, lev[i], n), hoverinfo = "text", textposition = "none")
      }
      p |> pl_style("Unresolved queries", "Site") |> plotly::layout(barmode = "stack", legend = list(traceorder = "normal"))
    })

    iss <- shiny::reactive({
      d <- issue_listing(snap())
      data.frame(Type = badge(d$TYPE), Subject = sid_link(d$SUBJID), Site = d$SITEID, Item = d$ITEM,
                 Detail = htmltools::htmlEscape(d$DETAIL), Days = d$DAYS, check.names = FALSE)
    })
    table_card_server("iss", iss, registry, "Issues", ctrl$about, empty = "No open issues.")
    vs <- shiny::reactive({
      snap()$visit |> dplyr::select(Subject = SUBJID, Site = SITEID, VISITNUM, VISIT, STATUS) |>
        dplyr::arrange(VISITNUM) |> dplyr::select(-VISITNUM) |>
        tidyr::pivot_wider(names_from = VISIT, values_from = STATUS) |> dplyr::arrange(Subject) |> as.data.frame(check.names = FALSE)
    })
    table_card_server("vs", vs, registry, "Visit status", ctrl$about)
  })
}
