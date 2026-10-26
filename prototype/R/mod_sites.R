# =============================================================================
#  Sites & compliance: site scorecard, entry lag, dosing compliance
#  (DR-006, DR-007, DR-008, DR-014)
# =============================================================================

sites_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("kpis")), shiny::br(),
    table_card_ui(ns("site"), "Site performance",
                  "Entry lag = days from visit to EDC entry (target 5 days). Sorted with sites needing the most attention first."),
    bslib::layout_columns(fill = FALSE, col_widths = c(6, 6),
      bslib::card(fill = FALSE, bslib::card_header("Median data entry lag by site"),
        plotly::plotlyOutput(ns("p_lag"), height = "380px"),
        shiny::p(class = "note", "Dashed line = 5-day entry target. Hover for the 90th percentile.")),
      bslib::card(fill = FALSE, bslib::card_header("Dosing compliance by subject (latest diary interval)"),
        legend_row(c("At or above threshold" = PAL$blue, "Below threshold" = PAL$warning)),
        plotly::plotlyOutput(ns("p_comp"), height = "360px"),
        shiny::p(class = "note", "Applications recorded in the dosing diary as a percent of prescribed once-daily applications."))),
    table_card_ui(ns("low"), "Subjects below compliance threshold")
  )
}

sites_server <- function(id, snap, ctrl, registry) {
  shiny::moduleServer(id, function(input, output, session) {
    ss <- shiny::reactive(site_summary(snap(), ctrl$cthr()))
    output$kpis <- shiny::renderUI({
      s <- snap(); r <- ss(); lags <- s$visit$ENTLAG[!is.na(s$visit$ENTLAG)]; lc <- s$lastcomp
      bslib::layout_columns(fill = FALSE, col_widths = bslib::breakpoints(sm = 6, lg = c(2, 2, 3, 3, 2)),
        kpi("Active sites", nrow(r), sprintf("%d need attention", sum(r$STATUS == "Needs attention")),
            if (any(r$STATUS == "Needs attention")) "warn" else ""),
        kpi("Median entry lag", if (length(lags)) sprintf("%g days", stats::median(lags)) else "–",
            sprintf("target ≤ %d days", APP_CFG$lag_target)),
        kpi("Entered within target", if (length(lags)) sprintf("%.0f%%", 100 * mean(lags <= APP_CFG$lag_target)) else "–",
            sprintf("%d visits entered", length(lags))),
        kpi("Mean dosing compliance", if (nrow(lc)) sprintf("%.0f%%", mean(lc$COMPPCT)) else "–",
            sprintf("latest interval, %d subjects", nrow(lc))),
        kpi(sprintf("Below %d%%", ctrl$cthr()), sum(lc$COMPPCT < ctrl$cthr()), "subjects, latest interval",
            if (any(lc$COMPPCT < ctrl$cthr())) "warn" else ""))
    })
    st <- shiny::reactive({
      r <- ss()
      data.frame(Site = r$SITEID, Status = badge(r$STATUS), Enrolled = r$ENROLLED, Dosed = r$DOSED, `Visits due` = r$VISDUE,
                 `% complete in window` = r$PCTWIN, Overdue = r$OVERDUE, Missed = r$MISSED, `Median entry lag (d)` = r$LAGMED,
                 `% entered ≤ 5 d` = r$PCTLAG, `Queries / 10 visits` = r$QRATE, `Unresolved queries` = r$OPENQ,
                 `Mean dosing %` = r$COMPMEAN, `Subjects below threshold` = r$LOWCOMP, check.names = FALSE)
    })
    table_card_server("site", st, registry, "Site performance", ctrl$about, page_length = 15)

    output$p_lag <- plotly::renderPlotly({
      r <- ss() |> dplyr::filter(!is.na(LAGMED)) |> dplyr::arrange(LAGMED)
      shiny::validate(shiny::need(nrow(r) > 0, "No visits entered yet."))
      plotly::plot_ly(r, y = ~factor(SITEID, levels = SITEID), x = ~LAGMED, type = "bar", orientation = "h",
                      marker = list(color = ~ifelse(LAGMED > APP_CFG$lag_target, PAL$serious, PAL$blue)),
                      text = ~sprintf("Site %s<br>Median %g days<br>90th pct %g days<br>%g%% within target", SITEID, LAGMED, LAGP90, PCTLAG),
                      hoverinfo = "text", textposition = "none") |>
        pl_style("Median days from visit to EDC entry", "Site", legend = FALSE) |>
        plotly::layout(shapes = list(vline(APP_CFG$lag_target, PAL$critical)), bargap = 0.3)
    })

    output$p_comp <- plotly::renderPlotly({
      lc <- snap()$lastcomp
      shiny::validate(shiny::need(nrow(lc) > 0, "No diary data yet."))
      lc <- lc |> dplyr::mutate(low = COMPPCT < ctrl$cthr())
      plotly::plot_ly(lc, x = ~COMPPCT, y = ~SITEID, type = "scatter", mode = "markers",
                      marker = list(color = ~ifelse(low, PAL$warning, PAL$blue), size = ~ifelse(low, 12, 9),
                                    line = list(color = PAL$surface, width = 1.5)),
                      text = ~sprintf("%s<br>%d%% through %s<br>%d of %d doses missed", SUBJID, as.integer(COMPPCT), VISIT, as.integer(DOSMISS), as.integer(DOSPRESC)),
                      hoverinfo = "text", customdata = ~SUBJID) |>
        pl_style("Diary compliance, latest interval (%)", "Site", legend = FALSE) |>
        plotly::layout(xaxis = list(range = c(40, 102)), shapes = list(vline(ctrl$cthr(), PAL$warning)))
    })

    low <- shiny::reactive({
      d <- low_compliance(snap(), ctrl$cthr())
      if (!nrow(d)) return(data.frame(Subject = character()))
      data.frame(Subject = sid_link(d$SUBJID), Site = d$SITEID, `Through visit` = d$VISIT, `Latest %` = d$COMPPCT,
                 `Doses missed` = sprintf("%d of %d", as.integer(d$DOSMISS), as.integer(d$DOSPRESC)),
                 `Mean % all intervals` = d$MEANPCT, `Change vs prior` = ifelse(is.na(d$DELTA), "–", sprintf("%+d", as.integer(d$DELTA))),
                 check.names = FALSE)
    })
    table_card_server("low", low, registry, "Low compliance", ctrl$about, empty = "No subjects below the threshold.")
  })
}
