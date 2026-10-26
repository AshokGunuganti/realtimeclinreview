# =============================================================================
#  Efficacy: IGA mean/CI, MCSS trajectories, IGA distribution, subject review
#  (DR-005, DR-012, DR-013)
# =============================================================================

efficacy_ui <- function(id, sched) {
  ns <- shiny::NS(id)
  post <- sched[sched$NOMWK > 0, ]
  shiny::tagList(
    shiny::uiOutput(ns("kpis")), shiny::br(),
    bslib::layout_columns(fill = FALSE, col_widths = c(6, 6),
      bslib::card(fill = FALSE, bslib::card_header("IGA by visit: mean and 95% CI"),
        plotly::plotlyOutput(ns("p_iga"), height = "320px"),
        shiny::p(class = "note", "7-point scale, -3 (very much worse) to +3 (very much improved). Visits with n < 3 not plotted.")),
      bslib::card(fill = FALSE, bslib::card_header("Blinded severity (MCSS) through run-in and treatment"),
        plotly::plotlyOutput(ns("p_mcss"), height = "320px"),
        shiny::p(class = "note", "Lower is better. Shaded band = 8-week run-in; a stable run-in supports the baseline-controlled design."))),
    bslib::layout_columns(fill = FALSE, col_widths = c(4, 8),
      bslib::card(fill = FALSE, bslib::card_header("IGA distribution"),
        shiny::selectInput(ns("vis"), NULL, stats::setNames(post$VISITNUM, post$VISIT), selected = 8),
        plotly::plotlyOutput(ns("p_dist"), height = "260px")),
      table_card_ui(ns("subj"), "Subject-level review",
                    "Click a row to highlight the subject in the severity plot; click the ID to open the profile."))
  )
}

efficacy_server <- function(id, snap, ctrl, registry) {
  shiny::moduleServer(id, function(input, output, session) {
    eff <- shiny::reactive(snap()$visit |> dplyr::filter(ENTERED, ITTFL))
    output$kpis <- shiny::renderUI({
      e <- eff(); w24 <- e |> dplyr::filter(NOMWK == 24)
      b <- e |> dplyr::filter(NOMWK %in% c(-8, 0)) |> dplyr::select(SUBJID, NOMWK, MCSS) |>
        tidyr::pivot_wider(names_from = NOMWK, values_from = MCSS, names_prefix = "w")
      drift <- if (all(c("w-8", "w0") %in% names(b))) stats::median(abs(b$w0 - b$`w-8`), na.rm = TRUE) else NA
      bslib::layout_columns(fill = FALSE, col_widths = bslib::breakpoints(sm = 6, lg = 3),
        kpi("ITT subjects with data", dplyr::n_distinct(e$SUBJID)),
        kpi("Reached Week 24", nrow(w24)),
        kpi("Week 24 mean IGA", if (nrow(w24)) sprintf("%+.2f", mean(w24$IGA)) else "–",
            if (nrow(w24)) sprintf("%.0f%% much/very much improved", 100 * mean(w24$IGA >= 2))),
        kpi("Run-in drift (median)", if (is.na(drift)) "–" else sprintf("%.1f", drift), "|MCSS change|, Wk -8 to Day 1"))
    })

    output$p_iga <- plotly::renderPlotly({
      d <- visit_mean_ci(snap(), "IGA")
      shiny::validate(shiny::need(nrow(d) > 0, "No post-baseline IGA data yet."))
      plotly::plot_ly(d, x = ~NOMWK, y = ~MEAN, type = "scatter", mode = "lines+markers",
                      line = list(color = PAL$blue, width = 2),
                      marker = list(color = PAL$blue, size = 8, line = list(color = PAL$surface, width = 2)),
                      error_y = list(type = "data", symmetric = FALSE, array = ~UCL - MEAN, arrayminus = ~MEAN - LCL,
                                     color = PAL$blue, thickness = 1.2, width = 4),
                      text = ~sprintf("%s<br>Mean %+.2f (95%% CI %+.2f to %+.2f)<br>n = %d", VISIT, MEAN, LCL, UCL, N),
                      hoverinfo = "text") |>
        plotly::add_annotations(x = d$NOMWK, y = d$UCL, text = paste0("n=", d$N), yshift = 12, showarrow = FALSE,
                                font = list(size = 10, color = PAL$ink3)) |>
        pl_style("Study week", "Mean IGA", legend = FALSE) |>
        plotly::layout(yaxis = list(range = c(-1, 3.4), zeroline = TRUE, zerolinecolor = "#c9c8c2"),
                       xaxis = list(tickvals = d$NOMWK))
    })

    hl <- shiny::reactiveVal(NULL)
    output$p_mcss <- plotly::renderPlotly({
      e <- eff() |> dplyr::filter(!is.na(MCSS)) |> dplyr::arrange(SUBJID, NOMWK)
      shiny::validate(shiny::need(nrow(e) > 0, "No efficacy data yet."))
      mn <- visit_mean_ci(snap(), "MCSS", post_only = FALSE)
      p <- plotly::plot_ly() |>
        plotly::add_lines(data = dplyr::group_by(e, SUBJID), x = ~NOMWK, y = ~MCSS,
                          line = list(color = "rgba(138,137,131,0.28)", width = 1),
                          text = ~sprintf("%s<br>%s<br>MCSS %.1f", SUBJID, VISIT, MCSS), hoverinfo = "text",
                          name = "Individual subjects") |>
        plotly::add_trace(data = mn, x = ~NOMWK, y = ~MEAN, type = "scatter", mode = "lines+markers",
                          line = list(color = PAL$blue, width = 2.5),
                          marker = list(color = PAL$blue, size = 8, line = list(color = PAL$surface, width = 2)),
                          text = ~sprintf("%s<br>Mean MCSS %.1f (n=%d)", VISIT, MEAN, N), hoverinfo = "text", name = "Mean")
      if (!is.null(hl())) {
        ss <- e |> dplyr::filter(SUBJID == hl())
        p <- p |> plotly::add_trace(data = ss, x = ~NOMWK, y = ~MCSS, type = "scatter", mode = "lines+markers",
                                    line = list(color = PAL$orange, width = 2.5),
                                    marker = list(color = PAL$orange, size = 8, line = list(color = PAL$surface, width = 2)),
                                    text = ~sprintf("%s<br>%s<br>MCSS %.1f", SUBJID, VISIT, MCSS), hoverinfo = "text", name = hl())
      }
      p |> pl_style("Study week", "MCSS (blinded)") |>
        plotly::layout(xaxis = list(tickvals = c(-8, 0, 4, 8, 12, 16, 20, 24, 36, 48)),
          shapes = list(list(type = "rect", xref = "x", yref = "paper", x0 = -8, x1 = 0, y0 = 0, y1 = 1,
                             fillcolor = "rgba(42,120,214,0.07)", line = list(width = 0), layer = "below"),
                        vline(0, PAL$ink3, "dot", 1)),
          annotations = list(list(x = -4, y = 1, xref = "x", yref = "paper", text = "Run-in", showarrow = FALSE, yanchor = "bottom",
                                  font = list(size = 10, color = PAL$ink2)),
                             list(x = 0, y = 1, xref = "x", yref = "paper", text = "First dose", showarrow = FALSE, xanchor = "left",
                                  yanchor = "bottom", font = list(size = 10, color = PAL$ink2))))
    })

    output$p_dist <- plotly::renderPlotly({
      at <- eff() |> dplyr::filter(VISITNUM == as.numeric(input$vis), !is.na(IGA))
      shiny::validate(shiny::need(nrow(at) > 0, "No data at this visit yet."))
      d <- data.frame(IGA = -3:3) |> dplyr::left_join(dplyr::count(at, IGA), by = "IGA") |>
        dplyr::mutate(n = dplyr::coalesce(n, 0L), lab = IGA_LABELS[as.character(IGA)])
      plotly::plot_ly(d, x = ~factor(sprintf("%+d", IGA), levels = sprintf("%+d", -3:3)), y = ~n, type = "bar",
                      marker = list(color = PAL$blue, line = list(color = PAL$surface, width = 2)),
                      text = ~sprintf("%+d %s<br>%d subjects (%.0f%%)", IGA, lab, n, 100 * n / sum(n)),
                      hoverinfo = "text", textposition = "none") |>
        pl_style("IGA score", "Subjects", legend = FALSE) |> plotly::layout(bargap = 0.25)
    })

    tbl <- shiny::reactive({
      d <- efficacy_subjects(snap())
      if (!nrow(d)) return(data.frame(Subject = character()))
      data.frame(Subject = sid_link(d$SUBJID), Site = d$SITEID, `Latest visit` = d$VISIT, IGA = sprintf("%+d", as.integer(d$IGA)),
                 `MCSS run-in` = d$MCSS_RI, `MCSS baseline` = d$MCSS_BL, `MCSS latest` = d$MCSS, `% change` = d$PCHG,
                 `Dosing %` = d$COMPPCT, Flag = badge(d$FLAG), check.names = FALSE)
    })
    sel <- table_card_server("subj", tbl, registry, "Efficacy by subject", ctrl$about, page_length = 8, selection = "single")
    shiny::observe({
      r <- sel()
      hl(if (is.null(r)) NULL else strip_html(r$Subject))
    })
  })
}
