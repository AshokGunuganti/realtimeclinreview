# =============================================================================
#  Safety & PK: AE summary/listing/onset plot, systemic Study drug (DR-009, DR-010)
# =============================================================================

safety_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("kpis")), shiny::br(),
    bslib::layout_columns(fill = FALSE, col_widths = c(6, 6),
      table_card_ui(ns("aesum"), "Adverse events by preferred term"),
      bslib::card(fill = FALSE, bslib::card_header("AE onset by study day"),
        legend_row(c("Treatment-related" = PAL$orange, "Not related" = PAL$blue)),
        plotly::plotlyOutput(ns("p_ae"), height = "360px"),
        shiny::p(class = "note", "Diamond = serious. Hover for term, severity and causality."))),
    bslib::layout_columns(fill = FALSE, col_widths = c(7, 5),
      bslib::card(fill = FALSE, bslib::card_header("Systemic Study drug trough concentrations"),
        plotly::plotlyOutput(ns("p_pk"), height = "320px"),
        shiny::p(class = "note", sprintf("Red = %.1f ng/mL exposure threshold; amber = alert threshold (sidebar); dotted = LLOQ %.1f ng/mL. BLQ plotted at LLOQ/2.",
                                         APP_CFG$pk_limit, APP_CFG$lloq))),
      table_card_ui(ns("pkal"), "PK samples at or above alert threshold")),
    table_card_ui(ns("ael"), "Adverse event listing")
  )
}

safety_server <- function(id, snap, ctrl, registry) {
  shiny::moduleServer(id, function(input, output, session) {
    output$kpis <- shiny::renderUI({
      s <- snap(); a <- s$ae; k <- s$pk
      mx <- if (any(!k$BLQFL)) max(k$AVAL, na.rm = TRUE) else NA
      bslib::layout_columns(fill = FALSE, col_widths = bslib::breakpoints(sm = 6, lg = 3),
        kpi("Subjects with any AE", sprintf("%d / %d", dplyr::n_distinct(a$SUBJID), sum(s$subj$DOSEDFL)), sprintf("%d events", nrow(a))),
        kpi("Treatment-related AEs", sum(a$ARELFL), sprintf("%d subjects", dplyr::n_distinct(a$SUBJID[a$ARELFL]))),
        kpi("Serious AEs", sum(a$ASERFL), sprintf("%d related", sum(a$ASERFL & a$ARELFL)), if (any(a$ASERFL)) "flag" else ""),
        kpi("Max systemic Study drug", if (is.na(mx)) "–" else sprintf("%.2f ng/mL", mx),
            sprintf("%d samples, %d BLQ", nrow(k), sum(k$BLQFL)), if (!is.na(mx) && mx >= ctrl$alert()) "warn" else ""))
    })

    aesum <- shiny::reactive({
      s <- snap(); n <- max(sum(s$subj$DOSEDFL), 1)
      if (!nrow(s$ae)) return(data.frame(`Preferred term` = character(), check.names = FALSE))
      s$ae |> dplyr::group_by(`System organ class` = SOC, `Preferred term` = PT) |>
        dplyr::summarise(Subjects = dplyr::n_distinct(SUBJID), Events = dplyr::n(), Related = sum(ARELFL),
                         Serious = sum(ASERFL), `Max severity` = c("Mild", "Moderate", "Severe")[max(match(ASEV, c("Mild", "Moderate", "Severe")))],
                         .groups = "drop") |>
        dplyr::mutate(`% subj` = round(100 * Subjects / n, 1), .after = Subjects) |>
        dplyr::arrange(dplyr::desc(Subjects), dplyr::desc(Events)) |> as.data.frame(check.names = FALSE)
    })
    table_card_server("aesum", aesum, registry, "AE summary", ctrl$about, empty = "No AEs reported yet.")

    output$p_ae <- plotly::renderPlotly({
      a <- snap()$ae
      shiny::validate(shiny::need(nrow(a) > 0, "No AEs reported yet."))
      a <- a |> dplyr::mutate(y = factor(SUBJID, levels = rev(sort(unique(SUBJID)))),
                              rel = ifelse(ARELFL, "Treatment-related", "Not related"))
      p <- plotly::plot_ly()
      for (g in c("Treatment-related", "Not related")) {
        d <- a |> dplyr::filter(rel == g); if (!nrow(d)) next
        p <- p |> plotly::add_trace(data = d, x = ~ASTDY, y = ~y, type = "scatter", mode = "markers", name = g,
          marker = list(color = if (g == "Treatment-related") PAL$orange else PAL$blue,
                        size = ~ifelse(ASERFL, 13, 9), symbol = ~ifelse(ASERFL, "diamond", "circle"),
                        line = list(color = ~ifelse(ASERFL, PAL$critical, PAL$surface), width = 2)),
          text = ~sprintf("%s<br>%s<br>Day %d | %s | %s%s", SUBJID, PT, ASTDY, ASEV, AEREL, ifelse(ASERFL, " | SERIOUS", "")),
          hoverinfo = "text")
      }
      p |> pl_style("Study day of onset", NULL, legend = FALSE) |>
        plotly::layout(yaxis = list(title = "", tickfont = list(size = 9)))
    })

    output$p_pk <- plotly::renderPlotly({
      k <- snap()$pk
      shiny::validate(shiny::need(nrow(k) > 0, "No PK results yet."))
      wk <- sort(unique(k$VISITNUM)); set.seed(1)
      k <- k |> dplyr::mutate(x = match(VISITNUM, wk) + stats::runif(dplyr::n(), -0.18, 0.18),
                              y = ifelse(BLQFL, LLOQ / 2, AVAL), flag = !BLQFL & AVAL >= ctrl$alert())
      plotly::plot_ly(k, x = ~x, y = ~y, type = "scatter", mode = "markers",
                      marker = list(color = ~ifelse(flag, PAL$warning, PAL$blue), size = ~ifelse(flag, 12, 8),
                                    line = list(color = PAL$surface, width = 2)),
                      text = ~sprintf("%s<br>%s<br>Study drug %s %s", SUBJID, VISIT, PCORRES, PCORRESU), hoverinfo = "text") |>
        pl_style(NULL, "Study drug (ng/mL)", legend = FALSE) |>
        plotly::layout(xaxis = list(tickvals = seq_along(wk), ticktext = unique(k$VISIT[order(k$VISITNUM)]), range = c(0.5, length(wk) + 0.5)),
                       yaxis = list(range = c(0, 2.3)),
                       shapes = list(hline(APP_CFG$pk_limit, PAL$critical, "dash", 2), hline(ctrl$alert(), PAL$warning),
                                     hline(APP_CFG$lloq, PAL$ink3, "dot", 1)))
    })

    pkal <- shiny::reactive({
      k <- snap()$pk |> dplyr::filter(!BLQFL, AVAL >= ctrl$alert()) |> dplyr::arrange(dplyr::desc(AVAL))
      data.frame(Subject = sid_link(k$SUBJID), Site = k$SITEID, Visit = k$VISIT, `Sample date` = fmt_d(k$PCDT),
                 `ng/mL` = k$PCORRES, `% of threshold` = k$PCTLIM, check.names = FALSE)
    })
    table_card_server("pkal", pkal, registry, "PK alerts", ctrl$about, empty = "No samples at or above the alert threshold.")

    ael <- shiny::reactive({
      a <- snap()$ae |> dplyr::arrange(dplyr::desc(ASERFL), SUBJID, AESEQ)
      data.frame(Subject = sid_link(a$SUBJID), Site = a$SITEID, `AE #` = a$AESEQ, `Verbatim term` = a$AETERM,
                 `Preferred term` = a$PT, Onset = fmt_d(a$AESTDT), `Study day` = a$ASTDY, Severity = a$ASEV,
                 Causality = a$AEREL, Serious = ifelse(a$ASERFL, badge("SAE"), "No"), Outcome = a$AEOUT,
                 `Last update` = ifelse(is.na(a$UPDDT), "", paste0(a$UPDNOTE, " (", fmt_d(a$UPDDT), ")")), check.names = FALSE)
    })
    table_card_server("ael", ael, registry, "AE listing", ctrl$about, empty = "No AEs reported yet.")
  })
}
