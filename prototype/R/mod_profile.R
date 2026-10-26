# =============================================================================
#  Subject profile: one patient across visits, efficacy, dosing, AEs, PK, queries
# =============================================================================

profile_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(style = "display:flex;gap:.5rem;align-items:center;flex-wrap:wrap;margin-bottom:.8rem",
      shiny::actionButton(ns("prev"), "‹", class = "btn-outline-secondary btn-sm"),
      shiny::div(style = "width:200px;margin-bottom:-1rem", shiny::selectInput(ns("subj"), NULL, choices = NULL)),
      shiny::actionButton(ns("nxt"), "›", class = "btn-outline-secondary btn-sm"),
      shiny::uiOutput(ns("note"), inline = TRUE)),
    shiny::uiOutput(ns("kpis")), shiny::br(),
    bslib::layout_columns(fill = FALSE, col_widths = c(6, 6),
      bslib::card(fill = FALSE, bslib::card_header("Severity (MCSS) and IGA over time"),
        plotly::plotlyOutput(ns("p_m"), height = "300px"),
        shiny::p(class = "note", "Line = blinded MCSS (lower is better). Labels = investigator IGA at each visit.")),
      bslib::card(fill = FALSE, bslib::card_header("Dosing compliance by diary interval"),
        plotly::plotlyOutput(ns("p_c"), height = "300px"),
        shiny::p(class = "note", "Dashed line = compliance threshold (sidebar)."))),
    table_card_ui(ns("vis"), "Visits"),
    bslib::layout_columns(fill = FALSE, col_widths = c(6, 6),
      table_card_ui(ns("ae"), "Adverse events"),
      shiny::div(table_card_ui(ns("pk"), "Study drug PK"), table_card_ui(ns("q"), "Queries")))
  )
}

profile_server <- function(id, snap, ctrl, selected) {
  shiny::moduleServer(id, function(input, output, session) {
    ids <- shiny::reactive(sort(snap()$subj$SUBJID))
    shiny::observe({
      i <- ids(); cur <- selected()
      if (!length(i)) return()
      if (is.null(cur) || !cur %in% i) { cur <- i[1]; selected(cur) }
      shiny::updateSelectInput(session, "subj", choices = i, selected = cur)
    })
    shiny::observeEvent(input$subj, if (nzchar(input$subj) && !identical(input$subj, selected())) selected(input$subj), ignoreInit = TRUE)
    step <- function(d) { i <- ids(); k <- match(selected(), i); selected(i[(k - 1 + d) %% length(i) + 1]) }
    shiny::observeEvent(input$prev, step(-1)); shiny::observeEvent(input$nxt, step(1))

    one <- shiny::reactive({
      s <- snap(); id <- selected(); shiny::req(id, id %in% s$subj$SUBJID)
      list(id = id, sj = s$subj[s$subj$SUBJID == id, ], v = s$visit[s$visit$SUBJID == id, ] |> dplyr::arrange(VISITNUM),
           ae = s$ae[s$ae$SUBJID == id, ], pk = s$pk[s$pk$SUBJID == id, ], q = s$qry[s$qry$SUBJID == id, ], asof = s$asof)
    })
    output$note <- shiny::renderUI({
      o <- one(); st <- if (o$sj$WDFL) "Withdrawn" else if (o$sj$DOSEDFL) "Ongoing" else "Run-in"
      shiny::span(shiny::HTML(badge(st)), shiny::span(class = "note", sprintf(" %d of %d subjects in current filter", match(o$id, ids()), length(ids()))))
    })
    output$kpis <- shiny::renderUI({
      o <- one(); sj <- o$sj; ent <- o$v |> dplyr::filter(ENTERED)
      bl <- ent |> dplyr::filter(NOMWK == 0); lt <- ent |> dplyr::filter(NOMWK > 0, !is.na(IGA)) |> utils::tail(1)
      cp <- ent |> dplyr::filter(!is.na(COMPPCT))
      pch <- if (nrow(bl) && nrow(lt)) round(100 * (lt$MCSS - bl$MCSS) / bl$MCSS) else NA
      bslib::layout_columns(fill = FALSE, col_widths = bslib::breakpoints(sm = 6, lg = c(2, 3, 3, 2, 2)),
        kpi("Site / age / sex", sj$SITEID, sprintf("%g y, %s%s", sj$AGE, if (sj$SEX == "F") "female" else "male", if (sj$ITTFL) "" else " | not in ITT")),
        kpi("First dose", if (sj$DOSEDFL) format(sj$TRTSDT, "%d %b %Y") else "Not dosed",
            if (sj$DOSEDFL) sprintf("Study day %d at review date", as.integer(o$asof - sj$TRTSDT + 1)) else if (sj$WDFL) "withdrew during run-in" else "in run-in"),
        kpi("Latest IGA", if (nrow(lt)) sprintf("%+d", as.integer(lt$IGA)) else "–",
            if (nrow(lt)) sprintf("%s; MCSS %+g%% vs baseline", lt$VISIT, pch) else "no post-baseline visit yet",
            if (nrow(lt) && lt$IGA < 0) "flag" else ""),
        kpi("Dosing compliance", if (nrow(cp)) sprintf("%.0f%%", mean(cp$COMPPCT)) else "–",
            if (nrow(cp)) sprintf("latest %d%%", as.integer(utils::tail(cp$COMPPCT, 1))) else "no diary data yet",
            if (nrow(cp) && utils::tail(cp$COMPPCT, 1) < ctrl$cthr()) "warn" else ""),
        kpi("AEs / open queries", sprintf("%d / %d", nrow(o$ae), sum(o$q$STATUS != "Closed")),
            sprintf("%d serious, %d related", sum(o$ae$ASERFL), sum(o$ae$ARELFL)), if (any(o$ae$ASERFL)) "flag" else ""))
    })
    output$p_m <- plotly::renderPlotly({
      e <- one()$v |> dplyr::filter(ENTERED, !is.na(MCSS))
      shiny::validate(shiny::need(nrow(e) > 0, "No assessments entered yet."))
      bl <- e$MCSS[e$NOMWK == 0]
      plotly::plot_ly(e, x = ~NOMWK, y = ~MCSS, type = "scatter", mode = "lines+markers+text",
                      text = ~ifelse(is.na(IGA), "", sprintf("%+d", as.integer(IGA))), textposition = "top center",
                      textfont = list(size = 11, color = ~ifelse(!is.na(IGA) & IGA < 0, PAL$critical, PAL$ink2)),
                      line = list(color = PAL$blue, width = 2.5), marker = list(color = PAL$blue, size = 9, line = list(color = PAL$surface, width = 2)),
                      hovertext = ~sprintf("%s (%s)<br>MCSS %.1f%s%s", VISIT, format(VISDT, "%d %b %Y"), MCSS,
                                           ifelse(is.na(IGA), "", sprintf("<br>IGA %+d", as.integer(IGA))),
                                           ifelse(is.na(COMPPCT), "", sprintf("<br>Dosing %d%%", as.integer(COMPPCT)))),
                      hoverinfo = "text") |>
        pl_style("Study week", "MCSS (blinded)", legend = FALSE) |>
        plotly::layout(xaxis = list(tickvals = c(-8, 0, 4, 8, 12, 16, 20, 24, 36, 48), range = c(-10, 50)), yaxis = list(range = c(0, 21)),
                       shapes = c(list(list(type = "rect", xref = "x", yref = "paper", x0 = -8, x1 = 0, y0 = 0, y1 = 1,
                                            fillcolor = "rgba(42,120,214,0.07)", line = list(width = 0), layer = "below")),
                                  if (length(bl)) list(hline(bl, PAL$ink3, "dot", 1)) else list()))
    })
    output$p_c <- plotly::renderPlotly({
      cp <- one()$v |> dplyr::filter(ENTERED, !is.na(COMPPCT))
      shiny::validate(shiny::need(nrow(cp) > 0, "No diary intervals reconciled yet."))
      cp$low <- cp$COMPPCT < ctrl$cthr()
      plotly::plot_ly(cp, x = ~factor(VISSHORT, levels = VISSHORT), y = ~COMPPCT, type = "bar",
                      marker = list(color = ~ifelse(low, PAL$warning, PAL$blue)),
                      text = ~COMPPCT, textposition = "outside",
                      hovertext = ~sprintf("%s<br>%d%% of %d days<br>%d doses missed", VISIT, as.integer(COMPPCT), as.integer(DOSPRESC), as.integer(DOSMISS)),
                      hoverinfo = "text") |>
        pl_style("Diary interval ending at visit", "Doses recorded (%)", legend = FALSE) |>
        plotly::layout(yaxis = list(range = c(0, 110)), shapes = list(hline(ctrl$cthr(), PAL$warning)), bargap = 0.3)
    })
    vt <- shiny::reactive({ v <- one()$v
      data.frame(Visit = v$VISIT, Target = fmt_d(v$TARGDT), Actual = ifelse(v$ENTERED, fmt_d(v$VISDT), "–"),
                 Status = badge(v$STATUS), `Entry lag (d)` = dash(v$ENTLAG), MCSS = dash(v$MCSS, function(x) sprintf("%.1f", x)),
                 IGA = dash(v$IGA, function(x) sprintf("%+d", as.integer(x))), `Dosing %` = dash(v$COMPPCT),
                 `Doses missed` = ifelse(is.na(v$COMPPCT), "–", sprintf("%d of %d", as.integer(v$DOSMISS), as.integer(v$DOSPRESC))),
                 check.names = FALSE) })
    table_card_server("vis", vt, page_length = 12)
    at <- shiny::reactive({ a <- one()$ae
      data.frame(`AE #` = a$AESEQ, `Preferred term` = a$PT, Onset = fmt_d(a$AESTDT), Day = a$ASTDY, Severity = a$ASEV,
                 Causality = a$AEREL, Serious = ifelse(a$ASERFL, badge("SAE"), "No"), Outcome = a$AEOUT, check.names = FALSE) })
    table_card_server("ae", at, empty = "No AEs reported.")
    pt <- shiny::reactive({ k <- one()$pk
      data.frame(Visit = k$VISIT, `Sample date` = fmt_d(k$PCDT), Result = paste(htmltools::htmlEscape(k$PCORRES), k$PCORRESU),
                 `% of threshold` = dash(k$PCTLIM), check.names = FALSE) })
    table_card_server("pk", pt, empty = "No PK results yet.")
    qt <- shiny::reactive({ q <- one()$q
      data.frame(Query = q$QRYID, Item = q$ITEM, Text = q$QRYTEXT, Opened = fmt_d(q$OPENDT), Status = badge(q$STATUS),
                 `Age (d)` = dash(q$AGEDAYS), check.names = FALSE) })
    table_card_server("q", qt, empty = "No queries.")
  })
}
