# Visit status grid: subject by visit, status computed against the review date
# by rtcrcore::visit_status(). Params: form, date, not_done, fallback.

visitgrid_ui <- function(id, params, study) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("kpis")), shiny::br(),
    bslib::card(fill = FALSE,
                bslib::card_header(shiny::span("Visit status by subject"),
                                   shiny::downloadButton(ns("xl"), "Excel", class = "btn-sm", icon = NULL)),
                shiny::p(class = "note", paste("Discontinued: no visits expected after the end of study date.",
                                               "Overdue: past target date plus window and not entered.")),
                DT::DTOutput(ns("grid"), fill = FALSE)),
    table_card_ui(ns("over"), "Overdue visits")
  )
}

visitgrid_server <- function(id, ctx, params) {
  shiny::moduleServer(id, function(input, output, session) {
    status <- shiny::reactive({
      rd <- ctx$data()
      rtcrcore::visit_status(rd$tables, rd$adsl, ctx$study$schedule, ctx$asof(), params$form, params$date,
                             not_done = params$not_done, fallback = params$fallback)
    })
    grid <- shiny::reactive({
      v <- status()
      labels <- unique(v$VISIT[order(v$VISITNUM)])
      subj <- unique(v$SUBJID)
      g <- data.frame(Subject = sid_link(subj), Site = v$SITEID[match(subj, v$SUBJID)], stringsAsFactors = FALSE)
      for (l in labels) {
        r <- v[v$VISIT == l, ]
        g[[l]] <- r$STATUS[match(subj, r$SUBJID)]
      }
      g
    })
    output$kpis <- shiny::renderUI({
      v <- status()
      do.call(bslib::layout_columns, c(list(fill = FALSE, col_widths = bslib::breakpoints(sm = 6, lg = 2)),
        lapply(names(status_colours), function(s) kpi(s, sum(v$STATUS == s)))))
    })
    output$grid <- DT::renderDT({
      g <- grid()
      cols <- setdiff(names(g), c("Subject", "Site"))
      DT::formatStyle(
        DT::datatable(g, escape = FALSE, rownames = FALSE, selection = "none",
                      options = list(pageLength = 25, scrollX = TRUE, dom = "ftip")),
        cols, backgroundColor = DT::styleEqual(names(status_colours), unname(status_colours)))
    })
    output$xl <- shiny::downloadHandler(
      filename = function() paste0("Visit_status_", format(ctx$asof(), "%Y%m%d"), ".xlsx"),
      content = function(file) {
        g <- grid()
        g$Subject <- strip_html(g$Subject)
        write_export(list(`Visit status` = g), file, ctx)
        rtcrcore::log_access(ctx$con, ctx$study$id, ctx$user, "export", basename(file), "visit status")
      })
    overdue <- shiny::reactive({
      v <- status()
      v <- v[v$STATUS == "Overdue / not entered", , drop = FALSE]
      v <- v[order(-v$DAYSOVER), , drop = FALSE]
      data.frame(Subject = sid_link(v$SUBJID), Site = v$SITEID, Visit = v$VISIT, `Target date` = format(v$TARGDT),
                 `Days overdue` = v$DAYSOVER, check.names = FALSE, stringsAsFactors = FALSE)
    })
    table_card_server("over", overdue, ctx, "Overdue visits")
  })
}
