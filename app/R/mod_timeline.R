# Event timeline: log-form events by study day, one row per subject, coloured
# by a chosen field. Params: form, day, label, color, title.

timeline_ui <- function(id, params, study) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::card(fill = FALSE, bslib::card_header(if (is.null(params$title)) params$form else params$title),
                shiny::p(class = "note", "Study day from first dose (no day 0). Click a point to open the subject."),
                plotly::plotlyOutput(ns("plot"), height = "520px"))
  )
}

timeline_server <- function(id, ctx, params) {
  shiny::moduleServer(id, function(input, output, session) {
    events <- shiny::reactive({
      t <- ctx$data()$tables[[params$form]]
      d <- data.frame(SUBJID = t$SUBJID, DAY = as.numeric(t[[params$day]]), LABEL = as.character(t[[params$label]]),
                      COLOR = if (is.null(params$color)) params$form else as.character(t[[params$color]]),
                      stringsAsFactors = FALSE)
      d$COLOR[is.na(d$COLOR)] <- "Missing"
      d[!is.na(d$DAY), , drop = FALSE]
    })
    output$plot <- plotly::renderPlotly({
      d <- events()
      p <- plotly::plot_ly(d, x = ~DAY, y = ~SUBJID, color = ~COLOR, type = "scatter", mode = "markers",
                           marker = list(size = 9), text = ~paste0(SUBJID, ": ", LABEL, " (day ", DAY, ")"),
                           hoverinfo = "text", customdata = ~SUBJID, source = session$ns("tl"))
      p <- plotly::layout(p, xaxis = list(title = "Study day", zeroline = TRUE),
                          yaxis = list(title = "", type = "category", categoryorder = "category descending"),
                          legend = list(orientation = "h"))
      plotly::event_register(p, "plotly_click")
    })
    # Read the click input plotly's JavaScript sets, not event_data(): in this
    # module event_data() warned "event not registered" on every flush, even
    # after the plot had drawn.
    click_id <- paste0("plotly_click-", session$ns("tl"))
    shiny::observeEvent(session$rootScope()$input[[click_id]], {
      ev <- jsonlite::parse_json(session$rootScope()$input[[click_id]], simplifyVector = TRUE)
      if (!is.null(ev$customdata)) ctx$goto_subject(ev$customdata[[1]])
    }, ignoreInit = TRUE)
  })
}
