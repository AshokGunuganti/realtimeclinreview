# Subject profile: one subject across every form, with reference dates and
# status from ADSL and a timeline anchored on first dose.
# Params: timeline (list of {form, day, label}), forms (forms to list; default all).

profile_ui <- function(id, params, study) {
  ns <- shiny::NS(id)
  forms <- profile_forms(params, study)
  shiny::tagList(
    shiny::selectInput(ns("subject"), "Subject", choices = NULL, width = "240px"),
    bslib::card(fill = FALSE, bslib::card_header("Subject"), shiny::uiOutput(ns("header"))),
    if (length(params$timeline)) bslib::card(fill = FALSE, bslib::card_header("Timeline (study day)"),
                                             plotly::plotlyOutput(ns("plot"), height = "260px")),
    lapply(forms, function(f) bslib::card(fill = FALSE, bslib::card_header(f), DT::DTOutput(ns(paste0("t_", f)), fill = FALSE)))
  )
}

profile_server <- function(id, ctx, params) {
  shiny::moduleServer(id, function(input, output, session) {
    forms <- profile_forms(params, ctx$study)
    shiny::observe({
      ids <- ctx$data()$adsl$SUBJID
      sel <- shiny::isolate(input$subject)
      want <- ctx$selected_subject()
      pick <- if (!is.null(want) && want %in% ids) want else if (!is.null(sel) && sel %in% ids) sel else ids[1]
      shiny::updateSelectInput(session, "subject", choices = ids, selected = pick)
    })
    subject <- shiny::reactive(shiny::req(input$subject))
    output$header <- shiny::renderUI({
      a <- ctx$data()$adsl
      a <- a[a$SUBJID == subject(), , drop = FALSE]
      if (!nrow(a)) return(shiny::p(class = "note", "Subject not available on this review date."))
      vals <- vapply(a, function(v) { x <- fmt_cell(v)[1]; if (is.na(x)) "" else as.character(x) }, "")
      vals <- vals[!grepl("SRC$", names(vals)) & names(vals) != "STUDYID"]
      shiny::tags$table(class = "table table-sm", style = "width:auto",
                        lapply(names(vals), function(n) shiny::tags$tr(shiny::tags$th(n), shiny::tags$td(vals[[n]]))))
    })
    for (f in forms) local({
      form <- f
      output[[paste0("t_", form)]] <- DT::renderDT({
        t <- ctx$data()$tables[[form]]
        t <- t[t$SUBJID == subject(), setdiff(names(t), c("STUDYID", "SITEID", "SUBJID", "FORMOID")), drop = FALSE]
        t[] <- lapply(t, fmt_cell)
        DT::datatable(t, rownames = FALSE, selection = "none",
                      options = list(dom = "t", pageLength = 100, scrollX = TRUE,
                                     language = list(emptyTable = "No records.")))
      })
    })
    output$plot <- plotly::renderPlotly({
      ev <- do.call(rbind, lapply(params$timeline, function(it) {
        t <- ctx$data()$tables[[it$form]]
        t <- t[t$SUBJID == subject(), , drop = FALSE]
        if (!nrow(t) || !it$day %in% names(t)) return(NULL)
        data.frame(FORM = it$form, DAY = as.numeric(t[[it$day]]), LABEL = as.character(t[[it$label]]),
                   stringsAsFactors = FALSE)
      }))
      if (is.null(ev)) ev <- data.frame(FORM = character(), DAY = numeric(), LABEL = character())
      ev <- ev[!is.na(ev$DAY), , drop = FALSE]
      p <- plotly::plot_ly(ev, x = ~DAY, y = ~FORM, type = "scatter", mode = "markers", marker = list(size = 10),
                           text = ~paste0(LABEL, " (day ", DAY, ")"), hoverinfo = "text")
      plotly::layout(p, xaxis = list(title = "Study day"), yaxis = list(title = "", type = "category"),
                     shapes = list(list(type = "line", x0 = 1, x1 = 1, yref = "paper", y0 = 0, y1 = 1,
                                        line = list(color = "#2a78d6", dash = "dot"))))
    })
  })
}

profile_forms <- function(params, study) {
  if (length(params$forms)) unlist(params$forms) else names(study$tables)
}
