# =============================================================================
#  Sidebar controls: review date, change window, filters, thresholds, export.
#  Returns a list of reactives consumed by every screen module.
# =============================================================================

controls_ui <- function(id, sites) {
  ns <- shiny::NS(id)
  bslib::sidebar(
    width = 280, title = NULL,
    shiny::h6("Data refresh"),
    shiny::uiOutput(ns("asof_lab")),
    shiny::dateInput(ns("since"), "Show changes since", value = APP_CFG$default_asof - APP_CFG$default_lookback),
    shiny::actionButton(ns("advance"), "Simulate next weekly refresh", class = "btn-primary btn-sm"),
    shiny::actionButton(ns("reset"), "Reset review date", class = "btn-outline-secondary btn-sm"),
    shiny::hr(),
    shiny::h6("Filters"),
    shiny::selectInput(ns("site"), "Site", choices = c("All sites", sites)),
    shiny::radioButtons(ns("pop"), "Population",
                        c("All enrolled" = "all", "Efficacy ITT (dosed, age 6+)" = "itt")),
    shiny::sliderInput(ns("alert"), "PK alert threshold (ng/mL)", min = 0.5, max = 2, value = APP_CFG$pk_alert, step = 0.1),
    shiny::sliderInput(ns("cthr"), "Compliance flag below (%)", min = 60, max = 95, value = APP_CFG$comp_threshold, step = 5),
    shiny::hr(),
    shiny::h6("Export"),
    shiny::downloadButton(ns("xlall"), "Review workbook (.xlsx)", class = "btn-sm btn-xl", icon = NULL),
    shiny::p(class = "note", style = "margin-top:.4rem", "Every listing as its own sheet for the current review date and filters."),
    shiny::hr(),
    shiny::p(class = "note", "Simulated data for demonstration only. Not real trial data.")
  )
}

controls_server <- function(id, registry, sheet_order) {
  shiny::moduleServer(id, function(input, output, session) {
    asof <- shiny::reactiveVal(APP_CFG$default_asof)
    shiny::observeEvent(input$advance, {
      shiny::updateDateInput(session, "since", value = asof())
      asof(asof() + 7)
      shiny::showNotification(paste("Review date moved to", format(asof(), "%d %b %Y")), duration = 3)
    })
    shiny::observeEvent(input$reset, {
      asof(APP_CFG$default_asof)
      shiny::updateDateInput(session, "since", value = APP_CFG$default_asof - APP_CFG$default_lookback)
    })
    output$asof_lab <- shiny::renderUI(shiny::div(class = "note", shiny::HTML(sprintf(
      "Review as of: <b>%s</b>", format(asof(), "%d %b %Y")))))
    since <- shiny::reactive({ s <- input$since; if (is.null(s) || is.na(s)) asof() - 7 else min(s, asof() - 1) })

    about <- shiny::reactive(data.frame(
      Item = c("Report", "Data", "Review as of", "Changes since", "Site filter", "Population",
               "PK alert threshold (ng/mL)", "Compliance flag threshold (%)", "Generated"),
      Value = c("Phase 3 clinical data review", "SIMULATED data for demonstration only",
                format(asof(), "%d %b %Y"), format(since(), "%d %b %Y"), input$site,
                if (input$pop == "itt") "Efficacy ITT (dosed, age 6+)" else "All enrolled",
                input$alert, input$cthr, format(Sys.time(), "%Y-%m-%d %H:%M"))))

    output$xlall <- shiny::downloadHandler(
      filename = function() paste0("data_review_", format(asof(), "%Y%m%d"), ".xlsx"),
      content = function(file) {
        keys <- intersect(sheet_order, names(shiny::reactiveValuesToList(registry)))
        sheets <- lapply(keys, function(k) registry[[k]]()); names(sheets) <- keys
        write_review_xlsx(sheets, file, about())
      })

    list(asof = asof, since = since, site = shiny::reactive(input$site), pop = shiny::reactive(input$pop),
         alert = shiny::reactive(input$alert), cthr = shiny::reactive(input$cthr), about = about)
  })
}
