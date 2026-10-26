# =============================================================================
#  Reusable table card module: DT table + per-table Excel download.
#  Each instance registers its current data in a shared registry so the
#  sidebar can export every listing as one workbook.
# =============================================================================

table_card_ui <- function(id, title, note = NULL, full_screen = FALSE) {
  ns <- shiny::NS(id)
  bslib::card(
    fill = FALSE, full_screen = full_screen,
    bslib::card_header(shiny::span(title),
                       shiny::downloadButton(ns("xl"), "Excel", class = "btn-sm btn-xl", icon = NULL)),
    if (!is.null(note)) shiny::p(class = "note", note),
    DT::DTOutput(ns("tbl"), fill = FALSE)
  )
}

#' @param data reactive data.frame (may contain HTML for links/badges)
#' @param sheet sheet name used in the workbook export (NULL = not registered)
#' @param about reactive data.frame describing the current review context
#' @return reactive: the selected row of `data` (or NULL)
table_card_server <- function(id, data, registry = NULL, sheet = NULL, about = NULL,
                              page_length = 10, selection = "none", empty = "No records.") {
  shiny::moduleServer(id, function(input, output, session) {
    if (!is.null(registry) && !is.null(sheet)) registry[[sheet]] <- data
    output$tbl <- DT::renderDT({
      d <- data()
      DT::datatable(d, escape = FALSE, rownames = FALSE, selection = selection,
                    options = list(pageLength = page_length, dom = if (nrow(d) > page_length) "ftip" else "t",
                                   scrollX = TRUE, order = list(), language = list(emptyTable = empty)))
    })
    output$xl <- shiny::downloadHandler(
      filename = function() paste0(gsub("[^A-Za-z0-9]+", "_", sheet %||% id), "_", format(Sys.Date(), "%Y%m%d"), ".xlsx"),
      content = function(file) {
        sh <- list(data()); names(sh) <- sheet %||% id
        write_review_xlsx(sh, file, if (is.null(about)) data.frame(Item = "Export", Value = id) else about())
      })
    shiny::reactive({ i <- input$tbl_rows_selected; if (length(i)) data()[i, , drop = FALSE] else NULL })
  })
}

`%||%` <- function(a, b) if (is.null(a)) b else a
