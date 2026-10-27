# Queries: every EDC query from the query report with its status on the review
# date, unresolved and oldest first. Params: form (the query report form, so
# the screen appears only when the study has one), query_age (days after which
# an unresolved query counts as old; default 30).

queries_ui <- function(id, params, study) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("kpis")), shiny::br(),
    table_card_ui(ns("tbl"), params$title,
                  "Status uses only dates on or before the review date. Unresolved: open or answered.")
  )
}

queries_server <- function(id, ctx, params) {
  shiny::moduleServer(id, function(input, output, session) {
    old <- if (is.null(params$query_age)) 30 else params$query_age
    q <- shiny::reactive({
      x <- ctx$queries()
      if (is.null(x)) x <- data.frame(SUBJID = character(), STATUS = character(), AGE = numeric())
      x[order(x$STATUS == "Closed", -ifelse(is.na(x$AGE), 0, x$AGE)), , drop = FALSE]
    })
    output$kpis <- shiny::renderUI({
      x <- q()
      un <- x$STATUS != "Closed"
      do.call(bslib::layout_columns, c(list(fill = FALSE, col_widths = bslib::breakpoints(sm = 6, lg = 3)), list(
        kpi("Unresolved", sum(un), sprintf("%d open, %d answered", sum(x$STATUS == "Open"), sum(x$STATUS == "Answered"))),
        kpi(sprintf("Older than %d days", old), sum(un & x$AGE > old, na.rm = TRUE), "unresolved"),
        kpi("Closed", sum(x$STATUS == "Closed"),
            if (any(!is.na(x$DAYSTOCLOSE))) sprintf("median %g days to close", stats::median(x$DAYSTOCLOSE, na.rm = TRUE))),
        kpi("Raised", nrow(x), "on or before the review date"))))
    })
    tbl <- shiny::reactive({
      x <- q()
      data.frame(Subject = sid_link(x$SUBJID), Site = x$SITEID, Query = x$RECID, Form = x$QRYFORM,
                 Folder = htmltools::htmlEscape(x$FOLDER), Text = htmltools::htmlEscape(x$QRYTEXT),
                 Status = badge(x$STATUS), Opened = format(x$QRYOPDT), Answered = format(x$QRYANSDT),
                 Closed = format(x$QRYCLSDT), `Age (d)` = x$AGE, check.names = FALSE, stringsAsFactors = FALSE)
    })
    table_card_server("tbl", tbl, ctx, "Queries", page_length = 20, selection = "none")
  })
}
