# =============================================================================
#  Source checks: data load manifest and SC-xxx check results
# =============================================================================

checks_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("kpis")), shiny::br(),
    bslib::layout_columns(fill = FALSE, col_widths = c(7, 5),
      table_card_ui(ns("sum"), "Source data checks", "Load-time checks plus as-of checks that rerun when the review date changes."),
      table_card_ui(ns("man"), "Source files loaded")),
    table_card_ui(ns("find"), "Check findings")
  )
}

checks_server <- function(id, src, rv, snap, ctrl, registry) {
  shiny::moduleServer(id, function(input, output, session) {
    ck <- shiny::reactive(run_checks(src, rv, snap()))
    output$kpis <- shiny::renderUI({
      s <- ck()$summary
      bslib::layout_columns(fill = FALSE, col_widths = bslib::breakpoints(sm = 6, lg = 3),
        kpi("Checks run", nrow(s), sprintf("%d pass", sum(s$RESULT == "Pass"))),
        kpi("Errors", sum(s$N[s$SEVERITY == "Error"]), "block review if > 0", if (any(s$N[s$SEVERITY == "Error"] > 0)) "flag" else ""),
        kpi("Warnings", sum(s$N[s$SEVERITY == "Warning"]), "review with data management", if (any(s$N[s$SEVERITY == "Warning"] > 0)) "warn" else ""),
        kpi("Source files", if (is.null(rv$manifest)) "–" else nrow(rv$manifest),
            if (is.null(rv$manifest)) NULL else paste("extract", rv$manifest$EXTRACT_DT[1])))
    })
    sm <- shiny::reactive({ s <- ck()$summary
      data.frame(Check = sprintf('<span style="white-space:nowrap">%s</span>', s$CHECK), Description = htmltools::htmlEscape(s$DESCRIPTION), Severity = s$SEVERITY, Findings = s$N, Result = badge(s$RESULT),
                 check.names = FALSE) })
    table_card_server("sum", sm, registry, "Source checks", ctrl$about, page_length = 15)
    mn <- shiny::reactive({ m <- rv$manifest; if (is.null(m)) data.frame(File = character()) else
      data.frame(File = m$FILE, Source = m$SOURCE, Rows = m$ROWS, Extract = m$EXTRACT_DT) })
    table_card_server("man", mn, page_length = 12)
    fd <- shiny::reactive({ f <- ck()$findings
      if (!nrow(f)) return(data.frame(Check = character()))
      data.frame(Check = sprintf('<span style="white-space:nowrap">%s</span>', f$CHECK), Source = f$SOURCE, Subject = ifelse(f$SUBJID %in% src$dm$SUBJID, sid_link(f$SUBJID), f$SUBJID),
                 Field = f$FIELD, Value = htmltools::htmlEscape(f$VALUE), Detail = f$DETAIL, check.names = FALSE) })
    table_card_server("find", fd, registry, "Check findings", ctrl$about, empty = "No findings.")
  })
}
