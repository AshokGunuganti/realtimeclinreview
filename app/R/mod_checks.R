# Check results: one row per check with its finding count and result; click a
# check to list its findings, with review status and marks.

checks_ui <- function(id, params, study) {
  ns <- shiny::NS(id)
  shiny::tagList(
    table_card_ui(ns("sum"), "Checks", "Click a check to list only its findings."),
    table_card_ui(ns("fnd"), "Findings", "Select findings to mark them. A mark resets when the values it relied on change.",
                  mark_type = "finding")
  )
}

checks_server <- function(id, ctx, params) {
  shiny::moduleServer(id, function(input, output, session) {
    summary <- shiny::reactive({
      s <- ctx$data()$summary
      data.frame(ID = s$ID, Name = s$NAME, Tier = s$TIER, Timing = s$TIMING, Severity = badge(s$SEVERITY),
                 Owner = s$OWNER, Findings = s$N, Result = badge(s$RESULT), stringsAsFactors = FALSE)
    })
    picked <- table_card_server("sum", summary, ctx, "Checks", selection = "single", page_length = 20)
    findings <- shiny::reactive({
      f <- ctx$data()$findings
      p <- picked()
      if (!is.null(p)) f <- f[f$CHECKID == p$ID, , drop = FALSE]
      f
    })
    tbl <- shiny::reactive({
      f <- findings()
      x <- data.frame(Check = f$CHECKID, Severity = badge(f$SEVERITY), Subject = sid_link(f$SUBJID), Site = f$SITEID,
                      Where = f$ANCHOR, Field = f$FIELD, Value = f$VALUE, Message = htmltools::htmlEscape(f$MESSAGE),
                      Owner = f$OWNER, stringsAsFactors = FALSE)
      with_marks(cbind(x, mark_cols(f$FINDINGKEY, f$DEPHASH, f$SUBJID, f$VERSION)), ctx, "finding")
    })
    table_card_server("fnd", tbl, ctx, "Findings", mark_type = "finding", page_length = 15)
  })
}
