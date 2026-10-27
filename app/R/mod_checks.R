# Check results: one row per check with its finding count and result; click a
# check to list its findings, new ones (since the earlier date) first, with
# review status and marks. Params: tiers (check tiers to show) or
# exclude_tiers (tiers to leave out); default all.

checks_ui <- function(id, params, study) {
  ns <- shiny::NS(id)
  shiny::tagList(
    table_card_ui(ns("sum"), params$title, "Click a check to list only its findings."),
    table_card_ui(ns("fnd"), "Findings", "Select findings to mark them. A mark resets when the values it relied on change.",
                  mark_type = "finding")
  )
}

checks_server <- function(id, ctx, params) {
  shiny::moduleServer(id, function(input, output, session) {
    in_scope <- function(tier) {
      (is.null(params$tiers) | tier %in% unlist(params$tiers)) & !tier %in% unlist(params$exclude_tiers)
    }
    new_keys <- shiny::reactive({
      cmp <- rtcrcore::compare_findings(ctx$data_since()$findings, ctx$data()$findings)
      cmp$FINDINGKEY[cmp$FINDSTAT %in% c("New", "Changed")]
    })
    summary <- shiny::reactive({
      s <- ctx$data()$summary
      s <- s[in_scope(s$TIER), , drop = FALSE]
      new <- new_keys()
      f <- ctx$data()$findings
      data.frame(ID = s$ID, Name = s$NAME, Category = s$CATEGORY, Severity = badge(s$SEVERITY), Owner = s$OWNER,
                 Findings = s$N, New = vapply(s$ID, function(id) sum(f$CHECKID == id & f$FINDINGKEY %in% new), 0L),
                 Result = badge(s$RESULT),
                 Note = if (is.null(s$ERROR)) "" else ifelse(is.na(s$ERROR), "", htmltools::htmlEscape(s$ERROR)),
                 stringsAsFactors = FALSE)
    })
    picked <- table_card_server("sum", summary, ctx, "Checks", selection = "single", page_length = 20)
    findings <- shiny::reactive({
      f <- ctx$data()$findings
      f <- f[f$CHECKID %in% summary()$ID, , drop = FALSE]
      p <- picked()
      if (!is.null(p)) f <- f[f$CHECKID == p$ID, , drop = FALSE]
      f[order(!f$FINDINGKEY %in% new_keys()), , drop = FALSE]
    })
    tbl <- shiny::reactive({
      f <- findings()
      x <- data.frame(Check = f$CHECKID, Status = badge(ifelse(f$FINDINGKEY %in% new_keys(), "New", "")),
                      Severity = badge(f$SEVERITY), Subject = sid_link(f$SUBJID), Site = f$SITEID,
                      Where = f$ANCHOR, Field = f$FIELD, Value = f$VALUE, Message = htmltools::htmlEscape(f$MESSAGE),
                      Owner = f$OWNER, stringsAsFactors = FALSE)
      with_marks(cbind(x, mark_cols(f$FINDINGKEY, f$DEPHASH, f$SUBJID, f$VERSION)), ctx, "finding")
    })
    table_card_server("fnd", tbl, ctx, "Findings", mark_type = "finding", page_length = 15)
  })
}
