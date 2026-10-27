# What's new: records and findings that are new, changed or deleted since the
# reviewer's last sign-off (or the chosen date), priority forms first.

whatsnew_ui <- function(id, params, study) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("kpis")), shiny::br(),
    table_card_ui(ns("rec"), "Record changes", "New, changed and deleted records in the window (since, review date].",
                  mark_type = "record"),
    table_card_ui(ns("fnd"), "Finding changes", "Check findings that are new, changed or resolved since the earlier date.",
                  mark_type = "finding")
  )
}

whatsnew_server <- function(id, ctx, params) {
  shiny::moduleServer(id, function(input, output, session) {
    prio <- unlist(params$priority)
    changes <- shiny::reactive({
      d <- ctx$changes()
      p <- match(d$FORMOID, prio)
      d[order(is.na(p), p, match(d$RECSTAT, c("Deleted", "Changed", "New")), d$FORMOID, d$SUBJID), , drop = FALSE]
    })
    findings <- shiny::reactive({
      cmp <- rtcrcore::compare_findings(ctx$data_since()$findings, ctx$data()$findings)
      cmp <- cmp[cmp$FINDSTAT != "Persisting", , drop = FALSE]
      cmp[order(match(cmp$FINDSTAT, c("New", "Changed", "Resolved")), match(cmp$SEVERITY, c("error", "warning", "info"))), ,
          drop = FALSE]
    })

    rec_tbl <- shiny::reactive({
      d <- changes()
      x <- data.frame(Status = badge(d$RECSTAT), Form = d$FORMOID, Subject = sid_link(d$SUBJID), Record = d$RECID,
                      `Changed fields` = d$CHGVARS, Version = d$VERSION, check.names = FALSE, stringsAsFactors = FALSE)
      with_marks(cbind(x, mark_cols(d$KEY, d$DEPHASH, d$SUBJID, d$VERSION)), ctx, "record")
    })
    fnd_tbl <- shiny::reactive({
      f <- findings()
      x <- data.frame(Status = badge(f$FINDSTAT), Check = f$CHECKID, Severity = badge(f$SEVERITY),
                      Subject = sid_link(f$SUBJID), Where = f$ANCHOR, Field = f$FIELD, Value = f$VALUE,
                      Message = htmltools::htmlEscape(f$MESSAGE), check.names = FALSE, stringsAsFactors = FALSE)
      with_marks(cbind(x, mark_cols(f$FINDINGKEY, f$DEPHASH, f$SUBJID, f$VERSION)), ctx, "finding")
    })
    output$kpis <- shiny::renderUI({
      d <- changes()
      f <- findings()
      bslib::layout_columns(
        fill = FALSE, col_widths = bslib::breakpoints(sm = 6, lg = 2),
        kpi("New records", sum(d$RECSTAT == "New"), sprintf("since %s", format(ctx$since()))),
        kpi("Changed records", sum(d$RECSTAT == "Changed"), "corrections to earlier data"),
        kpi("Deleted records", sum(d$RECSTAT == "Deleted")),
        kpi("New findings", sum(f$FINDSTAT == "New"), sprintf("%d error", sum(f$FINDSTAT == "New" & f$SEVERITY == "error"))),
        kpi("Changed findings", sum(f$FINDSTAT == "Changed")),
        kpi("Resolved findings", sum(f$FINDSTAT == "Resolved")))
    })
    table_card_server("rec", rec_tbl, ctx, "Record changes", mark_type = "record")
    table_card_server("fnd", fnd_tbl, ctx, "Finding changes", mark_type = "finding")
  })
}
