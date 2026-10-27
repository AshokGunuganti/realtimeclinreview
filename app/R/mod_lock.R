# Lock readiness: per subject and form, unresolved queries, open findings and
# EDC form status (SDV, PI signature, freeze, lock). Params: form_status (form
# status report form), forms (default: all clinical forms), exclude_tiers
# (check tiers that do not block lock; default clinical).

lock_ui <- function(id, params, study) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("kpis")), shiny::br(),
    table_card_ui(ns("subj"), "Subjects",
                  paste("A subject is locked when every EDC form is locked and vendor data has no blockers.",
                        "Click a subject to list only its forms.",
                        "Status uses only events on or before the review date.")),
    table_card_ui(ns("forms"), "Forms", "Blockers: unresolved queries, open data findings, SDV and PI signature.")
  )
}

lock_server <- function(id, ctx, params) {
  shiny::moduleServer(id, function(input, output, session) {
    forms <- if (length(params$forms)) unlist(params$forms) else clinical_forms(ctx$study)
    excl <- if (is.null(params$exclude_tiers)) c("clinical", "structural") else params$exclude_tiers
    lr <- shiny::reactive({
      rd <- ctx$data()
      fs <- rtcrcore::form_status(rd$tables[[params$form_status]], ctx$asof())
      rtcrcore::lock_readiness(rd$tables, forms, fs, ctx$queries(), findings_by_tier(rd, excl))
    })
    subjects <- shiny::reactive(rtcrcore::lock_by_subject(lr()))
    output$kpis <- shiny::renderUI({
      s <- subjects()
      x <- lr()
      n <- function(st) sum(s$STATUS == st)
      do.call(bslib::layout_columns, c(list(fill = FALSE, col_widths = bslib::breakpoints(sm = 6, lg = 3)), list(
        kpi("Subjects locked", n("Locked"), sprintf("of %d", nrow(s))),
        kpi("Ready to lock", n("Ready to lock"), "subjects"),
        kpi("Not ready", n("Not ready"), sprintf("%d of %d forms", sum(x$STATUS == "Not ready"), nrow(x))),
        kpi("Unresolved queries", sum(x$OPENQ), sprintf("%d open findings", sum(x$FINDINGS))))))
    })
    stbl <- shiny::reactive({
      s <- subjects()
      data.frame(Subject = sid_link(s$SUBJID), Site = s$SITEID, Status = badge(s$STATUS),
                 `Forms ready` = sprintf("%d of %d", s$READY, s$FORMS), `Unresolved queries` = s$OPENQ,
                 `Open findings` = s$FINDINGS, .SUBJID = s$SUBJID, check.names = FALSE, stringsAsFactors = FALSE)
    })
    picked <- table_card_server("subj", stbl, ctx, "Lock readiness subjects", page_length = 15)
    ftbl <- shiny::reactive({
      x <- lr()
      p <- picked()
      if (!is.null(p)) x <- x[x$SUBJID == p$.SUBJID, , drop = FALSE]
      of <- function(k) ifelse(x$INSTANCES > 0, sprintf("%d of %d", k, x$INSTANCES), "\u2013")
      data.frame(Subject = sid_link(x$SUBJID), Site = x$SITEID, Form = x$FORMOID, Records = x$RECORDS,
                 SDV = of(x$SDV), Signed = of(x$SIGNED), Frozen = of(x$FROZEN), Locked = of(x$LOCKED),
                 `Unresolved queries` = x$OPENQ, `Open findings` = x$FINDINGS, Status = badge(x$STATUS),
                 Blockers = x$BLOCKERS, check.names = FALSE, stringsAsFactors = FALSE)
    })
    table_card_server("forms", ftbl, ctx, "Lock readiness forms", page_length = 20)
  })
}
