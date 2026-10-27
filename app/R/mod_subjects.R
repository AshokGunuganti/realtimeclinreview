# Subject list: every subject with status, reference dates, review status in
# the window and open items. Click a subject to open it in the side panel.

subjects_ui <- function(id, params, study) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("kpis")), shiny::br(),
    table_card_ui(ns("tbl"), params$title,
                  "Click a subject to open it. Review: status for changes in the window (since, review date].")
  )
}

subjects_server <- function(id, ctx, params) {
  shiny::moduleServer(id, function(input, output, session) {
    output$kpis <- shiny::renderUI({
      a <- ctx$data()$adsl
      q <- ctx$review_queue()
      st <- toupper(a$EOSSTT)
      do.call(bslib::layout_columns, c(list(fill = FALSE, col_widths = bslib::breakpoints(sm = 6, lg = 3)), list(
        kpi("Subjects", nrow(a), sprintf("%d ongoing", sum(st == "ONGOING", na.rm = TRUE))),
        kpi("Completed", sum(st == "COMPLETED", na.rm = TRUE)),
        kpi("Discontinued", sum(st == "DISCONTINUED", na.rm = TRUE)),
        kpi("To review", sum(q$STATUS == "To review"), sprintf("changes since %s", fmt_date(ctx$since()))))))
    })
    tbl <- shiny::reactive({
      rd <- ctx$data()
      a <- rd$adsl
      q <- ctx$review_queue()
      fl <- clinical_findings(rd)
      dq <- findings_by_tier(rd)
      qr <- ctx$queries()
      x <- data.frame(Subject = sid_link(a$SUBJID), Site = a$SITEID,
                      Status = tools::toTitleCase(tolower(a$EOSSTT)), stringsAsFactors = FALSE, check.names = FALSE)
      dates <- c(Enrolled = "ENRLDT", `First dose` = "TRTSDT", `Last contact` = "LSTCNTDT")
      for (n in names(dates)) if (!is.null(a[[dates[[n]]]])) x[[n]] <- format(a[[dates[[n]]]])
      if (!is.null(a$TRTSDT)) x$`Study day` <- study_day(ctx$asof(), a$TRTSDT)
      x$Review <- badge(q$STATUS[match(a$SUBJID, q$SUBJID)])
      x$`Clinical flags` <- count_by(fl$SUBJID, a$SUBJID)
      x$`Data findings` <- count_by(dq$SUBJID, a$SUBJID)
      if (!is.null(qr)) x$`Unresolved queries` <- count_by(qr$SUBJID[qr$STATUS != "Closed"], a$SUBJID)
      x
    })
    table_card_server("tbl", tbl, ctx, "Subjects", page_length = 25, selection = "none")
  })
}

count_by <- function(x, ids) {
  n <- table(factor(x, levels = ids))
  as.integer(n[ids])
}
