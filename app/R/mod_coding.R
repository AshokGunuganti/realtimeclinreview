# Coding review: verbatim terms against coded terms for one coded log form,
# with consistency across subjects and dictionary version. Params: form,
# verbatim, coded, soc, version, date (coding date).

coding_ui <- function(id, params, study) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("kpis")), shiny::br(),
    table_card_ui(ns("terms"), "Verbatim terms",
                  "Inconsistent terms first: the same verbatim coded to more than one term. Click a term to list its records."),
    table_card_ui(ns("recs"), params$title,
                  "Select records to mark the coding reviewed. A mark resets when the record changes.",
                  mark_type = "record")
  )
}

coding_server <- function(id, ctx, params) {
  shiny::moduleServer(id, function(input, output, session) {
    form <- params$form
    cr <- shiny::reactive({
      rtcrcore::coding_review(ctx$data()$tables[[form]], params$verbatim, params$coded, soc = params$soc,
                              version = params$version, date = params$date, asof = ctx$asof())
    })
    output$kpis <- shiny::renderUI({
      r <- cr()$records
      t <- cr()$terms
      vers <- stats::na.omit(r$VERSION[r$VERSION != ""])
      latest <- if (length(vers)) as.character(max(numeric_version(vers))) else character()
      do.call(bslib::layout_columns, c(list(fill = FALSE, col_widths = bslib::breakpoints(sm = 6, lg = 3)), list(
        kpi("Records", nrow(r), sprintf("%d verbatim terms", nrow(t))),
        kpi("Not coded", sum(r$FLAG == "Not coded"), "awaiting coding on the review date"),
        kpi("Inconsistent terms", sum(!t$CONSISTENT), sprintf("%d records", sum(r$FLAG == "Inconsistent"))),
        kpi("Older version", sum(r$FLAG == "Older version"),
            if (length(latest)) paste("latest in use", latest) else NULL))))
    })
    terms <- shiny::reactive({
      t <- cr()$terms
      data.frame(Verbatim = htmltools::htmlEscape(t$VERBATIM), Records = t$RECORDS, Subjects = t$SUBJECTS,
                 `Coded as` = htmltools::htmlEscape(t$CODED),
                 Consistent = badge(ifelse(t$CONSISTENT, "Consistent", "Inconsistent")),
                 check.names = FALSE, stringsAsFactors = FALSE)
    })
    picked <- table_card_server("terms", terms, ctx, "Verbatim terms", page_length = 15)
    recs <- shiny::reactive({
      r <- cr()$records
      p <- picked()
      if (!is.null(p)) r <- r[htmltools::htmlEscape(r$VERBATIM) == p$Verbatim, , drop = FALSE]
      h <- ctx$record_hashes(form)
      key <- paste0(form, ":", r$RECID)
      t <- ctx$data()$tables[[form]]
      ver <- t$VERSION[match(r$RECID, t$RECID)]
      x <- data.frame(Subject = sid_link(r$SUBJID), Site = r$SITEID, Record = r$RECID,
                      Verbatim = htmltools::htmlEscape(r$VERBATIM), `Coded term` = htmltools::htmlEscape(r$CODED),
                      check.names = FALSE, stringsAsFactors = FALSE)
      if (!is.null(params$soc)) x$SOC <- htmltools::htmlEscape(r$SOC)
      if (!is.null(params$version)) x$Version <- r$VERSION
      if (!is.null(params$date)) x$`Coded on` <- format(r$CODEDT)
      x$Flag <- badge(r$FLAG)
      with_marks(cbind(x, mark_cols(key, h$DEPHASH[match(key, h$TARGET_KEY)], r$SUBJID, ver)), ctx, "record")
    })
    table_card_server("recs", recs, ctx, params$title, mark_type = "record", page_length = 15)
  })
}
