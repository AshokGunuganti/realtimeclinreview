# New and updated data: for each monitored form, the records that are new,
# updated or deleted since the earlier date, as coloured listings with the
# changed values marked, as in the 2017 review workbook. Forms with nothing
# new are left out. Params: forms (default: the study's monitored forms).

monitored_ui <- function(id, params, study) {
  ns <- shiny::NS(id)
  forms <- if (length(params$forms)) unlist(params$forms) else monitored_forms(study)
  shiny::tagList(
    shiny::div(class = "listing-tools", shiny::uiOutput(ns("summary")), change_legend()),
    lapply(forms, function(f) {
      shiny::conditionalPanel(sprintf("output['%s'] == 'yes'", ns(paste0("has_", f))), class = "form-changes",
                              listing_ui(ns(f), list(form = f, title = f, show = "changes", tools = FALSE), study))
    }),
    shiny::conditionalPanel(sprintf("output['%s'] == 'no'", ns("any")),
                            shiny::p(class = "note", "No new or updated records in this window."))
  )
}

monitored_server <- function(id, ctx, params) {
  shiny::moduleServer(id, function(input, output, session) {
    forms <- if (length(params$forms)) unlist(params$forms) else monitored_forms(ctx$study)
    counts <- lapply(stats::setNames(forms, forms), function(f) {
      l <- listing_server(f, ctx, list(form = f, show = "changes", tools = FALSE))
      output[[paste0("has_", f)]] <- shiny::renderText(if (l$n_changes() > 0) "yes" else "no")
      shiny::outputOptions(output, paste0("has_", f), suspendWhenHidden = FALSE)
      l$n_changes
    })
    output$any <- shiny::renderText(if (any(vapply(counts, function(n) n() > 0, logical(1)))) "yes" else "no")
    shiny::outputOptions(output, "any", suspendWhenHidden = FALSE)
    output$summary <- shiny::renderUI({
      ch <- ctx$changes()
      ch <- ch[ch$FORMOID %in% forms, , drop = FALSE]
      shiny::span(shiny::strong(sprintf("%d new, %d updated, %d deleted records", sum(ch$RECSTAT == "New"),
                                        sum(ch$RECSTAT == "Changed"), sum(ch$RECSTAT == "Deleted"))),
                  shiny::span(class = "note", sprintf(" since %s, in %d form(s)", fmt_date(ctx$since()),
                                                      length(unique(ch$FORMOID)))))
    })
  })
}
