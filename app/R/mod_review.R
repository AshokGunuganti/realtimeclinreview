# My review: an inbox. Subjects with new, changed or deleted data in the
# window (changes since, review date], or a clinical flag new in the window,
# are listed on the left, still to review first; the selected subject is on
# the right, new data first. "Reviewed, next" marks the subject and moves on.
# The full list of record and finding changes is the What's new screen.

review_ui <- function(id, params, study) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("progress")),
    shiny::div(class = "inbox",
               shiny::div(class = "inbox-list", shiny::uiOutput(ns("list"))),
               shiny::div(class = "inbox-main",
                          shiny::div(class = "inbox-head", shiny::uiOutput(ns("head"))),
                          shiny::div(class = "inbox-scroll", shiny::uiOutput(ns("body"))),
                          shiny::div(class = "inbox-foot", review_actions(ns))))
  )
}

review_server <- function(id, ctx, params) {
  shiny::moduleServer(id, function(input, output, session) {
    picked <- shiny::reactiveVal(NULL)
    # The subject shown: the one picked while it is in the queue, else the first still to review.
    subj <- shiny::reactive({
      q <- ctx$review_queue()
      p <- picked()
      if (!is.null(p) && p %in% q$SUBJID) p else if (nrow(q)) {
        first <- next_in_queue(q)
        if (is.null(first)) q$SUBJID[1] else first
      } else NULL
    })
    open <- function(s) picked(s)
    shiny::observeEvent(input$pick, picked(input$pick))
    review_controls(input, session, ctx, subj, open)

    output$progress <- shiny::renderUI({
      q <- ctx$review_queue()
      it <- ctx$review_items()
      done <- sum(q$STATUS == "Reviewed")
      rec <- it[it$TYPE == "record", , drop = FALSE]
      shiny::div(class = "review-progress",
                 shiny::div(class = "grow",
                            shiny::div(shiny::strong(sprintf("%d of %d subjects reviewed", done, nrow(q))),
                                       shiny::span(class = "note", sprintf(
                                         " \u00b7 %d new and %d changed records, %d new clinical flags since %s",
                                         sum(rec$RECSTAT == "New"), sum(rec$RECSTAT == "Changed"),
                                         sum(it$TYPE == "finding"), fmt_date(ctx$since())))),
                            shiny::div(class = "track", shiny::tags$i(style = sprintf(
                              "width:%.0f%%", if (nrow(q)) 100 * done / nrow(q) else 100)))))
    })
    output$list <- shiny::renderUI({
      q <- ctx$review_queue()
      if (!nrow(q)) return(shiny::div(class = "inbox-empty", "Nothing new in this window."))
      cur <- subj()
      sm <- ctx$data()$summary
      flag_name <- function(ids) ifelse(is.na(match(ids, sm$ID)), ids, sm$NAME[match(ids, sm$ID)])
      a <- ctx$data()$adsl
      day <- if (is.null(a$TRTSDT)) rep(NA, nrow(q)) else study_day(ctx$asof(), a$TRTSDT[match(q$SUBJID, a$SUBJID)])
      item <- function(i) {
        chips <- c(if (nzchar(q$FLAGS[i])) sprintf('<span class="badge-s b-crit">%s</span>',
                                                   htmltools::htmlEscape(flag_name(strsplit(q$FLAGS[i], "; ")[[1]]))),
                   if (nzchar(q$CHANGES[i])) sprintf('<span class="badge-s b-new">%s</span>',
                                                     htmltools::htmlEscape(strsplit(q$CHANGES[i], "; ")[[1]])))
        done <- q$STATUS[i] == "Reviewed"
        shiny::tags$a(
          class = paste("inbox-item", if (identical(q$SUBJID[i], cur)) "cur", if (done) "done"),
          role = "button", tabindex = "0",
          onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", session$ns("pick"), q$SUBJID[i]),
          shiny::div(class = "inbox-row", shiny::span(class = "inbox-id", q$SUBJID[i]),
                     shiny::span(class = "note", if (done) paste("\u2713", q$LASTBY[i]) else
                       paste0("Site ", q$SITEID[i], if (!is.na(day[i])) paste(" \u00b7 day", day[i])))),
          if (!done) shiny::div(class = "inbox-chips", shiny::HTML(paste(chips, collapse = " "))))
      }
      todo <- which(q$STATUS == "To review")
      done <- which(q$STATUS == "Reviewed")
      shiny::tagList(
        shiny::div(class = "inbox-sep", sprintf("To review (%d)", length(todo))),
        lapply(todo, item),
        if (length(done)) shiny::div(class = "inbox-sep", sprintf("Reviewed (%d)", length(done))),
        lapply(done, item))
    })
    output$head <- shiny::renderUI({
      s <- subj()
      if (is.null(s)) return(shiny::span(class = "note", "Nothing to review."))
      panel_head(ctx, s)
    })
    output$body <- shiny::renderUI({
      s <- subj()
      if (is.null(s)) {
        return(shiny::div(class = "inbox-empty", sprintf(
          "No new or changed data and no new clinical flags since %s. Change \"Changes since\" to look further back.",
          fmt_date(ctx$since()))))
      }
      panel_body(ctx, s, ctx$timeline)
    })
  })
}
