# Sign-off: closes a review cycle. Review is tracked subject by subject with
# review marks; when every subject in the window (changes since, review date]
# is reviewed, the reviewer signs off the cycle. The sign-off records who,
# when, what it means and a hash of every item reviewed, and its review date
# becomes the default "changes since" for the next cycle.
#
# Signing needs re-authentication through the hosting platform's single
# sign-on. Until that is wired up, only simulated studies (study.yml app:
# simulated) can sign, and the record says no re-authentication took place.

signoff_ui <- function(id, params, study) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("status")), shiny::br(),
    table_card_ui(ns("hist"), "Sign-off history", "Newest first. The scope hash identifies exactly what was reviewed.")
  )
}

signoff_server <- function(id, ctx, params) {
  shiny::moduleServer(id, function(input, output, session) {
    cfg <- if (is.null(ctx$study$config$app)) list() else ctx$study$config$app
    type <- review_type(cfg)
    demo <- isTRUE(cfg$simulated)
    rev <- shiny::reactiveVal(0)
    output$status <- shiny::renderUI({
      q <- ctx$review_queue()
      todo <- sum(q$STATUS == "To review")
      ready <- todo == 0 && identical(ctx$site(), "All sites")
      bslib::card(fill = FALSE, bslib::card_header(sprintf("Sign off the %s review", type)),
        shiny::p(sprintf("Window: changes from %s to %s, all sites. %d subject(s) in the window, %d still to review.",
                         fmt_date(ctx$since()), fmt_date(ctx$asof()), nrow(q), todo)),
        if (!identical(ctx$site(), "All sites")) shiny::p(class = "note", "Set Site to All sites to sign off: a sign-off covers the whole study."),
        if (!demo) shiny::p(class = "note", "Signing needs re-authentication through the login, which is not set up yet.")
        else shiny::p(class = "note", "Simulated study: signing is a demonstration and records no re-authentication."),
        shiny::div(shiny::actionButton(session$ns("sign"), "Sign off review", class = "btn-primary",
                                       disabled = !(ready && demo))))
    })
    shiny::observeEvent(input$sign, {
      shiny::showModal(shiny::modalDialog(
        title = sprintf("Sign off the %s review as of %s", type, fmt_date(ctx$asof())),
        shiny::textInput(session$ns("name"), "Printed name"),
        shiny::selectInput(session$ns("meaning"), "Meaning of signature",
                           c("I have reviewed the data in this review window",
                             "I have reviewed the data in this review window and approve it")),
        shiny::p(class = "note", "This is an electronic signature. It is recorded with your user ID and the time."),
        footer = shiny::tagList(shiny::modalButton("Cancel"),
                                shiny::actionButton(session$ns("confirm"), "Sign", class = "btn-primary"))))
    })
    shiny::observeEvent(input$confirm, {
      it <- ctx$review_items()
      ok <- tryCatch({
        rtcrcore::sign_off(ctx$con, ctx$study$id, type, ctx$asof(),
                           data.frame(TARGET_KEY = paste0(it$TYPE, ":", it$KEY), DEPHASH = it$DEPHASH),
                           ctx$user, input$name, input$meaning, auth_at = Sys.time())
        TRUE
      }, error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "error")
        FALSE
      })
      if (ok) {
        shiny::removeModal()
        rev(rev() + 1)
        ctx$bump_marks()
        shiny::showNotification(sprintf("Signed off. Next time, changes since defaults to %s.", fmt_date(ctx$asof())),
                                type = "message", duration = 8)
      }
    })
    hist <- shiny::reactive({
      rev()
      s <- rtcrcore::signoffs(ctx$con, ctx$study$id)
      data.frame(`Review date` = s$ASOF, Type = s$REVIEW_TYPE, Name = s$FULL_NAME, User = s$USERID,
                 Meaning = s$MEANING, `Items` = s$SCOPE_N, `Signed (UTC)` = s$SIGNED_AT, `Scope hash` = s$SCOPE_HASH,
                 check.names = FALSE, stringsAsFactors = FALSE)
    })
    table_card_server("hist", hist, ctx, "Sign-offs", selection = "none")
  })
}

audit_ui <- function(id, params, study) {
  table_card_ui(shiny::NS(id)("tbl"), params$title,
                "Every review mark, cleared mark and sign-off, newest first. The store never changes or deletes a row.")
}

audit_server <- function(id, ctx, params) {
  shiny::moduleServer(id, function(input, output, session) {
    tbl <- shiny::reactive({
      ctx$marks_rev()
      a <- rtcrcore::audit_trail(ctx$con, ctx$study$id)
      a <- a[rev(seq_len(nrow(a))), , drop = FALSE]
      data.frame(`When (UTC)` = a$EVENT_AT, User = a$USERID, Event = a$EVENT, Target = htmltools::htmlEscape(a$TARGET),
                 Before = htmltools::htmlEscape(a$OLD_VALUE), After = htmltools::htmlEscape(a$NEW_VALUE),
                 check.names = FALSE, stringsAsFactors = FALSE)
    })
    table_card_server("tbl", tbl, ctx, "Audit trail", page_length = 25, selection = "none")
  })
}
