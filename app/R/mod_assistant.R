# The assistant panel: a chat that slides in from the right over any screen,
# opened by "Ask the data" in the top strip. It has no backdrop, so the screen
# stays usable while the assistant opens tabs, checks and profiles. When the
# assistant cannot run (no key, no packages) the panel says how to set it up.
# See assistant.R for what the model may see and assistant_tools.R for its tools.

assistant_suggestions <- c("Which subjects have severe or serious adverse events?",
                           "What is new or updated since the last review?",
                           "Which checks have new findings?",
                           "Which sites have the most open queries?")

assistant_button <- function() {
  shiny::tags$button(type = "button", class = "btn btn-sm btn-primary ask-btn", `data-bs-toggle` = "offcanvas",
                     `data-bs-target` = "#assistant-oc", `aria-controls` = "assistant-oc", "Ask the data")
}

assistant_ui <- function(id, study, cfg) {
  ns <- shiny::NS(id)
  why <- assistant_unavailable(cfg)
  body <- if (nzchar(why)) shiny::div(class = "ask-off", shiny::p(shiny::strong("The assistant is not set up.")),
                                      shiny::p(class = "note", why))
    else shinychat::chat_ui(ns("chat"), greeting = assistant_greeting(study), placeholder = "Ask about the study data",
                            height = "100%", fill = TRUE, show_history = FALSE)
  shiny::div(
    class = "offcanvas offcanvas-end ask-panel", tabindex = "-1", id = "assistant-oc",
    `data-bs-scroll` = "true", `data-bs-backdrop` = "false", `aria-labelledby` = "assistant-title",
    shiny::div(class = "offcanvas-header",
      shiny::div(class = "flex-grow-1",
        shiny::div(id = "assistant-title", class = "ask-title", "Study assistant"),
        shiny::div(class = "note", "Answers from the review data at the review date and site. Free text is withheld.")),
      if (!nzchar(why)) shiny::actionLink(ns("reset"), "New chat", class = "ask-reset"),
      shiny::tags$button(type = "button", class = "btn-close", `data-bs-dismiss` = "offcanvas", `aria-label` = "Close")),
    shiny::div(class = "offcanvas-body ask-body", body))
}

assistant_greeting <- function(study) {
  paste0("Ask me about **", study$id, "**: subjects, adverse events, visits, checks, queries or what changed. ",
         "I can also open screens and subject profiles for you.\n\n",
         paste0("* <span class=\"suggestion\">", assistant_suggestions, "</span>", collapse = "\n"))
}

# `client_factory()` builds the session's chat client and `append(response)`
# shows a response; tests inject both, so no network or key is needed.
assistant_server <- function(id, ctx, cfg, client_factory = NULL, append = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    if (is.null(client_factory) && nzchar(assistant_unavailable(cfg))) return(invisible(NULL))
    factory <- client_factory %||% function() assistant_client(ctx$study, cfg, assistant_tools(ctx, cfg))
    show <- append %||% function(response) shinychat::chat_append("chat", response)
    client <- shiny::reactiveVal(NULL)
    turns <- shiny::reactiveVal(0L)

    shiny::observeEvent(input$chat_user_input, {
      msg <- trimws(input$chat_user_input %||% "")
      if (!nzchar(msg)) return()
      if (turns() >= cfg$max_turns) return(show("_That is the limit for one chat. Start a new chat to go on._"))
      rtcrcore::log_access(ctx$con, ctx$study$id, ctx$user, "assistant", "question", msg)
      if (is.null(client())) {
        cl <- tryCatch(factory(), error = function(e) e)
        if (inherits(cl, "error")) return(show(assistant_error_text(conditionMessage(cl))))
        client(cl)
      }
      turns(turns() + 1L)
      p <- tryCatch(show(client()$stream_async(msg)),
                    error = function(e) { show(assistant_error_text(conditionMessage(e))); NULL })
      if (promises::is.promise(p)) promises::catch(p, function(e) show(assistant_error_text(conditionMessage(e))))
    })

    shiny::observeEvent(input$reset, {
      client(NULL)
      turns(0L)
      if (is.null(append)) shinychat::chat_clear("chat")
    })

    invisible(list(client = client, turns = turns))
  })
}
