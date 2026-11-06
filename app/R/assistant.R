# AI assistant: a chat panel where reviewers ask questions about the study data
# in plain language (after Tahoe Explorer's assistant). Claude answers through
# read-only tools over the review data (assistant_tools.R) and can open screens,
# checks and subject profiles; it cannot change review marks or action items.
#
# Opt-in and never breaks the app: the panel is active only when the ellmer and
# shinychat packages are installed, ANTHROPIC_API_KEY is set on the server and
# study.yml does not turn the assistant off. Nothing is pasted in the browser.
# What the model may see is limited: at most `row_cap` rows per tool call, and
# no verbatim free text (fields named *TERM, *TXT, *TEXT, *COMM or *COMMENT,
# plus study.yml `assistant: free_text`) unless the study shares it
# (`assistant: share_free_text`). Every question and tool call goes to the
# access log.
#
# study.yml:
#   assistant:
#     enabled: true                       # false removes the panel
#     model: claude-sonnet-5-5            # or RTCR_ASSISTANT_MODEL
#     row_cap: 50
#     free_text: [SV.VISNDRS]             # also withheld; FORM.FIELD or FIELD
#     share_free_text: [AE.AETERM]        # free text the model may see

assistant_defaults <- list(enabled = TRUE, model = "claude-sonnet-5-5", row_cap = 50L, max_turns = 30L,
                           max_tokens = 2000L)
free_text_pattern <- "(TERM|TXT|TEXT|COMM|COMMENT)$"

assistant_config <- function(study) {
  cfg <- utils::modifyList(assistant_defaults, as.list(study$config$assistant))
  model <- Sys.getenv("RTCR_ASSISTANT_MODEL", "")
  if (nzchar(model)) cfg$model <- model
  cfg$row_cap <- max(1L, as.integer(cfg$row_cap))
  cfg$free_text <- as.character(unlist(cfg$free_text))
  cfg$share_free_text <- as.character(unlist(cfg$share_free_text))
  cfg
}

# Why the assistant is off, or "" when it can run. RTCR_ASSISTANT_DISABLE=1 turns
# it off everywhere (the test suite sets it).
assistant_unavailable <- function(cfg) {
  if (!isTRUE(cfg$enabled)) return("The assistant is turned off for this study.")
  if (nzchar(Sys.getenv("RTCR_ASSISTANT_DISABLE", ""))) return("The assistant is turned off on this server.")
  if (!requireNamespace("ellmer", quietly = TRUE) || !requireNamespace("shinychat", quietly = TRUE))
    return("Install the ellmer and shinychat packages to use the assistant.")
  if (!nzchar(Sys.getenv("ANTHROPIC_API_KEY", "")))
    return("Set ANTHROPIC_API_KEY on the server (for example in .Renviron) to use the assistant.")
  ""
}

# TRUE when `field` of `form` is free text the model must not see.
is_free_text <- function(form, field, cfg) {
  ids <- c(paste0(form, ".", field), field)
  hidden <- grepl(free_text_pattern, field) | ids[1] %in% cfg$free_text | ids[2] %in% cfg$free_text
  hidden & !(ids[1] %in% cfg$share_free_text | ids[2] %in% cfg$share_free_text)
}

assistant_system_prompt <- function(study, cfg) {
  paste(
    sprintf("You are the data review assistant for clinical study %s, inside its review app.", study$id),
    if (isTRUE(study$config$app$simulated)) "The study data is simulated." else NULL,
    "Reviewers (medical monitors, data managers, safety physicians, CRAs) ask you about the study data.",
    "",
    "Rules:",
    "- Answer only from the tools. Never guess or invent subjects, values, counts or dates; if the tools",
    "  cannot answer, say so and say which screen might.",
    "- Data is as of the review date and site the reviewer has chosen; the tools apply both. Mention them",
    "  when they matter (study_overview gives them).",
    "- Call study_overview or describe_form first when you are unsure which form or field holds something.",
    "  Use the field labels, not variable names, in answers; give subject IDs as they are.",
    sprintf("- Tools return at most %d rows. When total_matches is larger, say how many there are in all and", cfg$row_cap),
    "  that you are showing part of them; use count_records for totals.",
    "- Verbatim free text (such as reported AE terms and query text) is withheld from you. If a question needs",
    "  it, say it is withheld and offer to open the screen where the reviewer can read it.",
    "- When the reviewer asks to see or open something, use open_screen, open_check or open_subject_profile.",
    "- You cannot mark records reviewed, raise queries or change action items; say so if asked.",
    "- Use the totals the tools give (total_matches, counts) rather than counting rows yourself. Work out the",
    "  answer before writing it; do not correct yourself mid-answer.",
    "- Be brief: a sentence or two, then a small markdown table when listing records. No medical advice.",
    sep = "\n")
}

# The Claude chat for one session, with the tools registered.
assistant_client <- function(study, cfg, tools) {
  chat <- ellmer::chat_anthropic(
    system_prompt = assistant_system_prompt(study, cfg), model = cfg$model, echo = "none",
    params = ellmer::params(max_tokens = cfg$max_tokens))
  for (t in tools) chat$register_tool(as_ellmer_tool(t))
  chat
}

as_ellmer_tool <- function(spec) {
  arg_type <- function(a) {
    req <- isTRUE(a$required)
    switch(a$type,
           string = ellmer::type_string(a$desc, required = req),
           integer = ellmer::type_integer(a$desc, required = req),
           array = ellmer::type_array(ellmer::type_string(), a$desc, required = req),
           stop("unknown tool argument type: ", a$type, call. = FALSE))
  }
  # Results go to the model as JSON text; the copy keeps the argument names ellmer matches.
  as_json <- spec$fun
  body(as_json) <- substitute(as.character(jsonlite::toJSON(CALL, auto_unbox = TRUE, na = "null", null = "null")),
                              list(CALL = as.call(c(list(spec$fun), lapply(names(formals(spec$fun)), as.name)))))
  ellmer::tool(as_json, spec$description, arguments = lapply(spec$arguments, arg_type), name = spec$name,
               annotations = ellmer::tool_annotations(read_only_hint = !isTRUE(spec$navigates)))
}

# A provider error as a chat message, without the key or request detail.
assistant_error_text <- function(msg) {
  msg <- gsub("sk-ant-[A-Za-z0-9_-]+", "[key]", msg)
  hint <- if (grepl("401|authentication|api[_ ]key", msg, ignore.case = TRUE)) "The server's API key was refused."
          else if (grepl("429|rate|overloaded|529", msg, ignore.case = TRUE)) "The model is busy; try again in a moment."
          else "The assistant could not answer."
  paste0("_", hint, "_\n\n<details><summary>Detail</summary>\n\n", substr(msg, 1, 400), "\n\n</details>")
}
