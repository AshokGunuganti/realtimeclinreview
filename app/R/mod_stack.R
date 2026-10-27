# Stack: several display modules on one page, top to bottom, so related views
# share a screen (e.g. the AE timeline above the AE listing). Params: items, a
# list of {module, title, params}.

stack_items <- function(params) {
  lapply(params$items, function(it) {
    def <- get0(it$module, envir = display_registry, inherits = FALSE)
    if (is.null(def)) stop("Stack item: unknown module '", it$module, "'.", call. = FALSE)
    p <- if (is.null(it$params)) list() else it$params
    if (is.null(p$title)) p$title <- if (is.null(it$title)) it$module else it$title
    list(def = def, params = p)
  })
}

stack_ui <- function(id, params, study) {
  ns <- shiny::NS(id)
  items <- stack_items(params)
  shiny::tagList(lapply(seq_along(items), function(i) {
    shiny::div(class = "stack-item", items[[i]]$def$ui(ns(paste0("i", i)), items[[i]]$params, study))
  }))
}

stack_server <- function(id, ctx, params) {
  shiny::moduleServer(id, function(input, output, session) {
    items <- stack_items(params)
    lapply(seq_along(items), function(i) items[[i]]$def$server(paste0("i", i), ctx, items[[i]]$params))
  })
}

stack_needs <- function(params, data) {
  length(params$items) > 0 && all(vapply(stack_items(params), function(it) isTRUE(it$def$needs(it$params, data)),
                                         logical(1)))
}
