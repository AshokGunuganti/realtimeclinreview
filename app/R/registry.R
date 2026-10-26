# Display registry. A display module is a UI function, a server function and a
# `needs` test; the app shows a screen only when its data exists, so a study
# with no PK simply has no PK screen. Studies can register plug-in modules.

display_registry <- new.env(parent = emptyenv())

#' Register a display module
#'
#' @param name Module name, used as `module` in study.yml `displays`.
#' @param ui `function(id, params, study)` returning UI.
#' @param server `function(id, ctx, params)`.
#' @param needs `function(params, data)` returning `TRUE` when the module's
#'   forms and fields exist in `data` (review data at the extract date).
#' @return `name`, invisibly.
#' @export
register_display <- function(name, ui, server, needs = function(params, data) TRUE) {
  stopifnot(is.character(name), length(name) == 1, is.function(ui), is.function(server), is.function(needs))
  assign(name, list(ui = ui, server = server, needs = needs), envir = display_registry)
  invisible(name)
}

#' Registered display modules
#' @return Character vector of module names.
#' @export
display_modules <- function() sort(ls(display_registry))

# Displays from study.yml whose module exists and whose data needs are met.
resolve_displays <- function(config, data) {
  out <- list()
  for (d in config) {
    def <- get0(d$module, envir = display_registry, inherits = FALSE)
    if (is.null(def)) stop("Display ", d$id, ": unknown module '", d$module, "'.", call. = FALSE)
    params <- if (is.null(d$params)) list() else d$params
    if (!isTRUE(def$needs(params, data))) next
    title <- if (is.null(d$title)) d$id else d$title
    if (is.null(params$title)) params$title <- title         # modules title their cards with it
    out[[d$id]] <- list(id = d$id, title = title, module = d$module,
                        params = params, def = def)
  }
  out
}

has_fields <- function(data, form, fields = character()) {
  t <- data$tables[[form]]
  !is.null(t) && all(unlist(fields) %in% names(t))
}

register_library_displays <- function() {
  register_display("whats_new", whatsnew_ui, whatsnew_server)
  register_display("listing", listing_ui, listing_server, function(p, data) has_fields(data, p$form, p$columns))
  register_display("check_results", checks_ui, checks_server, function(p, data) nrow(data$summary) > 0)
  register_display("over_time", overtime_ui, overtime_server, function(p, data) has_fields(data, p$form, p$value))
  register_display("visit_grid", visitgrid_ui, visitgrid_server,
                   function(p, data) has_fields(data, p$form, p$date) && !is.null(data$adsl))
  register_display("timeline", timeline_ui, timeline_server,
                   function(p, data) has_fields(data, p$form, c(p$day, p$label, p$color)))
  register_display("subject_profile", profile_ui, profile_server, function(p, data) !is.null(data$adsl))
}

.onLoad <- function(libname, pkgname) {
  register_library_displays()
}
