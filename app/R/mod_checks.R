# Checks: the review checks implemented for the study, as the check spec
# describes them. The review team updates the spec during the study, so each
# check shows when it last changed, by whom and why, and retired checks stay
# listed (collapsed) with their reason. Checks are grouped by area, each group
# collapsed at first; a check opens the tab for its form, where its findings
# are reviewed (see check_links.R), or the review summary when no tab shows
# its form. Programmer checks are left out; they show on Data load.
# Params: areas (group order; other categories follow alphabetically).

catalogue_ui <- function(id, params, study) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(class = "act-head",
      shiny::div(shiny::div(class = "note", paste("The review checks in the check spec. The review team updates them during",
                                                  "the study; each change is kept. A check opens the tab for its form.")),
                 shiny::uiOutput(ns("headline")))),
    shiny::uiOutput(ns("groups"))
  )
}

catalogue_server <- function(id, ctx, params) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    new_keys <- shiny::reactive(new_finding_keys(ctx))
    catalogue <- shiny::reactive(check_catalogue(ctx$study, ctx$data(), new_keys(), ctx$check_tabs, ctx$tab_titles))
    output$headline <- shiny::renderUI({
      cat <- catalogue()
      active <- cat[cat$Status == "active", , drop = FALSE]
      shiny::p(class = "act-sum", sprintf("%d active checks \u00b7 %d findings \u00b7 ", nrow(active), sum(active$Findings)),
               shiny::span(class = if (sum(active$New)) "late", sprintf("%d new", sum(active$New))),
               if (any(cat$Status == "retired")) sprintf(" \u00b7 %d retired", sum(cat$Status == "retired")),
               sprintf(" \u00b7 last change %s", fmt_date(max(as.Date(cat$Changed_on), na.rm = TRUE))))
    })
    output$groups <- shiny::renderUI({
      cat <- catalogue()
      active <- cat[cat$Status == "active", , drop = FALSE]
      g <- check_groups(active, if (is.null(params$areas)) check_areas else unlist(params$areas))
      panels <- lapply(names(g), function(area) {
        x <- g[[area]]
        bslib::accordion_panel(
          value = area,
          title = shiny::div(class = "act-group", style = area_style(area), shiny::span(class = "act-count", nrow(x)),
                             shiny::span(class = "act-gtitle", area),
                             shiny::span(class = paste("note", if (sum(x$New)) "late"), group_summary(x))),
          shiny::tagList(lapply(seq_len(nrow(x)), function(i) catalogue_row(x[i, ], ns))))
      })
      retired <- cat[cat$Status == "retired", , drop = FALSE]
      if (nrow(retired)) {
        panels <- c(panels, list(bslib::accordion_panel(
          value = "retired",
          title = shiny::div(class = "act-group", shiny::span(class = "act-count", nrow(retired)),
                             shiny::span(class = "act-gtitle", "Retired checks"),
                             shiny::span(class = "note", "no longer run; kept for the record")),
          shiny::tagList(lapply(seq_len(nrow(retired)), function(i) catalogue_row(retired[i, ], ns))))))
      }
      if (!length(panels)) return(shiny::p(class = "note", "No review checks in the check spec."))
      do.call(bslib::accordion, c(list(id = ns("acc"), open = FALSE, multiple = TRUE), panels))
    })
    shiny::observeEvent(input$open, ctx$goto_check(input$open))
  })
}

# One row per review check: active ones from the checks the study runs (with
# their findings), retired ones from the check spec table.
check_catalogue <- function(study, data, new_keys, check_tabs, tab_titles) {
  s <- data$summary[data$summary$TIER != "structural", , drop = FALSE]
  f <- data$findings
  changed <- function(id, what) {
    v <- study$checks[[id]]$changed[[what]]
    if (is.null(v) || !length(v)) NA_character_ else as.character(v)
  }
  tab <- unname(check_tabs[s$ID])
  active <- data.frame(
    ID = s$ID, Name = s$NAME, Description = vapply(s$ID, function(id) study$checks[[id]]$description %|na|% "", ""),
    Category = ifelse(is.na(s$CATEGORY), "Other", s$CATEGORY), Severity = s$SEVERITY,
    Owner = ifelse(is.na(s$OWNER), "", s$OWNER), Status = "active",
    Findings = s$N, New = vapply(s$ID, function(id) sum(f$CHECKID == id & f$FINDINGKEY %in% new_keys), 0L),
    Note = if (is.null(s$ERROR)) "" else ifelse(is.na(s$ERROR), "", s$ERROR),
    Changed_on = vapply(s$ID, changed, "", what = "on"), Changed_by = vapply(s$ID, changed, "", what = "by"),
    Reason = vapply(s$ID, changed, "", what = "reason"),
    Tab = vapply(tab, function(t) if (is.na(t)) NA_character_ else tab_titles[[t]], ""),
    stringsAsFactors = FALSE, row.names = NULL)
  sp <- study$check_spec$table
  ret <- if (is.null(sp)) NULL else sp[sp$STATUS == "retired" & !sp$ID %in% s$ID, , drop = FALSE]
  if (is.null(ret) || !nrow(ret)) return(active)
  retired <- data.frame(
    ID = ret$ID, Name = ret$NAME, Description = ifelse(is.na(ret$DESCRIPTION), "", ret$DESCRIPTION),
    Category = ifelse(is.na(ret$CATEGORY), "Other", ret$CATEGORY), Severity = ret$SEVERITY,
    Owner = ifelse(is.na(ret$OWNER), "", ret$OWNER), Status = "retired", Findings = 0L, New = 0L, Note = "",
    Changed_on = as.character(ret$CHANGED_ON), Changed_by = as.character(ret$CHANGED_BY),
    Reason = as.character(ret$REASON), Tab = NA_character_, stringsAsFactors = FALSE)
  rbind(active, retired)
}

catalogue_row <- function(x, ns) {
  change <- paste0(if (x$Status == "retired") "Retired " else "Changed ",
                   if (is.na(x$Changed_on)) "" else fmt_date(x$Changed_on),
                   if (!is.na(x$Changed_by) && nzchar(x$Changed_by)) paste0(" by ", x$Changed_by),
                   if (!is.na(x$Reason) && nzchar(x$Reason)) paste0(" \u00b7 ", x$Reason))
  counts <- if (x$Status == "retired") "" else if (nzchar(x$Note)) "not running" else
    paste0(x$Findings, " finding", if (x$Findings == 1) "" else "s", if (x$New) sprintf(" \u00b7 %d new", x$New))
  opens <- if (x$Status == "retired") NULL else shiny::tags$button(
    type = "button", class = "btn btn-sm btn-outline-primary cat-open",
    onclick = sprintf("Shiny.setInputValue('%s','%s',{priority:'event'})", ns("open"), htmltools::htmlEscape(x$ID)),
    if (is.na(x$Tab)) "Findings on the review summary \u2192" else paste("Open", x$Tab, "\u2192"))
  shiny::div(
    class = paste("cat-row", if (x$Status == "retired") "retired"),
    shiny::div(class = "act-main",
      shiny::span(shiny::span(class = "mono note", x$ID), " ", shiny::strong(x$Name), " ", shiny::HTML(badge(x$Severity))),
      shiny::span(x$Description),
      shiny::span(class = "note", paste(c(if (nzchar(x$Owner)) paste("Owner:", x$Owner), change, if (nzchar(x$Note)) x$Note),
                                        collapse = " \u00b7 "))),
    shiny::div(class = "act-side", shiny::span(class = paste("note", if (x$New) "late"), counts), opens))
}
