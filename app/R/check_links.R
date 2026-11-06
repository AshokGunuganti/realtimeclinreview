# Checks tied to screens. Each active review check reads one form (check spec
# FORM); every screen that shows that form gets a "Checks on this form" card
# with the check's findings, and the check links to the first such screen in
# sidebar order. Checks with no form, or a form no screen shows, have no tab:
# their findings stay on the review summary.

# Modules whose screens show a form's data and so carry its checks.
check_tab_modules <- c("listing", "stack", "over_time", "timeline", "visit_grid", "coding_review")

# The forms a display shows: params$form, or its stacked items' forms.
display_forms <- function(d) {
  items <- lapply(d$params$items, function(it) it$params$form)
  unique(unlist(c(d$params$form, items)))
}

# Active review checks (not programmer checks) with the form each one reads.
review_check_forms <- function(study) {
  tab <- study$check_spec$table
  if (is.null(tab) || !nrow(tab)) return(stats::setNames(character(), character()))
  keep <- tab$STATUS == "active" & !tab$TIER %in% "structural" & !is.na(tab$ID)
  stats::setNames(as.character(tab$FORM[keep]), tab$ID[keep])
}

# Check ID -> display ID of the screen it links to (NA when none shows its form).
check_tab_map <- function(study, displays) {
  forms <- review_check_forms(study)
  tabs <- Filter(function(d) d$module %in% check_tab_modules, displays)
  ids <- vapply(tabs, function(d) d$id, "")
  shown <- lapply(tabs, display_forms)
  vapply(forms, function(f) {
    hit <- which(vapply(shown, function(s) !is.na(f) && f %in% s, logical(1)))
    if (length(hit)) ids[[hit[1]]] else NA_character_
  }, "")
}

# The checks a display carries: every active review check on a form it shows.
display_checks <- function(study, d) {
  if (!d$module %in% check_tab_modules) return(character())
  forms <- review_check_forms(study)
  names(forms)[!is.na(forms) & forms %in% display_forms(d)]
}

# Findings rows for the findings table: checks, new ones first, review marks.
# `record` (optional, one per finding) is its record's change status.
findings_rows <- function(ctx, f, new_keys, record = NULL) {
  o <- order(!f$FINDINGKEY %in% new_keys)
  f <- f[o, , drop = FALSE]
  x <- data.frame(Check = f$CHECKID, Status = badge(ifelse(f$FINDINGKEY %in% new_keys, "New", "")),
                  Severity = badge(f$SEVERITY), Subject = sid_link(f$SUBJID), Site = f$SITEID,
                  Where = f$ANCHOR, Field = col_label(f$FIELD, ctx$study), Value = f$VALUE,
                  Message = htmltools::htmlEscape(f$MESSAGE), Owner = f$OWNER, stringsAsFactors = FALSE)
  if (!is.null(record)) {
    rec <- unname(record)[o]
    x <- cbind(x[1:2], Record = badge(ifelse(is.na(rec), "", status_label(rec))), x[-(1:2)])
  }
  with_marks(cbind(x, mark_cols(f$FINDINGKEY, f$DEPHASH, f$SUBJID, f$VERSION)), ctx, "finding")
}

new_finding_keys <- function(ctx) {
  cmp <- rtcrcore::compare_findings(ctx$data_since()$findings, ctx$data()$findings)
  cmp$FINDINGKEY[cmp$FINDSTAT %in% c("New", "Changed")]
}

# "Checks on this form": one chip per check (findings and new counts) that
# filters the findings below; opening the tab from a check's link picks it.
form_checks_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    fill = FALSE, class = "fc-card",
    bslib::card_header(shiny::span("Checks on this form"), shiny::uiOutput(ns("headline"), inline = TRUE)),
    shiny::uiOutput(ns("chips")),
    table_card_ui(ns("fnd"), "Findings", "Select findings to mark them. A mark resets when the values it relied on change.",
                  mark_type = "finding"))
}

form_checks_server <- function(id, ctx, check_ids, tab) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    picked <- shiny::reactiveVal(NULL)
    shiny::observeEvent(ctx$check_focus(), {
      fc <- ctx$check_focus()
      if (!is.null(fc) && identical(fc$tab, tab) && fc$check %in% check_ids) picked(fc$check)
    })
    shiny::observeEvent(input$pick, picked(if (identical(input$pick, "")) NULL else input$pick))
    new_keys <- shiny::reactive(new_finding_keys(ctx))
    all_findings <- shiny::reactive({
      f <- ctx$data()$findings
      f[f$CHECKID %in% check_ids, , drop = FALSE]
    })
    output$headline <- shiny::renderUI({
      f <- all_findings()
      n_new <- sum(f$FINDINGKEY %in% new_keys())
      shiny::span(class = "note", sprintf("%d check%s \u00b7 %d findings", length(check_ids),
                                          if (length(check_ids) == 1) "" else "s", nrow(f)),
                  if (n_new) shiny::span(class = "late", sprintf(" \u00b7 %d new", n_new)))
    })
    output$chips <- shiny::renderUI({
      f <- all_findings()
      s <- ctx$data()$summary
      p <- picked()
      chip <- function(value, label, on) {
        shiny::tags$button(type = "button", class = paste("fc-chip", if (on) "on"),
                           onclick = sprintf("Shiny.setInputValue('%s','%s',{priority:'event'})", ns("pick"), value),
                           label)
      }
      shiny::div(class = "fc-chips",
        chip("", sprintf("All checks (%d)", nrow(f)), is.null(p)),
        lapply(check_ids, function(cid) {
          n <- sum(f$CHECKID == cid)
          n_new <- sum(f$CHECKID == cid & f$FINDINGKEY %in% new_keys())
          chip(cid, shiny::tagList(shiny::span(class = "mono", cid), " ", s$NAME[s$ID == cid], " \u00b7 ", n,
                                   if (n_new) shiny::span(class = "late", sprintf(" \u00b7 %d new", n_new))),
               identical(p, cid))
        }))
    })
    tbl <- shiny::reactive({
      f <- all_findings()
      p <- picked()
      if (!is.null(p)) f <- f[f$CHECKID == p, , drop = FALSE]
      findings_rows(ctx, f, new_keys())
    })
    table_card_server("fnd", tbl, ctx, paste("Findings", tab), mark_type = "finding", page_length = 10)
    list(picked = picked)
  })
}
