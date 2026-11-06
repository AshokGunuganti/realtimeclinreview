# Review summary: the high-level view of the review. First "New and updated
# data", the check that runs across every clinical form: one line per form
# with records new, updated or deleted since the earlier date; then the check
# spec's checks grouped by the area they cover (safety, efficacy, dosing,
# visits, disposition, data). The screen opens at the high level: collapsed
# groups, each with its counts. A check opens its tab (the screen for its
# form, see check_links.R); a check with no tab lists its findings here, with
# the record's own change status. A form lists its changed records, with the
# other checks that flag each one. Params: tiers (check tiers to show) or
# exclude_tiers (tiers to leave out), default all; areas (group order; other
# categories follow alphabetically).

check_areas <- c("Safety", "Efficacy", "Dosing", "Visits", "Disposition", "Data")
changes_area <- "New and updated data"

summary_ui <- function(id, params, study) {
  ns <- shiny::NS(id)
  view_is <- function(v) sprintf("output['%s'] == '%s'", ns("view"), v)
  shiny::tagList(
    shiny::div(class = "act-head",
      shiny::div(shiny::div(class = "note", "New and updated data across every clinical form, then the review checks by area. A check opens its tab."),
                 shiny::uiOutput(ns("headline")))),
    shiny::uiOutput(ns("groups")),
    shiny::div(class = "chk-showing", shiny::uiOutput(ns("showing"))),
    shiny::conditionalPanel(view_is("findings"),
      table_card_ui(ns("fnd"), "Findings", "Select findings to mark them. A mark resets when the values it relied on change.",
                    mark_type = "finding")),
    shiny::conditionalPanel(sprintf("(output['%s'] || '').indexOf('form:') === 0", ns("view")),
                            shiny::div(class = "listing-tools", change_legend())),
    lapply(clinical_forms(study), function(f) {
      shiny::conditionalPanel(view_is(paste0("form:", f)),
        listing_ui(ns(paste0("rec_", f)), list(form = f, title = paste(f, "\u2014 new and updated records"), show = "changes",
                                               tools = FALSE, flagged_by = TRUE), study))
    })
  )
}

summary_server <- function(id, ctx, params) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    view <- shiny::reactiveVal("findings")
    picked <- shiny::reactive(if (startsWith(view(), "check:")) sub("^check:", "", view()) else NULL)
    forms <- clinical_forms(ctx$study)
    recs <- lapply(stats::setNames(forms, forms), function(f) {
      listing_server(paste0("rec_", f), ctx, list(form = f, show = "changes", tools = FALSE, flagged_by = TRUE))
    })
    form_counts <- shiny::reactive({
      n <- lapply(recs, function(r) r$status_counts())
      d <- data.frame(Form = forms, New = vapply(n, `[[`, 0L, "New"), Updated = vapply(n, `[[`, 0L, "Changed"),
                      Deleted = vapply(n, `[[`, 0L, "Deleted"), stringsAsFactors = FALSE)
      d$Total <- d$New + d$Updated + d$Deleted
      d$Description <- vapply(forms, function(f) {
        desc <- nz(ctx$study$mapping$forms[[f]]$description)
        if (is.na(desc)) f else desc
      }, "")
      d
    })
    changed_keys <- shiny::reactive(unlist(unname(lapply(recs, function(r) r$changed_keys()))))
    in_scope <- function(tier) {
      (is.null(params$tiers) | tier %in% unlist(params$tiers)) & !tier %in% unlist(params$exclude_tiers)
    }
    new_keys <- shiny::reactive(new_finding_keys(ctx))
    summary <- shiny::reactive({
      s <- ctx$data()$summary
      s <- s[in_scope(s$TIER), , drop = FALSE]
      new <- new_keys()
      f <- ctx$data()$findings
      data.frame(ID = s$ID, Name = s$NAME, Category = ifelse(is.na(s$CATEGORY), "Other", s$CATEGORY),
                 Severity = s$SEVERITY, Owner = ifelse(is.na(s$OWNER), "", s$OWNER), Tier = s$TIER, Findings = s$N,
                 New = vapply(s$ID, function(id) sum(f$CHECKID == id & f$FINDINGKEY %in% new), 0L),
                 Result = s$RESULT, Note = if (is.null(s$ERROR)) "" else ifelse(is.na(s$ERROR), "", s$ERROR),
                 stringsAsFactors = FALSE)
    })

    output$headline <- shiny::renderUI({
      s <- summary()
      not_run <- sum(nzchar(s$Note))
      shiny::p(class = "act-sum", sprintf("%d new or updated records \u00b7 ", sum(form_counts()$Total)),
               sprintf("%d checks \u00b7 %d findings \u00b7 ", nrow(s), sum(s$Findings)),
               shiny::span(class = if (sum(s$New)) "late", sprintf("%d new", sum(s$New))),
               if (not_run) shiny::span(class = "late", sprintf(" \u00b7 %d not running", not_run)))
    })

    output$groups <- shiny::renderUI({
      g <- check_groups(summary(), if (is.null(params$areas)) check_areas else unlist(params$areas))
      fc <- form_counts()
      changed <- fc[fc$Total > 0, , drop = FALSE]
      first <- bslib::accordion_panel(
        value = changes_area,
        title = shiny::div(class = "act-group", style = area_style(changes_area),
          shiny::span(class = "act-count", sum(fc$Total)),
          shiny::span(class = "act-gtitle", changes_area),
          shiny::span(class = paste("note", if (sum(fc$Total)) "late"), changes_summary(fc))),
        if (!nrow(changed)) shiny::p(class = "note", "No new or updated records in this window.") else
          shiny::tagList(lapply(seq_len(nrow(changed)), function(i) form_row(changed[i, ], ns))))
      panels <- lapply(names(g), function(area) {
        x <- g[[area]]
        bslib::accordion_panel(
          value = area,
          title = shiny::div(class = "act-group", style = area_style(area),
            shiny::span(class = "act-count", sum(x$Findings)),
            shiny::span(class = "act-gtitle", area),
            shiny::span(class = paste("note", if (sum(x$New)) "late"), group_summary(x))),
          shiny::tagList(lapply(seq_len(nrow(x)), function(i) check_row(x[i, ], ns, tab_title(ctx, x$ID[i]))))
        )
      })
      do.call(bslib::accordion, c(list(id = ns("acc"), open = FALSE, multiple = TRUE), list(first), panels))
    })

    shiny::observeEvent(input$pick, {
      tab <- ctx$check_tabs[input$pick]
      if (!is.na(tab)) ctx$goto_check(input$pick) else view(paste0("check:", input$pick))
    })
    shiny::observeEvent(ctx$check_focus(), {
      fc <- ctx$check_focus()
      if (!is.null(fc) && identical(fc$tab, id)) view(paste0("check:", fc$check))
    })
    shiny::observeEvent(input$pick_form, view(paste0("form:", input$pick_form)))
    shiny::observeEvent(input$all, view("findings"))
    output$view <- shiny::renderText(view())
    shiny::outputOptions(output, "view", suspendWhenHidden = FALSE)

    output$showing <- shiny::renderUI({
      v <- view()
      back <- shiny::actionLink(ns("all"), "back to the summary")
      if (startsWith(v, "form:")) {
        return(shiny::span(class = "note", "Showing new and updated ", shiny::strong(sub("^form:", "", v)),
                           " records \u00b7 ", back))
      }
      p <- picked()
      s <- summary()
      if (is.null(p) || !p %in% s$ID) {
        ids <- untabbed(ctx, s$ID)
        return(shiny::span(class = "note", if (length(ids)) paste("Findings from checks with no tab of their own:",
                                                                 paste(ids, collapse = ", ")) else
                             "Every check opens its own tab."))
      }
      shiny::span(class = "note", "Showing ", shiny::strong(paste(p, s$Name[s$ID == p])), " \u00b7 ", back)
    })

    findings <- shiny::reactive({
      f <- ctx$data()$findings
      p <- picked()
      keep <- if (is.null(p)) untabbed(ctx, summary()$ID) else p
      f[f$CHECKID %in% keep, , drop = FALSE]
    })
    tbl <- shiny::reactive({
      f <- findings()
      findings_rows(ctx, f, new_keys(), record = changed_keys()[paste0(f$FORMOID, ":", f$RECID)])
    })
    table_card_server("fnd", tbl, ctx, "Findings", mark_type = "finding", page_length = 15)
  })
}

# Checks split by area: `areas` first in their order, any other category after,
# alphabetically. Within an area, checks with new findings come first, then
# those with any findings, then the rest, keeping spec order otherwise.
check_groups <- function(s, areas = check_areas) {
  cats <- unique(s$Category)
  order_cats <- c(intersect(areas, cats), sort(setdiff(cats, areas)))
  stats::setNames(lapply(order_cats, function(a) {
    x <- s[s$Category == a, , drop = FALSE]
    x[order(-(x$New > 0), -(x$Findings > 0)), , drop = FALSE]
  }), order_cats)
}

changes_summary <- function(fc) {
  if (!sum(fc$Total)) return("nothing new")
  parts <- c(New = sum(fc$New), Updated = sum(fc$Updated), Deleted = sum(fc$Deleted))
  parts <- parts[parts > 0]
  paste0(sum(fc$Total > 0), " form", if (sum(fc$Total > 0) == 1) "" else "s", " \u00b7 ",
         paste(parts, tolower(names(parts)), collapse = " \u00b7 "))
}

form_row <- function(x, ns) {
  parts <- c(New = x$New, Updated = x$Updated, Deleted = x$Deleted)
  parts <- parts[parts > 0]
  shiny::tags$button(
    type = "button", class = "act-row",
    onclick = sprintf("Shiny.setInputValue('%s','%s',{priority:'event'})", ns("pick_form"), htmltools::htmlEscape(x$Form)),
    shiny::span(class = "act-main",
      shiny::span(shiny::span(class = "mono note", x$Form), " ", x$Description)),
    shiny::span(class = "act-side",
      shiny::span(class = "note late", paste(parts, tolower(names(parts)), collapse = " \u00b7 "))))
}

group_summary <- function(x) {
  not_run <- sum(nzchar(x$Note))
  paste0(sprintf("%d check%s", nrow(x), if (nrow(x) == 1) "" else "s"),
         if (sum(x$New)) sprintf(" \u00b7 %d new", sum(x$New)),
         if (not_run) sprintf(" \u00b7 %d not running", not_run))
}

# Review checks with no screen of their own; their findings stay here.
untabbed <- function(ctx, ids) ids[is.na(ctx$check_tabs[ids])]

# The screen a check opens, by title; NA when it has no tab.
tab_title <- function(ctx, check_id) {
  tab <- ctx$check_tabs[check_id]
  if (is.na(tab)) NA_character_ else ctx$tab_titles[[tab]]
}

check_row <- function(x, ns, opens = NA_character_) {
  findings <- paste0(x$Findings, " finding", if (x$Findings == 1) "" else "s",
                     if (x$New) sprintf(" \u00b7 %d new", x$New))
  where <- if (is.na(opens)) "findings below" else paste("opens", opens, "\u2192")
  shiny::tags$button(
    type = "button", class = "act-row",
    onclick = sprintf("Shiny.setInputValue('%s','%s',{priority:'event'})", ns("pick"), htmltools::htmlEscape(x$ID)),
    shiny::span(class = "act-main",
      shiny::span(shiny::span(class = "mono note", x$ID), " ", x$Name),
      shiny::span(class = "note", paste(c(if (nzchar(x$Owner)) x$Owner, if (nzchar(x$Note)) x$Note, where),
                                        collapse = " \u00b7 "))),
    shiny::span(class = "act-side",
      shiny::HTML(badge(x$Severity)),
      shiny::span(class = paste("note", if (x$New) "late"), if (nzchar(x$Note)) "not running" else findings)))
}
