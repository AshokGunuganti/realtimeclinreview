# Subject profile: one subject at a glance, then form by form. A header with
# the subject's key facts and what needs a look (review-check findings by
# check); one timeline with every lane on the same study-day scale (see
# profile_chart.R); then each clinical form as a collapsed row with a one-line
# summary and its flags, opening to its records with labels, change colours
# (earlier value on hover) and the checks that flag each record.
# The view is the reviewer's own: they choose which lanes and forms to show,
# can add any numeric field of a dated form to the timeline, and can switch
# the forms to the source data as it arrived (original file, columns and
# values), which matters most for external vendor data. Choices last for the
# session. Unlike the other screens it carries no checks card: its flags come
# from every check.
# Params: facts (list of {form, field} shown in the header beside the ADSL
# dates), lanes (timeline lanes, see profile_chart.R), forms (default every
# clinical form).

profile_ui <- function(id, params, study) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(class = "pf-bar",
      shiny::selectInput(ns("subject"), "Subject", choices = NULL, width = "200px"),
      shiny::radioButtons(ns("view"), "Show forms as", c("Mapped forms" = "form", "Source data" = "source"),
                          inline = TRUE),
      shiny::tags$details(class = "pf-custom",
        shiny::tags$summary("Customise this view"),
        shiny::div(class = "pf-custom-body",
          shiny::checkboxGroupInput(ns("lanes"), "Timeline", choices = lane_choices(params$lanes),
                                    selected = unname(lane_choices(params$lanes))),
          shiny::checkboxGroupInput(ns("forms_on"), "Forms", choices = form_choices(study, profile_forms(params, study)),
                                    selected = profile_forms(params, study)),
          shiny::div(class = "pf-add",
            shiny::selectInput(ns("add_form"), "Add a measure", choices = form_choices(study, profile_forms(params, study)),
                               width = "220px"),
            shiny::selectInput(ns("add_field"), "Field", choices = NULL, width = "200px"),
            shiny::actionButton(ns("add"), "Add to timeline", class = "btn-sm"))))),
    shiny::uiOutput(ns("header")),
    if (length(params$lanes)) bslib::card(
      fill = FALSE, class = "pf-timeline",
      bslib::card_header(shiny::span("Over the study"),
                         shiny::span(class = "note", "One study-day scale \u00b7 Day 1 = first dose \u00b7 dotted line = review date")),
      shiny::uiOutput(ns("timeline"))),
    shiny::div(class = "pf-forms-head", shiny::h2(class = "section-h", "Data by form"),
               shiny::span(class = "note", "Open a form for its records. Changed values show the earlier value on hover.")),
    shiny::uiOutput(ns("forms"))
  )
}

profile_server <- function(id, ctx, params) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    forms <- profile_forms(params, ctx$study)
    shiny::observe({
      ids <- ctx$data()$adsl$SUBJID
      sel <- shiny::isolate(input$subject)
      want <- ctx$selected_subject()
      pick <- if (!is.null(want) && want %in% ids) want else if (!is.null(sel) && sel %in% ids) sel else ids[1]
      shiny::updateSelectInput(session, "subject", choices = ids, selected = pick)
    })
    subject <- shiny::reactive(shiny::req(input$subject))
    adsl_row <- shiny::reactive({
      a <- ctx$data()$adsl
      a[a$SUBJID == subject(), , drop = FALSE]
    })
    findings <- shiny::reactive(subject_findings(ctx$data(), subject()))
    pulls <- shiny::reactive(data_pulls(ctx$study, ctx$asof()))
    # The reviewer's choices: NULL until they change one, then exactly what is ticked.
    lanes_on <- shiny::reactiveVal(NULL)
    forms_on <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$lanes, lanes_on(if (is.null(input$lanes)) character() else input$lanes),
                        ignoreNULL = FALSE, ignoreInit = TRUE)
    shiny::observeEvent(input$forms_on, forms_on(if (is.null(input$forms_on)) character() else input$forms_on),
                        ignoreNULL = FALSE, ignoreInit = TRUE)
    added <- shiny::reactiveVal(list())
    all_lanes <- shiny::reactive(c(stats::setNames(params$lanes, seq_along(params$lanes)), added()))
    shown_lanes <- shiny::reactive({
      on <- lanes_on()
      if (is.null(on)) all_lanes() else all_lanes()[intersect(names(all_lanes()), on)]
    })
    shown_forms <- shiny::reactive({
      on <- forms_on()
      if (is.null(on)) forms else intersect(forms, on)
    })
    shiny::observeEvent(input$add_form, {
      shiny::updateSelectInput(session, "add_field",
                               choices = measure_fields(ctx$data()$tables[[input$add_form]], ctx$study))
    })
    shiny::observeEvent(input$add, {
      lane <- measure_lane(input$add_form, input$add_field, ctx$data()$tables[[input$add_form]], params$lanes, ctx$study)
      if (is.null(lane)) {
        shiny::showNotification("That form has no date to place its values on the timeline.", type = "warning")
        return()
      }
      key <- paste0("added", length(added()) + 1)
      on <- c(if (is.null(lanes_on())) names(all_lanes()) else lanes_on(), key)
      added(c(added(), stats::setNames(list(lane), key)))
      lanes_on(on)
      shiny::updateCheckboxGroupInput(session, "lanes", choices = lane_choices(all_lanes()), selected = on)
    })
    changes <- shiny::reactive({
      lapply(stats::setNames(forms, forms), function(f) form_changes(ctx$study, ctx$data(), f, ctx$since(), ctx$asof()))
    })

    output$header <- shiny::renderUI({
      a <- adsl_row()
      if (!nrow(a)) return(shiny::p(class = "note", "Subject not available on this review date."))
      n_changed <- sum(vapply(changes(), function(fc) sum(fc$rows$SUBJID == subject() & fc$status != "Unchanged"), 0L))
      q <- ctx$queries()
      open_q <- if (is.null(q)) 0L else sum(q$SUBJID == subject() & q$STATUS != "Closed")
      subject_header(ctx$study, ctx$data()$tables, a, params$facts, findings(),
                     header_meta(nrow(findings()), open_q, n_changed, ctx$since()))
    })

    output$timeline <- shiny::renderUI({
      a <- adsl_row()
      if (!nrow(a)) return(NULL)
      rd <- ctx$data()
      if (!length(shown_lanes())) return(shiny::p(class = "note", "No timeline lanes chosen. Tick some under Customise."))
      lanes <- lapply(unname(shown_lanes()), lane_data, tables = rd$tables, subj = subject(), adsl = rd$adsl,
                      schedule = ctx$study$schedule, thresholds = ctx$study$thresholds)
      trt <- a$TRTSDT[1]
      sp <- subject_pulls(rd$tables, forms, subject(), pulls(), trt)
      timeline_svg(lanes, if (is.na(trt)) NA else study_day(ctx$asof(), trt),
                   if (is.na(trt) || is.na(a$LSTCNTDT[1])) NA else study_day(a$LSTCNTDT[1], trt),
                   pulls = sp, since_day = if (is.na(trt)) NA else study_day(ctx$since(), trt))
    })

    output$forms <- shiny::renderUI({
      s <- subject()
      if (identical(input$view, "source")) {
        panels <- Filter(Negate(is.null), lapply(shown_forms(), source_panel, study = ctx$study, subj = s))
        if (!length(panels)) return(shiny::p(class = "note", "No source rows for this subject in the chosen forms."))
        return(do.call(bslib::accordion, c(list(id = ns("src"), class = "pf-forms", open = FALSE, multiple = TRUE), panels)))
      }
      panels <- Filter(Negate(is.null), lapply(shown_forms(), function(f) {
        fc <- changes()[[f]]
        keep <- fc$rows$SUBJID == s
        if (!any(keep)) return(NULL)
        form_panel(ctx, f, fc$rows[keep, , drop = FALSE], fc$status[keep], fc$fields, findings(), pulls(), ctx$since())
      }))
      if (!length(panels)) return(shiny::p(class = "note", "No records for this subject."))
      do.call(bslib::accordion, c(list(id = ns("acc"), class = "pf-forms", open = FALSE, multiple = TRUE), panels))
    })
  })
}

profile_forms <- function(params, study) {
  if (length(params$forms)) unlist(params$forms) else clinical_forms(study)
}

form_title <- function(study, form) nz(study$mapping$forms[[form]]$description) %|na|% form

form_choices <- function(study, forms) stats::setNames(forms, vapply(forms, function(f) form_title(study, f), ""))

# Timeline lane choices: key (position, or "addedN") named by the lane title.
lane_choices <- function(lanes) {
  keys <- if (is.null(names(lanes))) as.character(seq_along(lanes)) else names(lanes)
  stats::setNames(keys, vapply(lanes, function(l) l$title %|na|% paste(l$form, l$value), ""))
}

# Numeric fields a reviewer can add to the timeline, named by their labels.
measure_fields <- function(t, study) {
  if (is.null(t)) return(character())
  header <- c("VISITNUM", "VERSION", "AVISITN", "ADY", "ASTDY", "ATDIFF")
  vars <- names(t)[vapply(t, is.numeric, logical(1)) & !names(t) %in% header]
  stats::setNames(vars, col_label(vars, study))
}

# A series lane for a reviewer-picked field, placed by the date the study's own
# lanes use for that form, else the form's first date column.
measure_lane <- function(form, field, t, lanes, study) {
  if (is.null(t) || is.null(field) || !nzchar(field)) return(NULL)
  own <- Filter(function(l) identical(l$form, form) && (!is.null(l$date) || !is.null(l$day)), lanes)
  if (length(own)) {
    where <- own[[1]][intersect(c("date", "day"), names(own[[1]]))]
  } else {
    dates <- names(t)[vapply(t, inherits, logical(1), "Date") & !names(t) %in% c("ENTRYDT", "UPDDT", "AVAILDT")]
    if (!length(dates)) return(NULL)
    where <- list(date = dates[1])
  }
  c(list(type = "series", form = form, value = field, title = col_label(field, study), note = form_title(study, form)),
    where)
}

# The subject's rows of a form's source file, as they arrived: original
# columns and values, read from the extract or cut the study was loaded from.
source_rows <- function(study, form, subj) {
  f <- study$mapping$forms[[form]]
  src <- study$sources[[f$source]]
  if (is.null(src)) return(NULL)
  subj_field <- Filter(function(e) identical(e$variable, "SUBJID"), f$fields)
  col <- if (length(subj_field)) subj_field[[1]]$source[1] else "SUBJID"
  if (!col %in% names(src)) return(NULL)
  src[!is.na(src[[col]]) & src[[col]] == subj, , drop = FALSE]
}

source_panel <- function(form, study, subj) {
  rows <- source_rows(study, form, subj)
  if (is.null(rows) || !nrow(rows)) return(NULL)
  f <- study$mapping$forms[[form]]
  cells <- as.data.frame(lapply(rows, function(v) ifelse(is.na(v), "", htmltools::htmlEscape(v))),
                         stringsAsFactors = FALSE, check.names = FALSE)
  bslib::accordion_panel(
    value = form,
    title = shiny::div(class = "act-group",
                       shiny::span(class = "act-count", nrow(rows)),
                       shiny::span(class = "act-gtitle", form_title(study, form)),
                       shiny::span(class = "note", paste0("source file ", f$source, " \u00b7 extract ", study$extract)),
                       if (identical(f$type, "vendor")) shiny::span(class = "pf-flag pf-info", "External data")),
    html_table(cells))
}

plural <- function(n, one, many = paste0(one, "s")) sprintf("%d %s", n, if (n == 1) one else many)

header_meta <- function(n_findings, n_queries, n_changed, since) {
  paste(plural(n_findings, "open finding"), plural(n_queries, "unresolved query", "unresolved queries"),
        if (n_changed) paste(plural(n_changed, "record"), "new or updated since", fmt_date(since))
        else paste("nothing new since", fmt_date(since)), sep = " \u00b7 ")
}

# The subject's findings from review checks (programmer checks left out), with
# each check's name.
subject_findings <- function(data, subj) {
  s <- data$summary[data$summary$TIER != "structural", , drop = FALSE]
  f <- data$findings[data$findings$SUBJID == subj & data$findings$CHECKID %in% s$ID, , drop = FALSE]
  f$NAME <- s$NAME[match(f$CHECKID, s$ID)]
  f
}

flag_chip <- function(text, severity) {
  level <- if (severity == "error") "crit" else if (severity == "warning") "warn" else "info"
  shape <- c(crit = "\u25b2", warn = "\u25c6", info = "\u25cf")[[level]]
  shiny::span(class = paste("pf-flag", paste0("pf-", level)), paste(shape, text))
}

# One chip per check with error or warning findings, "Name x3" when it fires
# more than once; errors first.
flag_chips <- function(f) {
  f <- f[f$SEVERITY %in% c("error", "warning"), , drop = FALSE]
  if (!nrow(f)) return(NULL)
  by <- split(f, f$CHECKID)
  by <- by[order(vapply(by, function(x) x$SEVERITY[1] != "error", logical(1)), names(by))]
  unname(lapply(by, function(x) flag_chip(paste0(x$NAME[1], if (nrow(x) > 1) paste0(" \u00d7", nrow(x))), x$SEVERITY[1])))
}

fact_value <- function(v) {
  v <- v[!is.na(v)]
  if (!length(v)) return("\u2014")
  if (inherits(v, "Date")) fmt_date(v[1]) else as.character(fmt_cell(v[1]))
}

subject_header <- function(study, tables, a, facts, findings, meta) {
  extra <- lapply(facts, function(f) {
    t <- tables[[f$form]]
    v <- if (is.null(t) || !f$field %in% names(t)) NA else t[[f$field]][t$SUBJID == a$SUBJID]
    list(label = f$label %|na|% col_label(f$field, study), value = fact_value(v))
  })
  last <- fact_value(a$LSTCNTDT)
  if (!is.na(a$TRTSDT) && !is.na(a$LSTCNTDT)) last <- paste0(last, " \u00b7 Day ", study_day(a$LSTCNTDT, a$TRTSDT))
  cells <- c(list(list(label = "Site", value = a$SITEID)), extra,
             list(list(label = col_label("EOSSTT", study), value = fact_value(a$EOSSTT)),
                  list(label = col_label("ENRLDT", study), value = fact_value(a$ENRLDT)),
                  list(label = "First dose (Day 1)", value = fact_value(a$TRTSDT)),
                  list(label = col_label("LSTCNTDT", study), value = last)))
  chips <- flag_chips(findings)
  shiny::tags$section(
    class = "pf-head",
    shiny::div(class = "pf-id", a$SUBJID),
    shiny::div(class = "pf-facts", lapply(cells, function(c) {
      shiny::div(shiny::div(class = "pf-lab", c$label), shiny::div(class = "pf-val", c$value))
    })),
    shiny::div(class = "pf-look",
               shiny::span(class = "note", if (length(chips)) "Needs a look:" else "Nothing flagged."), chips,
               shiny::span(class = "note pf-meta", meta)))
}

form_panel <- function(ctx, form, t, status, fields, findings, pulls = NULL, since = NA) {
  f <- findings[findings$FORMOID %in% form, , drop = FALSE]
  n_changed <- sum(status != "Unchanged")
  summary <- paste0(plural(nrow(t), "record"), if (n_changed) sprintf(" \u00b7 %d new or updated", n_changed),
                    if (nrow(f)) paste0(" \u00b7 ", plural(nrow(f), "finding")))
  by_key <- split(f$CHECKID, paste0(f$FORMOID, ":", f$RECID))
  cells <- label_columns(mark_changed_cells(record_cells(t), t$RECID, fields), ctx$study)
  lead <- data.frame(Status = badge(ifelse(status == "Unchanged", "", status_label(status))),
                     stringsAsFactors = FALSE, check.names = FALSE)
  if (!is.null(pulls) && nrow(pulls)) {
    now <- pull_of(t$AVAILDT, pulls)
    recent <- !is.na(now) & !is.na(since) & pulls$DATE[match(now, pulls$PULL)] > as.Date(since)
    lead$`Data pull` <- sprintf('<span class="pull-chip%s">%s</span>', ifelse(recent, " recent", ""),
                                htmltools::htmlEscape(record_pull_text(t$ENTRYDT, t$AVAILDT, pulls)))
    o <- order(-now, na.last = TRUE)                                       # latest pull first
    lead <- lead[o, , drop = FALSE]
    cells <- cells[o, , drop = FALSE]
    t <- t[o, , drop = FALSE]
    status <- status[o]
  }
  cells <- cbind(lead, cells)
  cells$`Flagged by` <- vapply(paste0(form, ":", t$RECID), function(k) paste(unique(by_key[[k]]), collapse = ", "), "",
                               USE.NAMES = FALSE)
  bslib::accordion_panel(
    value = form,
    title = shiny::div(class = "act-group",
                       shiny::span(class = "act-count", nrow(t)),
                       shiny::span(class = "act-gtitle", form_title(ctx$study, form)),
                       shiny::span(class = paste("note", if (n_changed) "late"), summary),
                       shiny::span(class = "pf-chips", flag_chips(f))),
    html_table(cells, row_class = row_classes(status)))
}
