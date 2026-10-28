# Subject panel: opens over any screen when a subject ID is clicked or
# searched. Shows what is new for the subject first (changes since the
# earlier date, open clinical flags), then all of its data. "Mark reviewed &
# next" marks every item of the subject still to review and opens the next
# subject in the review queue. Closing the panel leaves the screen as it was.

subject_panel_ui <- function(id = "panel") {
  ns <- shiny::NS(id)
  shiny::div(
    class = "offcanvas offcanvas-end subj-panel", tabindex = "-1", id = ns("oc"),
    `aria-labelledby` = ns("title"),
    shiny::div(class = "offcanvas-header",
               shiny::div(id = ns("title"), class = "flex-grow-1", shiny::uiOutput(ns("head"))),
               shiny::tags$button(type = "button", class = "btn-close", `data-bs-dismiss` = "offcanvas",
                                  `aria-label` = "Close")),
    shiny::div(class = "offcanvas-body", shiny::uiOutput(ns("body"))),
    shiny::div(class = "panel-foot", review_actions(ns))
  )
}

# Outcome, comment and the buttons that move through the queue; shared by the
# subject panel and the review inbox. The kb-* classes take the n/p/m keys.
review_actions <- function(ns) {
  shiny::tagList(
    shiny::selectInput(ns("outcome"), NULL, rtcrcore_outcomes(), width = "130px"),
    shiny::div(class = "comment", shiny::textInput(ns("comment"), NULL, placeholder = "Comment (optional)", width = "100%")),
    shiny::actionButton(ns("mark"), shiny::HTML("Reviewed, next &rsaquo;"), class = "btn-sm btn-primary kb-mark"),
    shiny::actionButton(ns("prev"), shiny::HTML("&lsaquo;"), class = "btn-sm kb-prev", title = "Previous subject (p)",
                        `aria-label` = "Previous subject"),
    shiny::actionButton(ns("next"), shiny::HTML("&rsaquo;"), class = "btn-sm kb-next", title = "Next subject (n)",
                        `aria-label` = "Next subject"),
    shiny::actionButton(ns("full"), "Full profile", class = "btn-sm btn-link"),
    shiny::span(class = "kb-hint", "Keys: m reviewed & next, n next, p previous")
  )
}

# timeline: list of {form, day, label} for the timeline strip (the subject
# profile display's `timeline`).
subject_panel_server <- function(id, ctx, timeline = list()) {
  shiny::moduleServer(id, function(input, output, session) {
    subj <- ctx$panel_subject
    open <- function(s) {
      subj(s)
      ctx$selected_subject(s)
      session$sendCustomMessage("subjpanel", list(id = session$ns("oc"), show = TRUE))
    }
    output$head <- shiny::renderUI({
      s <- shiny::req(subj())
      panel_head(ctx, s)
    })
    output$body <- shiny::renderUI({
      s <- shiny::req(subj())
      panel_body(ctx, s, timeline)
    })
    shiny::outputOptions(output, "head", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "body", suspendWhenHidden = FALSE)

    review_controls(input, session, ctx, subj, open, on_full = function() {
      session$sendCustomMessage("subjpanel", list(id = session$ns("oc"), show = FALSE))
    })
    list(open = open)
  })
}

# Mark reviewed & next, next, previous and full profile for a subject view.
# `subj()` is the subject shown and `open(s)` shows another.
review_controls <- function(input, session, ctx, subj, open, on_full = function() NULL) {
  shiny::observeEvent(input$mark, {
    s <- shiny::req(subj())
    before <- shiny::isolate(ctx$review_queue())      # order before marking, so "next" is the one below
    n <- tryCatch(mark_subject(ctx, ctx$review_items(), s, input$outcome, nz(input$comment)),
                  error = function(e) {
                    shiny::showNotification(conditionMessage(e), type = "error")
                    NA
                  })
    if (is.na(n)) return()
    ctx$bump_marks()
    shiny::updateTextInput(session, "comment", value = "")
    shiny::updateSelectInput(session, "outcome", selected = "no action")
    nxt <- next_in_queue(before, s)
    shiny::showNotification(sprintf("%s: %d item(s) marked %s.", s, n, input$outcome), type = "message")
    if (is.null(nxt)) {
      shiny::showNotification("Nothing left to review in this window. You can sign off the review.",
                              type = "message", duration = 8)
    } else {
      open(nxt)
    }
  })
  step <- function(k) {
    q <- ctx$review_queue()
    s <- subj()
    nxt <- if (!is.null(s) && s %in% q$SUBJID) next_in_queue(q, s, k, to_review = FALSE) else next_in_queue(q, NULL, k)
    if (is.null(nxt)) shiny::showNotification("No other subject in the review queue.", type = "warning") else open(nxt)
  }
  shiny::observeEvent(input$`next`, step(1))
  shiny::observeEvent(input$prev, step(-1))
  shiny::observeEvent(input$full, {
    on_full()
    ctx$open_profile(subj())
  })
}

panel_head <- function(ctx, s) {
  a <- ctx$data()$adsl
  a <- a[a$SUBJID == s, , drop = FALSE]
  q <- ctx$review_queue()
  k <- match(s, q$SUBJID)
  todo <- q$SUBJID[q$STATUS == "To review"]
  pos <- if (is.na(k)) "Not in the review queue" else if (s %in% todo) {
    sprintf("%d of %d to review", match(s, todo), length(todo))
  } else "Reviewed in this window"
  shiny::tagList(
    shiny::div(class = "panel-id",
               shiny::span(class = "sid-big", s),
               if (nrow(a)) shiny::HTML(badge(paste("Site", a$SITEID))),
               if (nrow(a)) shiny::HTML(badge(tools::toTitleCase(tolower(a$EOSSTT)))),
               if (!is.na(k)) shiny::HTML(badge(q$STATUS[k]))),
    shiny::div(class = "note", if (!nrow(a)) "Subject not in the data on this review date." else paste(c(
      if (!is.null(a$ENRLDT)) paste("Enrolled", fmt_date(a$ENRLDT)),
      if (!is.null(a$TRTSDT) && !is.na(a$TRTSDT)) paste("first dose", fmt_date(a$TRTSDT)),
      if (!is.null(a$TRTSDT) && !is.na(a$TRTSDT)) paste("study day", study_day(ctx$asof(), a$TRTSDT)),
      if (!is.null(a$DCSREAS) && !is.na(a$DCSREAS)) paste("discontinued:", a$DCSREAS),
      pos), collapse = " \u00b7 "))
  )
}

panel_body <- function(ctx, s, timeline = list()) {
  rd <- ctx$data()
  since <- ctx$since()
  items <- ctx$review_items()
  items <- items[items$SUBJID == s, , drop = FALSE]
  chg <- ctx$changes()
  chg <- chg[chg$SUBJID == s, , drop = FALSE]
  flags <- clinical_findings(rd)
  flags <- flags[flags$SUBJID == s, , drop = FALSE]
  other <- findings_by_tier(rd)
  other <- other[other$SUBJID == s, , drop = FALSE]
  q <- ctx$queries()
  q <- if (is.null(q)) NULL else q[q$SUBJID == s & q$STATUS != "Closed", , drop = FALSE]

  sections <- list()
  if (length(timeline)) sections$tl <- panel_timeline(rd, s, timeline, chg)
  flag_table <- function(f) {
    m <- rtcrcore::apply_marks(ctx$con, ctx$study$id, data.frame(FINDINGKEY = f$FINDINGKEY, DEPHASH = f$DEPHASH),
                               "finding")
    html_table(data.frame(Check = f$CHECKID, Where = htmltools::htmlEscape(f$ANCHOR),
                          Value = htmltools::htmlEscape(f$VALUE), Message = htmltools::htmlEscape(f$MESSAGE),
                          Review = badge(m$REVIEW_STATUS), check.names = FALSE, stringsAsFactors = FALSE))
  }
  fresh <- flags[flags$FINDINGKEY %in% items$KEY, , drop = FALSE]
  if (nrow(fresh)) {
    sections$flags <- panel_section("Clinical flags", badge(sprintf("%d new", nrow(fresh))), TRUE, flag_table(fresh))
  }
  for (f in unique(chg$FORMOID)) {
    x <- chg[chg$FORMOID == f, , drop = FALSE]
    n <- table(factor(x$RECSTAT, levels = c("New", "Changed", "Deleted")))
    n <- n[n > 0]
    lab <- status_label(names(n))
    sections[[paste0("chg_", f)]] <- panel_section(
      f, paste(sprintf('<span class="badge-s %s">%d %s</span>', badge_class[lab], as.integer(n), tolower(lab)),
               collapse = " "), TRUE,
      changed_records(ctx, f, s, x, since))
  }
  if (!nrow(chg) && !nrow(fresh)) {
    sections$none <- shiny::p(class = "note", sprintf("No new or updated data since %s.", fmt_date(since)))
  } else if (nrow(chg)) {
    sections$legend <- change_legend()
  }
  if (!is.null(q) && nrow(q)) {
    sections$q <- panel_section("Unresolved queries", badge(sprintf("%d open", nrow(q))), FALSE, html_table(
      data.frame(Form = q$QRYFORM, Folder = htmltools::htmlEscape(q$FOLDER), Text = htmltools::htmlEscape(q$QRYTEXT),
                 Status = badge(q$STATUS), `Age (d)` = q$AGE, check.names = FALSE, stringsAsFactors = FALSE)))
  }
  earlier <- flags[!flags$FINDINGKEY %in% items$KEY, , drop = FALSE]
  if (nrow(earlier)) {
    sections$earlier <- shiny::tags$details(class = "panel-all", shiny::tags$summary(
      shiny::span("Earlier clinical flags"), shiny::span(class = "note", sprintf("%d open", nrow(earlier)))),
      flag_table(earlier))
  }
  if (nrow(other)) {
    sections$dq <- panel_section("Data findings", badge(sprintf("%d open", nrow(other))), FALSE, html_table(
      data.frame(Check = other$CHECKID, Where = htmltools::htmlEscape(other$ANCHOR),
                 Message = htmltools::htmlEscape(other$MESSAGE), stringsAsFactors = FALSE)))
  }
  all <- lapply(clinical_forms(ctx$study), function(f) {
    t <- rd$tables[[f]]
    t <- t[t$SUBJID == s, , drop = FALSE]
    if (!nrow(t)) return(NULL)
    st <- chg$RECSTAT[chg$FORMOID == f][match(t$RECID, chg$RECID[chg$FORMOID == f])]
    shiny::tags$details(class = "panel-all",
                        shiny::tags$summary(shiny::span(f), shiny::span(class = "note", sprintf("%d record(s)", nrow(t)))),
                        html_table(record_cells(t), row_class = row_classes(st)))
  })
  shiny::tagList(
    sections,
    shiny::h6(class = "panel-h", "All data"),
    all
  )
}

panel_section <- function(title, right, is_new, content) {
  shiny::div(class = paste("panel-sec", if (is_new) "is-new"),
             shiny::div(class = "panel-sec-h", shiny::span(title), shiny::HTML(right)),
             content)
}

# New, updated and deleted records of one form for one subject, coloured by
# status; changed values are marked and show the earlier value on hover.
changed_records <- function(ctx, form, s, x, since) {
  fc <- form_changes(ctx$study, ctx$data(), form, since, ctx$asof())
  keep <- fc$rows$SUBJID == s & fc$rows$RECID %in% x$RECID
  t <- fc$rows[keep, , drop = FALSE]
  st <- fc$status[keep]
  cells <- mark_changed_cells(record_cells(t), t$RECID, fc$fields)
  cells <- cbind(data.frame(Status = badge(status_label(st)), stringsAsFactors = FALSE), cells)
  html_table(cells, row_class = row_classes(st))
}

row_classes <- function(status) {
  out <- unname(c(New = "row-new", Changed = "row-upd", Deleted = "row-del")[status])
  out[is.na(out)] <- ""
  out
}

# Display cells for records: collected and derived fields, header fields left out.
record_cells <- function(t) {
  drop <- c("STUDYID", "SITEID", "SUBJID", "FORMOID", "VISITNUM", "RECID", "ENTRYDT", "UPDDT", "AVAILDT", "VERSION",
            "DELFL")
  t <- t[setdiff(names(t), drop)]
  if ("FOLDER" %in% names(t) && all(is.na(t$FOLDER))) t$FOLDER <- NULL
  t[] <- lapply(t, function(v) {
    v <- fmt_cell(v)
    v <- htmltools::htmlEscape(as.character(v))
    v[is.na(v)] <- ""
    v
  })
  t
}

html_table <- function(d, row_class = NULL) {
  if (!nrow(d)) return(shiny::p(class = "note", "None."))
  rows <- lapply(seq_len(nrow(d)), function(i) {
    shiny::tags$tr(class = if (!is.null(row_class) && nzchar(row_class[i])) row_class[i],
                   lapply(d[i, , drop = TRUE], function(v) shiny::tags$td(shiny::HTML(as.character(v)))))
  })
  shiny::div(class = "panel-tbl", shiny::tags$table(
    class = "table table-sm",
    shiny::tags$thead(shiny::tags$tr(lapply(names(d), shiny::tags$th))),
    shiny::tags$tbody(rows)))
}

# Events by study day, one row per form; points new or changed in the window stand out.
panel_timeline <- function(rd, s, timeline, chg) {
  ev <- do.call(rbind, lapply(timeline, function(it) {
    t <- rd$tables[[it$form]]
    if (is.null(t) || !it$day %in% names(t)) return(NULL)
    t <- t[t$SUBJID == s, , drop = FALSE]
    if (!nrow(t)) return(NULL)
    data.frame(FORM = it$form, DAY = as.numeric(t[[it$day]]), LABEL = as.character(t[[it$label]]),
               NEW = t$RECID %in% chg$RECID[chg$FORMOID == it$form], stringsAsFactors = FALSE)
  }))
  if (is.null(ev)) return(NULL)
  ev <- ev[!is.na(ev$DAY), , drop = FALSE]
  if (!nrow(ev)) return(NULL)
  forms <- unique(ev$FORM)
  lo <- min(ev$DAY, 1)
  hi <- max(ev$DAY, 1)
  w <- 520
  left <- 44
  x <- function(d) left + (d - lo) / max(hi - lo, 1) * (w - left - 10)
  y <- function(f) 14 + 22 * (match(f, forms) - 1)
  h <- 22 * length(forms) + 18
  svg <- paste0(
    sprintf('<svg viewBox="0 0 %d %d" width="100%%" role="img" aria-label="Timeline by study day">', w, h),
    sprintf('<line x1="%.1f" x2="%.1f" y1="4" y2="%d" class="tl-day1"/>', x(1), x(1), h - 14),
    paste(sprintf('<text x="0" y="%d" class="tl-lab">%s</text>', y(forms) + 4, forms), collapse = ""),
    paste(sprintf('<line x1="%d" x2="%d" y1="%d" y2="%d" class="tl-axis"/>', left, w - 10, y(forms), y(forms)),
          collapse = ""),
    paste(sprintf('<circle cx="%.1f" cy="%d" r="%d" class="%s"><title>%s: %s (day %g)</title></circle>',
                  x(ev$DAY), y(ev$FORM), ifelse(ev$NEW, 6, 4), ifelse(ev$NEW, "tl-new", "tl-pt"), ev$FORM,
                  htmltools::htmlEscape(ev$LABEL), ev$DAY), collapse = ""),
    sprintf('<text x="%d" y="%d" class="tl-lab">Day %g</text>', left, h - 2, lo),
    sprintf('<text x="%d" y="%d" class="tl-lab" text-anchor="end">Day %g</text>', w - 10, h - 2, hi),
    "</svg>")
  shiny::div(class = "panel-tl", shiny::HTML(svg),
             shiny::div(class = "note", "Study day from first dose. Larger blue points are new or changed."))
}

fmt_date <- function(d) format(as.Date(d), "%d %b %Y")
