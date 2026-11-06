# Action items and the study team. The action log opens at the high level:
# collapsed groups (overdue, due in the next 7 days, due later, closed
# recently), each a one-line summary; opening a group lists its actions one
# line each, and an action opens in a panel with its history and an update
# form. Only the owner closes an action; an owner without a login is closed
# for in a meeting, with a note (rtcrcore::update_action()).
#
# Actions are live follow-ups, so due dates are judged against today, not the
# review date: reviewing an earlier cut does not make every action overdue.

action_areas <- c("Conduct", "Dosing", "Safety", "Efficacy", "Data entry", "Data quality")
action_attach <- c(Study = "study", Site = "site", Subject = "subject", Record = "record", Finding = "finding")
action_status_choices <- c("open", "in progress", "closed")
due_soon_days <- 7
today <- function() Sys.Date()
closed_recent_days <- 14

actions_ui <- function(id, params, study) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(class = "act-head",
      shiny::div(shiny::div(class = "note", "Raised in weekly review meetings. Owners close their own items. Due dates as of today."),
                 shiny::uiOutput(ns("summary"))),
      shiny::actionButton(ns("new"), "+ New action", class = "btn-primary")),
    shiny::uiOutput(ns("groups"))
  )
}

actions_server <- function(id, ctx, params) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    version <- shiny::reactiveVal(0)
    bump <- function() version(version() + 1)
    picked <- shiny::reactiveVal(NULL)
    team <- shiny::reactive({
      version()
      rtcrcore::team_members(ctx$con, ctx$study$id, active_only = FALSE)
    })
    log <- shiny::reactive({
      version()
      site_actions(rtcrcore::action_log(ctx$con, ctx$study$id, asof = today()), ctx$site(), ctx$data()$adsl)
    })
    open_meetings <- shiny::reactive({
      version()
      m <- rtcrcore::meetings(ctx$con, ctx$study$id)
      m[m$STATUS == "open", , drop = FALSE]
    })
    groups <- shiny::reactive({
      version()
      action_groups(log(), today(), last_meeting_date(ctx$con, ctx$study$id))
    })

    output$summary <- shiny::renderUI({
      l <- log()
      late <- sum(l$OVERDUE)
      shiny::p(class = "act-sum", sprintf("%d open", sum(l$STATUS != "closed")), " \u00b7 ",
               shiny::span(class = if (late) "late", sprintf("%d overdue", late)), " \u00b7 ",
               sprintf("%d %s", nrow(groups()$closed$rows), tolower(groups()$closed$label)))
    })

    output$groups <- shiny::renderUI({
      g <- groups()
      if (!nrow(log())) return(shiny::p(class = "note", "No actions yet. Raise one with + New action."))
      panels <- lapply(names(g), function(k) {
        x <- g[[k]]
        bslib::accordion_panel(
          value = k,
          title = shiny::div(class = "act-group",
            shiny::span(class = "act-count", nrow(x$rows)),
            shiny::span(class = "act-gtitle", x$label),
            shiny::span(class = paste("note", if (k == "overdue" && nrow(x$rows)) "late"), x$summary)),
          if (!nrow(x$rows)) shiny::p(class = "note", "None.") else
            shiny::tagList(lapply(seq_len(nrow(x$rows)), function(i) action_row(x$rows[i, ], ns)))
        )
      })
      do.call(bslib::accordion, c(list(id = ns("acc"), open = FALSE, multiple = FALSE), panels))
    })

    shiny::observeEvent(input$pick, {
      picked(input$pick)
      show_action(input$pick)
    })

    show_action <- function(aid) {
      a <- log()[log()$ACTIONID == aid, ]
      if (!nrow(a)) return()
      h <- rtcrcore::action_history(ctx$con, ctx$study$id, aid)
      tm <- team()
      owner <- tm[tm$MEMBERID == a$OWNER, ]
      no_login <- !nrow(owner) || is.na(owner$LOGIN) || !nzchar(owner$LOGIN)
      is_owner <- !no_login && identical(owner$LOGIN, ctx$user)
      mtg <- open_meetings()
      shiny::showModal(shiny::modalDialog(
        title = shiny::span(shiny::span(class = "mono note", a$ACTIONID), " ", a$TITLE),
        size = "l", easyClose = TRUE,
        shiny::tags$dl(class = "act-facts",
          shiny::tags$dt("Status"), shiny::tags$dd(a$STATUS),
          shiny::tags$dt("Owner"), shiny::tags$dd(owner_label(a, owner)),
          shiny::tags$dt("Due"), shiny::tags$dd(class = if (a$OVERDUE) "late",
                                                 paste0(fmt_date(a$DUE_DT), if (a$OVERDUE) " \u00b7 overdue")),
          shiny::tags$dt("About"), shiny::tags$dd(target_label(a)),
          shiny::tags$dt("Raised"), shiny::tags$dd(raised_label(a))),
        shiny::h6("History"),
        shiny::tags$ul(class = "act-hist", lapply(rev(seq_len(nrow(h))), function(i) shiny::tags$li(
          shiny::span(class = "note", paste0(fmt_time(h$UPDATED_AT[i]), " \u00b7 ", h$USERID[i],
                                             if (h$ON_BEHALF[i] == 1) " (for the owner, in a meeting)")),
          shiny::br(), history_text(h, i)))),
        shiny::hr(),
        shiny::textAreaInput(ns("note"), "Add an update", rows = 3, width = "100%"),
        shiny::div(class = "act-form",
          shiny::selectInput(ns("status"), "Status", action_status_choices, selected = a$STATUS, width = "150px"),
          shiny::dateInput(ns("due"), "Due", value = a$DUE_DT, width = "150px"),
          shiny::selectInput(ns("owner"), "Owner", member_choices(tm[tm$ACTIVE == 1 | tm$MEMBERID == a$OWNER, ]),
                             selected = a$OWNER, width = "220px")),
        if (a$STATUS != "closed") close_rule_note(is_owner, no_login, owner, nrow(mtg) > 0),
        if (a$STATUS != "closed" && no_login && nrow(mtg)) shiny::div(class = "act-form",
          shiny::checkboxInput(ns("on_behalf"), "Closing for the owner, in this meeting (set Status to closed)"),
          shiny::selectInput(ns("meeting"), "Meeting", meeting_choices(mtg), width = "220px")),
        footer = shiny::tagList(shiny::modalButton("Cancel"),
                                shiny::actionButton(ns("save"), "Save update", class = "btn-primary"))
      ))
    }

    shiny::observeEvent(input$save, {
      aid <- shiny::req(picked())
      a <- log()[log()$ACTIONID == aid, ]
      meeting <- if (isTRUE(input$on_behalf) && !is.null(input$meeting)) as.integer(input$meeting) else NA_integer_
      ok <- tryCatch({
        rtcrcore::update_action(
          ctx$con, ctx$study$id, aid, ctx$user, note = nz(input$note),
          status = if (!identical(input$status, a$STATUS)) input$status,
          owner = if (!identical(input$owner, a$OWNER)) input$owner,
          due = if (!identical(as.Date(input$due), a$DUE_DT)) input$due,
          meeting_id = meeting, on_behalf = isTRUE(input$on_behalf) && identical(input$status, "closed"))
        TRUE
      }, error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "error")
        FALSE
      })
      if (ok) {
        shiny::removeModal()
        bump()
        shiny::showNotification(sprintf("%s updated.", aid), type = "message")
      }
    })

    shiny::observeEvent(input$new, {
      tm <- team()
      tm <- tm[tm$ACTIVE == 1, ]
      if (!nrow(tm)) {
        shiny::showNotification("Add people to the study team first (Study team screen).", type = "warning")
        return()
      }
      mtg <- open_meetings()
      shiny::showModal(shiny::modalDialog(
        title = "New action", size = "m", easyClose = TRUE,
        shiny::textInput(ns("n_title"), "What needs doing", width = "100%"),
        shiny::div(class = "act-form",
          shiny::selectInput(ns("n_owner"), "Owner", member_choices(tm), width = "220px"),
          shiny::dateInput(ns("n_due"), "Due", value = today() + due_soon_days, width = "150px")),
        shiny::div(class = "act-form",
          shiny::selectInput(ns("n_type"), "About", action_attach, width = "130px"),
          shiny::textInput(ns("n_key"), "Site, subject, record or finding", width = "240px",
                           placeholder = "e.g. 113 or 101-002")),
        shiny::div(class = "act-form",
          shiny::selectInput(ns("n_area"), "Area", c("(none)" = "", action_areas), width = "160px"),
          if (nrow(mtg)) shiny::selectInput(ns("n_meeting"), "Raised in",
                                            c("(not in a meeting)" = "", meeting_choices(mtg)), width = "220px")),
        shiny::textAreaInput(ns("n_note"), "Note (optional)", rows = 2, width = "100%"),
        footer = shiny::tagList(shiny::modalButton("Cancel"),
                                shiny::actionButton(ns("create"), "Raise action", class = "btn-primary"))
      ))
    })

    shiny::observeEvent(input$create, {
      meeting <- if (is.null(input$n_meeting) || !nzchar(input$n_meeting)) NA_integer_ else as.integer(input$n_meeting)
      id <- tryCatch(rtcrcore::raise_action(
        ctx$con, ctx$study$id, input$n_title, input$n_owner, input$n_due, ctx$user,
        target_type = input$n_type, target_key = nz(input$n_key), area = nz(input$n_area),
        meeting_id = meeting, note = nz(input$n_note)), error = function(e) {
          shiny::showNotification(conditionMessage(e), type = "error")
          NULL
        })
      if (!is.null(id)) {
        shiny::removeModal()
        bump()
        shiny::showNotification(sprintf("%s raised.", id), type = "message")
      }
    })

    list(log = log, groups = groups, refresh = bump)
  })
}

# Splits the action log into the screen's groups, each with a one-line summary.
action_groups <- function(log, asof, last_meeting = NA) {
  asof <- as.Date(asof)
  open <- log[log$STATUS != "closed", , drop = FALSE]
  since <- if (is.na(last_meeting)) asof - closed_recent_days else as.Date(last_meeting)
  closed <- log[log$STATUS == "closed" & as.Date(substr(log$CLOSED_AT, 1, 10)) >= since, , drop = FALSE]
  over <- open[open$OVERDUE, , drop = FALSE]
  soon <- open[!open$OVERDUE & open$DUE_DT <= asof + due_soon_days, , drop = FALSE]
  later <- open[!open$OVERDUE & open$DUE_DT > asof + due_soon_days, , drop = FALSE]
  owners <- function(x) paste(unique(stats::na.omit(x$OWNER_NAME)), collapse = ", ")
  list(
    overdue = list(label = "Overdue", rows = over, summary = if (!nrow(over)) "Nothing overdue" else
      sprintf("%s \u00b7 oldest %d days late", owners(over), as.integer(asof - min(over$DUE_DT)))),
    soon = list(label = sprintf("Due in the next %d days", due_soon_days), rows = soon,
                summary = sprintf("%d in progress", sum(soon$STATUS == "in progress"))),
    later = list(label = "Due later", rows = later, summary = if (!nrow(later)) "None" else
      date_span(later$DUE_DT)),
    closed = list(label = if (is.na(last_meeting)) sprintf("Closed in the last %d days", closed_recent_days) else
      "Closed since the last meeting", rows = closed, summary = if (!nrow(closed)) "None" else owners(closed))
  )
}

# Actions for one site: its site and subject items, records and findings of
# its subjects, and study-wide items.
site_actions <- function(log, site, adsl) {
  if (is.null(site) || identical(site, "All sites") || !nrow(log)) return(log)
  subjects <- adsl$SUBJID[adsl$SITEID == site]
  about_subject <- vapply(log$TARGET_KEY, function(k) {
    !is.na(k) && any(vapply(subjects, function(s) grepl(s, k, fixed = TRUE), TRUE))
  }, TRUE, USE.NAMES = FALSE)
  keep <- log$TARGET_TYPE == "study" | (log$TARGET_TYPE == "site" & log$TARGET_KEY %in% site) | about_subject
  log[keep, , drop = FALSE]
}

last_meeting_date <- function(con, study) {
  m <- rtcrcore::meetings(con, study)
  m <- m[m$STATUS == "closed", , drop = FALSE]
  if (!nrow(m)) NA else max(as.Date(m$MEETING_DT))
}

action_row <- function(a, ns) {
  shiny::tags$button(
    type = "button", class = "act-row",
    onclick = sprintf("Shiny.setInputValue('%s','%s',{priority:'event'})", ns("pick"), htmltools::htmlEscape(a$ACTIONID)),
    shiny::span(class = "act-main",
      shiny::span(shiny::span(class = "mono note", a$ACTIONID), " ", a$TITLE),
      shiny::span(class = "note", paste0(if (is.na(a$OWNER_NAME)) a$OWNER else a$OWNER_NAME, " \u00b7 ", target_label(a)))),
    shiny::span(class = "act-side",
      shiny::span(a$STATUS),
      shiny::span(class = paste("note", if (a$OVERDUE) "late"), due_label(a))))
}

target_label <- function(a) {
  switch(a$TARGET_TYPE, study = "Whole study", site = paste("Site", a$TARGET_KEY),
         subject = paste("Subject", a$TARGET_KEY), record = paste("Record", a$TARGET_KEY),
         finding = paste("Finding", a$TARGET_KEY), a$TARGET_KEY)
}

due_label <- function(a) {
  if (a$STATUS == "closed") return(paste("Closed", fmt_date(as.Date(substr(a$CLOSED_AT, 1, 10)))))
  paste0("Due ", fmt_date(a$DUE_DT), if (a$OVERDUE) " \u00b7 overdue")
}

raised_label <- function(a) {
  paste0(fmt_time(a$RAISED_AT), if (!is.na(a$RAISED_MEETING)) paste0(" \u00b7 meeting ", a$RAISED_MEETING))
}

owner_label <- function(a, owner) {
  if (!nrow(owner)) return(a$OWNER)
  paste0(owner$NAME, if (!is.na(owner$ROLE)) paste0(" (", owner$ROLE, ")"),
         if (is.na(owner$LOGIN) || !nzchar(owner$LOGIN)) " \u00b7 no login")
}

history_text <- function(h, i) {
  bits <- character()
  if (i == 1) {
    bits <- c(bits, sprintf("Raised for %s, due %s", h$OWNER_NAME[i], fmt_date(as.Date(h$DUE_DT[i]))))
  } else {
    if (h$STATUS[i] != h$STATUS[i - 1]) bits <- c(bits, paste("Status:", h$STATUS[i]))
    if (h$OWNER[i] != h$OWNER[i - 1]) bits <- c(bits, paste("Owner:", h$OWNER_NAME[i]))
    if (h$DUE_DT[i] != h$DUE_DT[i - 1]) bits <- c(bits, paste("Due:", fmt_date(as.Date(h$DUE_DT[i]))))
  }
  if (!is.na(h$NOTE[i]) && nzchar(h$NOTE[i])) bits <- c(bits, h$NOTE[i])
  paste(bits, collapse = ". ")
}

close_rule_note <- function(is_owner, no_login, owner, in_meeting) {
  msg <- if (is_owner) "You own this action and can close it." else if (no_login && in_meeting)
    "The owner has no login: to close it, tick the box below and add a note." else if (no_login)
    "The owner has no login: it can be closed for them in an open meeting, with a note." else
    sprintf("Only %s can close this action.", owner$NAME)
  shiny::p(class = "note", msg)
}

member_choices <- function(tm) {
  stats::setNames(tm$MEMBERID, ifelse(is.na(tm$ROLE), tm$NAME, paste0(tm$NAME, " (", tm$ROLE, ")")))
}

meeting_choices <- function(m) {
  stats::setNames(as.character(m$MEETINGID), paste0("Meeting ", m$MEETINGID, ", ", fmt_date(as.Date(m$MEETING_DT))))
}

fmt_time <- function(x) sub("T(\\d{2}:\\d{2}).*", " \\1 UTC", x)

date_span <- function(d) {
  if (min(d) == max(d)) paste("Due", fmt_date(min(d))) else sprintf("Due %s to %s", fmt_date(min(d)), fmt_date(max(d)))
}

# Study team ------------------------------------------------------------------

team_ui <- function(id, params, study) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::p(class = "note", "The people actions can be assigned to. Members without a login can own actions;",
             "their items are closed for them in a meeting."),
    shiny::tableOutput(ns("team")),
    bslib::card(fill = FALSE, bslib::card_header("Add or change a member"),
      shiny::div(class = "act-form",
        shiny::textInput(ns("id"), "ID", width = "120px", placeholder = "e.g. cra113"),
        shiny::textInput(ns("name"), "Name", width = "200px"),
        shiny::textInput(ns("role"), "Role", width = "180px"),
        shiny::textInput(ns("email"), "Email", width = "200px"),
        shiny::textInput(ns("login"), "Login (if any)", width = "150px"),
        shiny::checkboxInput(ns("active"), "Active", value = TRUE)),
      shiny::p(class = "note", "Using an existing ID changes that member; the earlier details stay in the audit trail."),
      shiny::div(shiny::actionButton(ns("save"), "Save member", class = "btn-primary")))
  )
}

team_server <- function(id, ctx, params) {
  shiny::moduleServer(id, function(input, output, session) {
    version <- shiny::reactiveVal(0)
    team <- shiny::reactive({
      version()
      rtcrcore::team_members(ctx$con, ctx$study$id, active_only = FALSE)
    })
    output$team <- shiny::renderTable({
      t <- team()
      if (!nrow(t)) return(data.frame(Note = "No team members yet."))
      data.frame(ID = t$MEMBERID, Name = t$NAME, Role = t$ROLE, Email = t$EMAIL, Login = t$LOGIN,
                 Active = ifelse(t$ACTIVE == 1, "Yes", "No"), check.names = FALSE)
    }, na = "")
    shiny::observeEvent(input$save, {
      ok <- tryCatch({
        rtcrcore::set_team_member(ctx$con, ctx$study$id, trimws(input$id), trimws(input$name), ctx$user,
                                  role = nz(input$role), email = nz(input$email), login = nz(input$login),
                                  active = isTRUE(input$active))
        TRUE
      }, error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "error")
        FALSE
      })
      if (ok) {
        version(version() + 1)
        shiny::showNotification(sprintf("Saved %s.", trimws(input$name)), type = "message")
      }
    })
    list(team = team)
  })
}
