# =============================================================================
#  What's new: changes since the last refresh + attention panel (DR-011, DR-015)
# =============================================================================

whatsnew_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(class = "sim-banner", "Demo with SIMULATED data. Step the review date forward with the refresh button; click any subject ID to open the profile."),
    shiny::uiOutput(ns("kpis")), shiny::br(),
    bslib::layout_columns(fill = FALSE, col_widths = c(8, 4),
      table_card_ui(ns("chg"), "Changes since last refresh",
                    "Sorted by priority: SAEs, PK alerts, items needing review, low compliance, then newest first."),
      bslib::card(fill = FALSE, bslib::card_header("Needs attention now"), shiny::uiOutput(ns("att"))))
  )
}

whatsnew_server <- function(id, snap, ctrl, registry) {
  shiny::moduleServer(id, function(input, output, session) {
    chg <- shiny::reactive(change_log(snap(), ctrl$since(), ctrl$alert(), ctrl$cthr()))
    output$kpis <- shiny::renderUI({
      s <- snap(); c <- chg()
      nsae <- sum(c$PRIORITY == "SAE"); npk <- sum(c$PRIORITY == "PK alert")
      bslib::layout_columns(fill = FALSE, col_widths = bslib::breakpoints(sm = 6, lg = 2),
        kpi("Enrolled", nrow(s$subj), sprintf("%d dosed", sum(s$subj$DOSEDFL))),
        kpi("New visits", sum(c$DOMAIN == "Visit"), sprintf("since %s", format(ctrl$since(), "%d %b"))),
        kpi("New AEs", sum(c$DOMAIN == "AE" & c$CHANGE == "New"), sprintf("%d serious", nsae), if (nsae) "flag" else ""),
        kpi("Changed records", sum(c$CHANGE == "Changed"), "corrections to earlier data"),
        kpi("New PK results", sum(c$DOMAIN == "PK"), sprintf("%d at/above alert", npk), if (npk) "warn" else ""),
        kpi("New queries", sum(c$DOMAIN == "Query"), sprintf("%d open overall", sum(s$qry$STATUS == "Open"))))
    })
    tbl <- shiny::reactive({
      c <- chg()
      data.frame(Change = badge(c$CHANGE), Domain = c$DOMAIN, Subject = sid_link(c$SUBJID), Site = c$SITEID,
                 Item = c$ITEM, Detail = htmltools::htmlEscape(c$DETAIL), Date = fmt_d(c$CHGDT),
                 Priority = badge(c$PRIORITY), check.names = FALSE)
    })
    table_card_server("chg", tbl, registry, "Changes", ctrl$about, page_length = 12, empty = "No changes in this window.")
    output$att <- shiny::renderUI({
      a <- attention_items(snap(), chg(), ctrl$alert(), ctrl$cthr())
      shiny::tagList(
        lapply(seq_len(nrow(a)), function(i) shiny::div(class = "att",
          shiny::HTML(tab_link(a$ITEM[i], a$TAB[i])),
          shiny::HTML(sprintf('<span class="badge-s %s">%d</span>', if (a$N[i] > 0) paste0("b-", a$LEVEL[i]) else "b-chg", a$N[i])))),
        shiny::p(class = "note", style = "margin-top:.6rem", "Click an item to open its tab. In production this panel drives the weekly data review agenda."))
    })
  })
}
