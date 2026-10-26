# =============================================================================
#  Phase 3 Clinical Data Review  |  R Shiny demo
#  Modeled on a generic rare-dermatology design: single-arm, baseline-controlled, 8-week
#  run-in, once-daily topical study drug, 24-week efficacy
#  period + open-label extension.
#
#  ALL DATA ARE SIMULATED. Nothing here is real trial data.
#
#  Run:
#    install.packages(c("shiny", "bslib", "dplyr", "tidyr", "DT", "plotly"))
#    shiny::runApp("app.R")
#
#  Swap simulate_study() for a reader of live EDC extracts / SDTM
#  (DM, SV, QS, AE, PC, query log) and every screen keeps working.
# =============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(dplyr)
  library(tidyr)
  library(DT)
  library(plotly)
})

# ---- Palette (validated categorical slots + reserved status colors) --------
pal <- list(
  blue = "#2a78d6", orange = "#eb6834", aqua = "#1baf7a",
  good = "#0ca30c", warning = "#fab219", serious = "#ec835a", critical = "#d03b3b",
  neutral = "#c9c8c2", ink = "#0b0b0b", ink2 = "#52514e", ink3 = "#8a8983",
  grid = "#ecebe7", surface = "#fcfcfb",
  seq = c("#c6dcf5", "#86b4ec", "#2a78d6", "#18508f")  # sequential blue ramp
)

STATUS_LEVELS <- c("Complete", "Out of window", "Overdue / not entered",
                   "Missed", "Not yet due", "Discontinued")
STATUS_COLS   <- c("#b6dcb6", pal$warning, pal$serious,
                   pal$critical, "#e9e8e3", "#9d9c96")

VISITS <- data.frame(
  visit    = c("Run-in (Wk -8)", "Baseline (Day 1)", "Week 4", "Week 8", "Week 12",
               "Week 16", "Week 20", "Week 24", "Week 36", "Week 48"),
  visitnum = 1:10,
  week     = c(-8, 0, 4, 8, 12, 16, 20, 24, 36, 48),
  window   = c(7, 0, 7, 7, 7, 7, 7, 7, 14, 14),
  stringsAsFactors = FALSE
)
VISIT_ORDER <- VISITS$visit
PK_WEEKS    <- c(4, 12, 24)
LLOQ        <- 0.1   # ng/mL
MIN_N       <- 3     # minimum n to plot a visit mean
PK_LIMIT    <- 2.0   # ng/mL systemic exposure threshold

# =============================================================================
#  1. Simulated study (full timeline; screens show it as of a data-cut date)
# =============================================================================
simulate_study <- function(seed = 2026) {
  set.seed(seed)
  n     <- 51
  sites <- as.character(101:113)

  subj <- data.frame(
    site  = sample(sites, n, replace = TRUE),
    age   = sample(c(sample(3:5, 2), round(runif(n - 2, 6, 58)))),
    sex   = sample(c("F", "M"), n, replace = TRUE),
    runin = sort(as.Date("2025-12-01") + sample(0:260, n, replace = TRUE)),
    stringsAsFactors = FALSE
  ) |>
    group_by(site) |>
    mutate(subjid = paste0(site, "-", sprintf("%03d", row_number()))) |>
    ungroup() |>
    mutate(
      dosed     = TRUE,
      dose_date = runin + 56,
      resp      = rnorm(n, 2.6, 0.6),           # latent Week-24 IGA response
      mcss0     = round(runif(n, 9, 18), 1),       # severity at entry
      effect    = pmin(0.85, pmax(0, rnorm(n, 0.45, 0.18)))
    )
  nd <- sample(which(subj$age >= 6), 1)            # one withdrawal in run-in
  subj$dosed[nd] <- FALSE
  subj$dose_date[nd] <- NA
  subj$disc_date <- as.Date(NA)
  subj$disc_date[nd] <- subj$runin[nd] + 20
  subj$itt <- subj$dosed & subj$age >= 6

  # ---- Visits + efficacy (QS-like) -----------------------------------------
  vis <- tidyr::crossing(subjid = subj$subjid, VISITS) |>
    left_join(subj, by = "subjid") |>
    filter(dosed | visitnum == 1) |>
    arrange(subjid, visitnum)
  m <- nrow(vis)
  vis <- vis |>
    mutate(
      target = if_else(visitnum == 1, runin, dose_date + week * 7),
      oow    = runif(m) < 0.06 & visitnum > 2,
      offset = case_when(visitnum == 2 ~ 0,
                         oow ~ sample(c(-1, 1), m, TRUE) * sample(9:16, m, TRUE),
                         TRUE ~ round(rnorm(m, 0, 2.5))),
      missed = runif(m) < 0.03 & visitnum > 2,
      vdate  = if_else(missed, as.Date(NA), target + offset),
      lag    = round(rgamma(m, 1.5, scale = 3)) + if_else(runif(m) < 0.08, sample(15:30, m, TRUE), 0L),
      entry  = vdate + lag,
      frac   = 1 - exp(-pmax(week, 0) / 7),
      mcss   = round(pmax(0, if_else(week <= 0, mcss0 + rnorm(m, 0, 0.7),
                                     mcss0 * (1 - effect * frac) + rnorm(m, 0, 0.8))), 1),
      iga    = if_else(week > 0, pmin(3, pmax(-3, round(resp * frac + rnorm(m, 0, 0.45)))), NA_real_),
      mod    = if_else(runif(m) < 0.10, entry + sample(2:25, m, TRUE), as.Date(NA)),
      mcss_prev = if_else(!is.na(mod), mcss + sample(c(-2, -1, 1, 2), m, TRUE), NA_real_)
    ) |>
    select(subjid, site, age, itt, dosed, dose_date, visit, visitnum, week, window,
           target, offset, oow, missed, vdate, entry, mcss, iga, mod, mcss_prev)

  # ---- Adverse events (AE-like) --------------------------------------------
  terms <- data.frame(
    pt   = c("Application site irritation", "Application site pruritus",
             "Application site erythema", "Acne", "Dermatitis contact", "Nasopharyngitis",
             "Upper respiratory tract infection", "Headache", "Pyrexia", "Cellulitis",
             "COVID-19", "Arthralgia"),
    soc  = c(rep("General disorders and administration site conditions", 3),
             rep("Skin and subcutaneous tissue disorders", 2),
             rep("Infections and infestations", 2), "Nervous system disorders",
             "General disorders and administration site conditions",
             rep("Infections and infestations", 2),
             "Musculoskeletal and connective tissue disorders"),
    w    = c(10, 7, 6, 6, 3, 8, 7, 5, 3, 2, 2, 2),
    prel = c(.9, .85, .85, .6, .5, .02, .02, .1, .05, .1, 0, .05),
    stringsAsFactors = FALSE
  )
  dosed <- subj |> filter(dosed)
  k <- rpois(nrow(dosed), 1.4)
  ae <- dosed[rep(seq_len(nrow(dosed)), k), c("subjid", "site", "dose_date")]
  na <- nrow(ae)
  ti <- sample(seq_len(nrow(terms)), na, TRUE, prob = terms$w)
  ae <- ae |>
    mutate(
      aeseq   = ave(seq_len(na), subjid, FUN = seq_along),
      pt      = terms$pt[ti], soc = terms$soc[ti],
      day     = sample(1:336, na, TRUE),
      onset   = dose_date + day - 1,
      sev     = sample(c("Mild", "Moderate", "Severe"), na, TRUE, prob = c(.72, .24, .04)),
      serious = runif(na) < if_else(pt == "Cellulitis", .5, .01),
      related = runif(na) < terms$prel[ti] & !serious,
      lag     = round(rgamma(na, 1.5, scale = 3)),
      entry   = onset + lag,
      mod     = if_else(runif(na) < 0.2, entry + sample(3:30, na, TRUE), as.Date(NA)),
      modtext = sample(c("Severity updated", "Outcome updated to Recovered",
                         "Action taken updated", "Causality reassessed"), na, TRUE)
    )

  # ---- Systemic study drug PK (PC-like) -------------------------------------
  pk <- vis |>
    filter(week %in% PK_WEEKS, !missed) |>
    transmute(subjid, site, visit, week, sdate = vdate) |>
    mutate(
      conc   = pmin(1.35, rlnorm(n(), log(0.22), 0.75)),
      entry  = sdate + sample(14:28, n(), TRUE)          # bioanalytical turnaround
    )
  pk$conc[order(pk$entry)[6]] <- 1.68                      # one early sample near the limit
  pk <- pk |> mutate(blq = conc < LLOQ, result = if_else(blq, "<LLOQ", sprintf("%.2f", conc)))

  # ---- Queries --------------------------------------------------------------
  qv <- vis |> filter(!missed) |>
    transmute(subjid, site, visit, entry, form = "Efficacy / visit")
  qa <- ae |> transmute(subjid, site, visit = paste0("AE #", aeseq), entry, form = "Adverse event")
  q  <- bind_rows(qv, qa)
  q  <- q[runif(nrow(q)) < 0.17, ]
  nq <- nrow(q)
  q <- q |>
    mutate(
      qid      = sprintf("Q%04d", seq_len(nq)),
      text     = sample(c("Assessment missing for visit", "Visit date inconsistent with dosing diary",
                          "Value outside expected range", "Lesion photo not uploaded",
                          "AE onset date precedes first dose", "Concomitant medication missing stop date"),
                        nq, TRUE),
      opened   = entry + sample(1:6, nq, TRUE),
      answered = if_else(runif(nq) < 0.95, opened + round(rgamma(nq, 1.6, scale = 5)), as.Date(NA)),
      closed   = if_else(!is.na(answered) & runif(nq) < 0.97, answered + sample(1:7, nq, TRUE), as.Date(NA))
    )

  list(subj = subj, vis = vis, ae = ae, pk = pk, q = q)
}

# =============================================================================
#  2. Snapshot: what the EDC shows as of a data-cut date
# =============================================================================
snapshot <- function(sim, cut) {
  subj <- sim$subj |> filter(runin <= cut)

  vis <- sim$vis |>
    filter(subjid %in% subj$subjid) |>
    left_join(select(subj, subjid, disc_date), by = "subjid") |>
    mutate(
      entered = !missed & !is.na(entry) & entry <= cut,
      status  = case_when(
        entered & oow                         ~ "Out of window",
        entered                               ~ "Complete",
        target + window >= cut                ~ "Not yet due",
        missed                                ~ "Missed",
        TRUE                                  ~ "Overdue / not entered"
      ),
      days_overdue = if_else(status == "Overdue / not entered", as.numeric(cut - (target + window)), NA_real_)
    )
  # withdrawn subject: everything after run-in is not applicable
  wd <- subj$subjid[!subj$dosed]
  if (length(wd)) {
    extra <- tidyr::crossing(subjid = wd, VISITS |> filter(visitnum > 1)) |>
      left_join(select(subj, subjid, site, age, itt, dosed), by = "subjid") |>
      mutate(status = "Discontinued", entered = FALSE)
    vis <- bind_rows(vis, extra)
  }

  ae <- sim$ae |> filter(subjid %in% subj$subjid, entry <= cut) |>
    mutate(mod = if_else(!is.na(mod) & mod <= cut, mod, as.Date(NA)))
  pk <- sim$pk |> filter(subjid %in% subj$subjid, entry <= cut)
  q  <- sim$q  |> filter(subjid %in% subj$subjid, opened <= cut) |>
    mutate(
      status = case_when(!is.na(closed) & closed <= cut     ~ "Closed",
                         !is.na(answered) & answered <= cut ~ "Answered",
                         TRUE                                ~ "Open"),
      age_days = if_else(status == "Closed", NA_real_, as.numeric(cut - opened))
    )

  list(subj = subj, vis = vis, ae = ae, pk = pk, q = q, cut = cut)
}

# =============================================================================
#  3. Change log between two refreshes
# =============================================================================
change_log <- function(s, since, alert) {
  cut <- s$cut
  inwin <- function(d) !is.na(d) & d > since & d <= cut

  v_new <- s$vis |> filter(entered, inwin(entry)) |>
    transmute(Change = "New", Domain = "Visit", Subject = subjid, Site = site, Visit = visit,
              Detail = if_else(is.na(iga), sprintf("Visit entered | MCSS %.1f", mcss),
                               sprintf("Visit entered | IGA %+d | MCSS %.1f", as.integer(iga), mcss)),
              Date = entry, Priority = if_else(!is.na(iga) & iga < 0, "Review", ""))
  v_chg <- s$vis |> filter(entered, inwin(mod), entry <= since) |>
    transmute(Change = "Changed", Domain = "Visit", Subject = subjid, Site = site, Visit = visit,
              Detail = sprintf("MCSS corrected %.1f -> %.1f", mcss_prev, mcss),
              Date = mod, Priority = "")
  a_new <- s$ae |> filter(inwin(entry)) |>
    transmute(Change = "New", Domain = "AE", Subject = subjid, Site = site, Visit = paste0("AE #", aeseq),
              Detail = paste0(pt, " | ", sev, if_else(related, " | Related", " | Not related"),
                              if_else(serious, " | SERIOUS", "")),
              Date = entry, Priority = if_else(serious, "SAE", if_else(sev == "Severe", "Review", "")))
  a_chg <- s$ae |> filter(inwin(mod), entry <= since) |>
    transmute(Change = "Changed", Domain = "AE", Subject = subjid, Site = site, Visit = paste0("AE #", aeseq),
              Detail = paste0(pt, " | ", modtext), Date = mod, Priority = "")
  p_new <- s$pk |> filter(inwin(entry)) |>
    transmute(Change = "New", Domain = "PK", Subject = subjid, Site = site, Visit = visit,
              Detail = paste0("study drug ", result, " ng/mL"),
              Date = entry, Priority = if_else(!blq & conc >= alert, "PK alert", ""))
  q_new <- s$q |> filter(inwin(opened)) |>
    transmute(Change = "New", Domain = "Query", Subject = subjid, Site = site, Visit = visit,
              Detail = paste0(qid, ": ", text), Date = opened, Priority = "")

  bind_rows(v_new, v_chg, a_new, a_chg, p_new, q_new) |>
    mutate(rank = match(Priority, c("SAE", "PK alert", "Review", ""))) |>
    arrange(rank, desc(Date)) |>
    select(-rank)
}

# =============================================================================
#  4. Plot helpers
# =============================================================================
pl_style <- function(p, xtitle = NULL, ytitle = NULL, legend = TRUE) {
  ax <- list(gridcolor = pal$grid, zeroline = FALSE, linecolor = pal$grid,
             tickfont = list(color = pal$ink2, size = 11),
             titlefont = list(color = pal$ink2, size = 12))
  p |>
    layout(
      font = list(family = "Inter, system-ui, sans-serif", color = pal$ink),
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      xaxis = c(ax, list(title = xtitle)), yaxis = c(ax, list(title = ytitle)),
      showlegend = legend,
      legend = list(orientation = "h", y = 1.12, x = 0, font = list(size = 11, color = pal$ink2)),
      hoverlabel = list(bgcolor = "white", bordercolor = pal$grid, font = list(color = pal$ink)),
      margin = list(l = 10, r = 10, t = 30, b = 10)
    ) |>
    config(displaylogo = FALSE, modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"))
}

badge <- function(x) {
  cls <- c("SAE" = "b-crit", "PK alert" = "b-warn", "Review" = "b-ser",
           "New" = "b-new", "Changed" = "b-chg", "Open" = "b-ser", "Answered" = "b-warn",
           "Missed" = "b-crit", "Overdue / not entered" = "b-ser", "Out of window" = "b-warn")
  ifelse(x == "" | is.na(x), "",
         sprintf('<span class="badge-s %s">%s</span>', ifelse(is.na(cls[x]), "b-new", cls[x]), x))
}

flag_badge <- function(x) {
  cls <- c("Worsening" = "b-crit", "Run-in instability" = "b-warn", "No response" = "b-ser")
  ifelse(x == "" | is.na(x), "", sprintf('<span class="badge-s %s">%s</span>', cls[x], x))
}

dt_opts <- function(pageLength = 10, ...) {
  list(pageLength = pageLength, dom = "ftip", scrollX = TRUE, ...)
}

# =============================================================================
#  5. UI
# =============================================================================
SIM <- simulate_study()
DEFAULT_CUT <- as.Date("2026-10-22")

theme <- bs_theme(
  version = 5, bg = pal$surface, fg = pal$ink, primary = pal$blue,
  base_font = font_collection("Inter", "system-ui", "sans-serif"),
  "border-radius" = "8px"
)

css <- HTML("
  .sim-banner{background:#fff4dc;border:1px solid #f2d18a;color:#5a4410;font-size:.82rem;
              padding:.35rem .8rem;border-radius:6px;margin-bottom:.75rem}
  .kpi{border:1px solid #e7e6e2;border-radius:10px;padding:.7rem .9rem;background:#fff;height:100%}
  .kpi .lab{font-size:.75rem;color:#52514e;text-transform:uppercase;letter-spacing:.04em}
  .kpi .val{font-size:1.7rem;font-weight:650;line-height:1.15;color:#0b0b0b}
  .kpi .sub{font-size:.78rem;color:#52514e}
  .kpi.flag{border-left:4px solid #d03b3b}
  .kpi.warn{border-left:4px solid #fab219}
  .badge-s{display:inline-block;padding:.1rem .45rem;border-radius:999px;font-size:.72rem;
           font-weight:600;white-space:nowrap}
  .b-crit{background:#fbe3e3;color:#8f1f1f}.b-warn{background:#fff1cf;color:#6b4a00}
  .b-ser{background:#fde8df;color:#8a3a17}.b-new{background:#e3eefb;color:#174b8f}
  .b-chg{background:#ecebe7;color:#3b3a37}
  .card-header{font-weight:600;font-size:.92rem}
  .note{font-size:.78rem;color:#6b6a65}
  table.dataTable{font-size:.84rem}
")

kpi <- function(label, value, sub = NULL, cls = "") {
  div(class = paste("kpi", cls), div(class = "lab", label), div(class = "val", value),
      if (!is.null(sub)) div(class = "sub", sub))
}

status_legend <- div(
  style = "display:flex;flex-wrap:wrap;gap:.4rem 1rem;font-size:.78rem;color:#52514e;margin-bottom:.25rem",
  lapply(seq_along(STATUS_LEVELS), function(i)
    span(span(style = sprintf("display:inline-block;width:11px;height:11px;border-radius:2px;background:%s;margin-right:5px;vertical-align:-1px", STATUS_COLS[i])),
         STATUS_LEVELS[i]))
)

sidebar_ui <- sidebar(
  width = 280,
  h6("Data refresh"),
  uiOutput("cut_info"),
  dateInput("since", "Show changes since", value = DEFAULT_CUT - 14),
  actionButton("advance", "Simulate next weekly refresh", class = "btn-primary btn-sm"),
  actionButton("reset", "Reset to 22 Oct 2026", class = "btn-outline-secondary btn-sm"),
  hr(),
  h6("Filters"),
  selectInput("site", "Site", choices = c("All sites", sort(unique(SIM$subj$site))), selected = "All sites"),
  radioButtons("pop", "Population",
               c("All enrolled" = "all", "Efficacy ITT (dosed, age >= 6)" = "itt"), selected = "all"),
  sliderInput("alert", "PK alert threshold (ng/mL)", min = 0.5, max = 2, value = 1.5, step = 0.1),
  hr(),
  p(class = "note", "Simulated data for demonstration only. Not real trial data.")
)

ui <- page_navbar(
  title = "Phase 3 | Clinical Data Review",
  theme = theme, sidebar = sidebar_ui, fillable = FALSE,
  header = tags$head(tags$style(css)),

  nav_panel("What's new",
    div(class = "sim-banner", "Demo with SIMULATED data modeled on a single-arm, baseline-controlled rare-dermatology design."),
    uiOutput("kpi_new"),
    br(),
    layout_columns(fill = FALSE, row_heights = "auto", col_widths = c(8, 4),
      card(fill = FALSE, card_header("Changes since last refresh"),
           p(class = "note", "Sorted by priority: SAEs, PK alerts, items needing medical review, then newest first."),
           DTOutput("tbl_changes", fill = FALSE)),
      card(fill = FALSE, card_header("Needs attention now"), uiOutput("attention"))
    )
  ),

  nav_panel("Efficacy",
    uiOutput("kpi_eff"),
    br(),
    layout_columns(fill = FALSE, row_heights = "auto", col_widths = c(6, 6),
      card(fill = FALSE, card_header("IGA by visit: mean and 95% CI"),
           plotlyOutput("p_iga", height = "320px"),
           p(class = "note", "7-point scale, -3 (very much worse) to +3 (very much improved). Visits with n < 3 not plotted.")),
      card(fill = FALSE, card_header("Blinded severity score (MCSS) through run-in and treatment"),
           plotlyOutput("p_mcss", height = "320px"),
           p(class = "note", "Lower is better. Shaded band = 8-week run-in; stable run-in supports the baseline-controlled design."))
    ),
    layout_columns(fill = FALSE, row_heights = "auto", col_widths = c(4, 8),
      card(fill = FALSE, card_header("IGA distribution"),
           selectInput("iga_visit", NULL, choices = VISIT_ORDER[3:10], selected = "Week 24"),
           plotlyOutput("p_igadist", height = "260px")),
      card(fill = FALSE, card_header("Subject-level review"),
           selectInput("subj", "Highlight subject in severity plot", choices = NULL),
           DTOutput("tbl_eff", fill = FALSE))
    )
  ),

  nav_panel("Safety & PK",
    uiOutput("kpi_saf"),
    br(),
    layout_columns(fill = FALSE, row_heights = "auto", col_widths = c(6, 6),
      card(fill = FALSE, card_header("Adverse events by preferred term"), DTOutput("tbl_ae", fill = FALSE)),
      card(fill = FALSE, card_header("AE onset by study day"),
           plotlyOutput("p_aetime", height = "360px"),
           p(class = "note", "Diamond = serious. Hover for term, severity and causality."))
    ),
    layout_columns(fill = FALSE, row_heights = "auto", col_widths = c(7, 5),
      card(fill = FALSE, card_header("Systemic study drug trough concentrations"),
           plotlyOutput("p_pk", height = "330px"),
           p(class = "note", sprintf("Red line = %.1f ng/mL exposure threshold; amber = alert threshold (sidebar); dotted = LLOQ %.1f ng/mL. BLQ plotted at LLOQ/2.", PK_LIMIT, LLOQ))),
      card(fill = FALSE, card_header("PK samples at or above alert threshold"), DTOutput("tbl_pk", fill = FALSE))
    )
  ),

  nav_panel("Data quality",
    uiOutput("kpi_dq"),
    br(),
    layout_columns(fill = FALSE, row_heights = "auto", col_widths = c(7, 5),
      card(fill = FALSE, card_header("Visit status by subject"),
           status_legend,
           plotlyOutput("p_status", height = "auto")),
      card(fill = FALSE, card_header("Unresolved queries by site and age"),
           plotlyOutput("p_queries", height = "330px"),
           p(class = "note", "Open + answered-not-closed queries, by days since opened."))
    ),
    card(fill = FALSE, card_header("Issue listing"), DTOutput("tbl_issues", fill = FALSE))
  ),

  nav_spacer(),
  nav_item(tags$span(class = "note", "Simulated data"))
)

# =============================================================================
#  6. Server
# =============================================================================
server <- function(input, output, session) {

  cut <- reactiveVal(DEFAULT_CUT)
  observeEvent(input$advance, {
    updateDateInput(session, "since", value = cut())
    cut(cut() + 7)
  })
  observeEvent(input$reset, {
    cut(DEFAULT_CUT)
    updateDateInput(session, "since", value = DEFAULT_CUT - 14)
  })

  output$cut_info <- renderUI({
    div(class = "note", HTML(sprintf("Current data cut: <b>%s</b>", format(cut(), "%d %b %Y"))))
  })

  snap <- reactive({
    s <- snapshot(SIM, cut())
    keep <- s$subj
    if (input$site != "All sites") keep <- keep |> filter(site == input$site)
    if (input$pop == "itt") keep <- keep |> filter(itt)
    ids <- keep$subjid
    s$subj <- keep
    s$vis <- s$vis |> filter(subjid %in% ids)
    s$ae  <- s$ae  |> filter(subjid %in% ids)
    s$pk  <- s$pk  |> filter(subjid %in% ids)
    s$q   <- s$q   |> filter(subjid %in% ids)
    s
  })
  since <- reactive({ min(input$since, cut() - 1) })
  chg   <- reactive({ change_log(snap(), since(), input$alert) })

  # ---- What's new ----------------------------------------------------------
  output$kpi_new <- renderUI({
    s <- snap(); c <- chg()
    nsae <- sum(c$Priority == "SAE"); npk <- sum(c$Priority == "PK alert")
    layout_columns(col_widths = rep(2, 6),
      kpi("Enrolled", nrow(s$subj), sprintf("%d dosed", sum(s$subj$dosed))),
      kpi("New visits", sum(c$Domain == "Visit" & c$Change == "New"),
          sprintf("since %s", format(since(), "%d %b"))),
      kpi("New AEs", sum(c$Domain == "AE" & c$Change == "New"),
          sprintf("%d serious", nsae), cls = if (nsae > 0) "flag" else ""),
      kpi("Changed records", sum(c$Change == "Changed"), "edits to previously entered data"),
      kpi("New PK results", sum(c$Domain == "PK"),
          sprintf("%d at/above alert", npk), cls = if (npk > 0) "warn" else ""),
      kpi("New queries", sum(c$Domain == "Query"),
          sprintf("%d open overall", sum(s$q$status == "Open")))
    )
  })

  output$tbl_changes <- renderDT({
    d <- chg() |> mutate(Priority = badge(Priority), Change = badge(Change),
                         Date = format(Date, "%d %b"))
    datatable(d, escape = FALSE, rownames = FALSE,
              options = dt_opts(12, order = list(), autoWidth = TRUE,
                                columnDefs = list(list(width = "38%", targets = 5))))
  })

  output$attention <- renderUI({
    s <- snap(); c <- chg()
    sae  <- c |> filter(Priority == "SAE")
    pka  <- s$pk |> filter(!blq, conc >= input$alert)
    ovd  <- s$vis |> filter(status == "Overdue / not entered", days_overdue > 14)
    oldq <- s$q |> filter(status == "Open", age_days > 30)
    worse <- s$vis |> filter(entered, !is.na(iga), iga < 0) |>
      group_by(subjid) |> slice_max(visitnum, n = 1) |> ungroup()
    item <- function(n, label, cls) {
      div(style = "display:flex;justify-content:space-between;align-items:center;padding:.45rem 0;border-bottom:1px solid #ecebe7",
          span(label), span(class = paste("badge-s", cls), n))
    }
    tagList(
      item(nrow(sae),   "New serious AEs this refresh", if (nrow(sae)) "b-crit" else "b-chg"),
      item(nrow(pka),   sprintf("PK samples >= %.1f ng/mL (all-time)", input$alert), if (nrow(pka)) "b-warn" else "b-chg"),
      item(nrow(worse), "Subjects with IGA below 0 (latest visit)", if (nrow(worse)) "b-ser" else "b-chg"),
      item(nrow(ovd),   "Visits overdue > 14 days", if (nrow(ovd)) "b-ser" else "b-chg"),
      item(nrow(oldq),  "Queries open > 30 days", if (nrow(oldq)) "b-ser" else "b-chg"),
      p(class = "note", style = "margin-top:.6rem",
        "Each count links to the detail on its tab. In production this panel would drive the weekly data review meeting agenda.")
    )
  })

  # ---- Efficacy -------------------------------------------------------------
  eff <- reactive({
    snap()$vis |> filter(entered, itt)
  })

  observe({
    ids <- sort(unique(eff()$subjid))
    sel <- isolate(input$subj)
    updateSelectInput(session, "subj", choices = c("None", ids),
                      selected = if (!is.null(sel) && sel %in% ids) sel else "None")
  })

  output$kpi_eff <- renderUI({
    e <- eff()
    w24 <- e |> filter(week == 24)
    resp <- if (nrow(w24)) mean(w24$iga >= 2) else NA
    base <- e |> filter(week <= 0) |> select(subjid, week, mcss) |>
      pivot_wider(names_from = week, values_from = mcss, names_prefix = "w")
    drift <- if (all(c("w-8", "w0") %in% names(base))) {
      d <- abs(base$w0 - base$`w-8`); round(median(d, na.rm = TRUE), 1)
    } else NA
    layout_columns(col_widths = rep(3, 4),
      kpi("ITT subjects with data", n_distinct(e$subjid)),
      kpi("Reached Week 24", nrow(w24)),
      kpi("Week 24 mean IGA", if (nrow(w24)) sprintf("%+.2f", mean(w24$iga)) else "-",
          if (!is.na(resp)) sprintf("%.0f%% much/very much improved", 100 * resp)),
      kpi("Run-in drift (median |change|)", ifelse(is.na(drift), "-", drift), "MCSS points, Wk -8 to Day 1")
    )
  })

  output$p_iga <- renderPlotly({
    d <- eff() |> filter(week > 0) |>
      group_by(visit, week) |>
      summarise(n = n(), m = mean(iga), se = sd(iga) / sqrt(n()), .groups = "drop") |>
      filter(n >= MIN_N) |>
      mutate(lo = m - qt(.975, n - 1) * se, hi = m + qt(.975, n - 1) * se) |>
      arrange(week)
    validate(need(nrow(d) > 0, "No post-baseline IGA data yet."))
    plot_ly(d, x = ~week, y = ~m, type = "scatter", mode = "lines+markers",
            line = list(color = pal$blue, width = 2),
            marker = list(color = pal$blue, size = 8, line = list(color = pal$surface, width = 2)),
            error_y = list(type = "data", symmetric = FALSE, array = ~hi - m, arrayminus = ~m - lo,
                           color = pal$blue, thickness = 1.2, width = 4),
            text = ~sprintf("%s<br>Mean %+.2f (95%% CI %+.2f to %+.2f)<br>n = %d", visit, m, lo, hi, n),
            hoverinfo = "text", name = "Mean IGA") |>
      add_annotations(x = d$week, y = d$hi, text = paste0("n=", d$n), yshift = 12, showarrow = FALSE,
                      font = list(size = 10, color = pal$ink3)) |>
      pl_style("Study week", "Mean IGA", legend = FALSE) |>
      layout(yaxis = list(range = c(-1, 3.4), zeroline = TRUE, zerolinecolor = pal$neutral),
             xaxis = list(tickvals = d$week))
  })

  output$p_mcss <- renderPlotly({
    e <- eff() |> arrange(subjid, week)
    validate(need(nrow(e) > 0, "No efficacy data yet."))
    mn <- e |> group_by(week) |> summarise(m = mean(mcss), n = n(), .groups = "drop") |> filter(n >= MIN_N)
    sel <- input$subj
    p <- plot_ly() |>
      add_lines(data = group_by(e, subjid), x = ~week, y = ~mcss,
                line = list(color = "rgba(138,137,131,0.28)", width = 1),
                text = ~sprintf("%s<br>%s<br>MCSS %.1f", subjid, visit, mcss),
                hoverinfo = "text", name = "Individual subjects", showlegend = TRUE) |>
      add_trace(data = mn, x = ~week, y = ~m, type = "scatter", mode = "lines+markers",
                line = list(color = pal$blue, width = 2.5),
                marker = list(color = pal$blue, size = 8, line = list(color = pal$surface, width = 2)),
                text = ~sprintf("Week %s<br>Mean MCSS %.1f (n=%d)", week, m, n),
                hoverinfo = "text", name = "Mean")
    if (!is.null(sel) && sel != "None") {
      ss <- e |> filter(subjid == sel)
      p <- p |> add_trace(data = ss, x = ~week, y = ~mcss, type = "scatter", mode = "lines+markers",
                          line = list(color = pal$orange, width = 2.5),
                          marker = list(color = pal$orange, size = 8, line = list(color = pal$surface, width = 2)),
                          text = ~sprintf("%s<br>%s<br>MCSS %.1f", subjid, visit, mcss),
                          hoverinfo = "text", name = sel)
    }
    p |>
      pl_style("Study week", "MCSS (blinded)") |>
      layout(legend = list(orientation = "h", y = -0.22, x = 0),
             shapes = list(
        list(type = "rect", xref = "x", yref = "paper", x0 = -8, x1 = 0, y0 = 0, y1 = 1,
             fillcolor = "rgba(42,120,214,0.07)", line = list(width = 0), layer = "below"),
        list(type = "line", xref = "x", yref = "paper", x0 = 0, x1 = 0, y0 = 0, y1 = 1,
             line = list(color = pal$ink3, width = 1, dash = "dot"))),
        annotations = list(list(x = -4, y = 1, xref = "x", yref = "paper", text = "Run-in",
                                showarrow = FALSE, yanchor = "bottom", font = list(size = 10, color = pal$ink2)),
                           list(x = 0, y = 1, xref = "x", yref = "paper", text = "First dose",
                                showarrow = FALSE, xanchor = "left", yanchor = "bottom",
                                font = list(size = 10, color = pal$ink2))),
        xaxis = list(tickvals = VISITS$week))
  })

  output$p_igadist <- renderPlotly({
    d <- eff() |> filter(visit == input$iga_visit) |>
      count(iga) |> right_join(data.frame(iga = -3:3), by = "iga") |>
      mutate(n = coalesce(n, 0L), pct = 100 * n / max(sum(n), 1))
    validate(need(sum(d$n) > 0, "No data at this visit yet."))
    lab <- c("-3 Very much worse", "-2", "-1", "0 No change", "+1", "+2", "+3 Very much improved")
    plot_ly(d, x = ~factor(iga), y = ~n, type = "bar",
            marker = list(color = pal$blue, line = list(color = pal$surface, width = 2)),
            text = ~sprintf("%s<br>%d subjects (%.0f%%)", lab, n, pct), hoverinfo = "text",
            textposition = "none") |>
      pl_style("IGA score", "Subjects", legend = FALSE) |>
      layout(bargap = 0.25)
  })

  eff_tbl <- reactive({
    e <- eff()
    validate(need(nrow(e) > 0, "No efficacy data yet."))
    b  <- e |> filter(week == 0) |> select(subjid, base = mcss)
    ri <- e |> filter(week == -8) |> select(subjid, runin = mcss)
    e |> filter(week > 0) |> group_by(subjid) |> slice_max(visitnum, n = 1) |> ungroup() |>
      select(subjid, site, latest = visit, iga, mcss) |>
      left_join(b, by = "subjid") |> left_join(ri, by = "subjid") |>
      mutate(pchg = round(100 * (mcss - base) / base),
             flag = case_when(iga < 0 ~ "Worsening",
                              coalesce(abs(base - runin) > 3, FALSE) ~ "Run-in instability",
                              iga <= 0 ~ "No response",
                              TRUE ~ "")) |>
      arrange(desc(flag != ""), pchg)
  })

  output$tbl_eff <- renderDT({
    d <- eff_tbl() |>
      transmute(Subject = subjid, Site = site, `Latest visit` = latest, IGA = sprintf("%+d", as.integer(iga)),
                `MCSS run-in` = runin, `MCSS baseline` = base, `MCSS latest` = mcss,
                `% change` = pchg, Flag = flag_badge(flag))
    datatable(d, escape = FALSE, rownames = FALSE, selection = "single", options = dt_opts(8))
  })

  observeEvent(input$tbl_eff_rows_selected, {
    id <- eff_tbl()$subjid[input$tbl_eff_rows_selected]
    if (length(id)) updateSelectInput(session, "subj", selected = id)
  })

  # ---- Safety & PK ----------------------------------------------------------
  output$kpi_saf <- renderUI({
    s <- snap(); a <- s$ae; dosed <- sum(s$subj$dosed)
    pkn <- s$pk
    layout_columns(col_widths = rep(3, 4),
      kpi("Subjects with any AE", sprintf("%d / %d", n_distinct(a$subjid), dosed),
          sprintf("%d events", nrow(a))),
      kpi("Treatment-related AEs", sum(a$related), sprintf("%d subjects", n_distinct(a$subjid[a$related]))),
      kpi("Serious AEs", sum(a$serious), sprintf("%d related", sum(a$serious & a$related)),
          cls = if (sum(a$serious)) "flag" else ""),
      kpi("Max systemic study drug", if (nrow(pkn)) sprintf("%.2f ng/mL", max(pkn$conc)) else "-",
          sprintf("%d samples, %d BLQ", nrow(pkn), sum(pkn$blq)),
          cls = if (nrow(pkn) && max(pkn$conc) >= input$alert) "warn" else "")
    )
  })

  output$tbl_ae <- renderDT({
    s <- snap(); dosed <- max(sum(s$subj$dosed), 1)
    validate(need(nrow(s$ae) > 0, "No AEs reported yet."))
    d <- s$ae |> group_by(SOC = soc, `Preferred term` = pt) |>
      summarise(Subjects = n_distinct(subjid), Events = n(),
                Related = sum(related), Serious = sum(serious),
                `Max severity` = c("Mild", "Moderate", "Severe")[max(match(sev, c("Mild", "Moderate", "Severe")))],
                .groups = "drop") |>
      mutate(`% subj` = round(100 * Subjects / dosed, 1)) |>
      relocate(`% subj`, .after = Subjects) |>
      arrange(desc(Subjects)) |>
      select(-SOC)
    datatable(d, rownames = FALSE, options = dt_opts(10, order = list(list(1, "desc"))))
  })

  output$p_aetime <- renderPlotly({
    a <- snap()$ae |> arrange(subjid)
    validate(need(nrow(a) > 0, "No AEs reported yet."))
    a <- a |> mutate(rel = if_else(related, "Treatment-related", "Not related"),
                     y = factor(subjid, levels = rev(sort(unique(subjid)))))
    p <- plot_ly()
    for (g in c("Treatment-related", "Not related")) {
      d <- a |> filter(rel == g)
      if (!nrow(d)) next
      p <- p |> add_trace(
        data = d, x = ~day, y = ~y, type = "scatter", mode = "markers", name = g,
        marker = list(color = if (g == "Treatment-related") pal$orange else pal$blue,
                      size = ~if_else(serious, 13, 9),
                      symbol = ~if_else(serious, "diamond", "circle"),
                      line = list(color = pal$surface, width = 2)),
        text = ~sprintf("%s<br>%s<br>Day %d | %s%s", subjid, pt, day, sev, if_else(serious, " | SERIOUS", "")),
        hoverinfo = "text")
    }
    p |> pl_style("Study day of onset", NULL) |>
      layout(yaxis = list(title = "", showticklabels = n_distinct(a$subjid) <= 30, tickfont = list(size = 9)))
  })

  output$p_pk <- renderPlotly({
    k <- snap()$pk
    validate(need(nrow(k) > 0, "No PK results yet."))
    set.seed(1)
    k <- k |> mutate(x = match(week, PK_WEEKS) + runif(n(), -0.18, 0.18),
                     y = if_else(blq, LLOQ / 2, conc),
                     flag = !blq & conc >= input$alert)
    hl <- function(y, col, dash, w = 1.5) list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = y, y1 = y,
                                               line = list(color = col, dash = dash, width = w))
    plot_ly(k, x = ~x, y = ~y, type = "scatter", mode = "markers",
            marker = list(color = ~if_else(flag, pal$warning, pal$blue),
                          size = ~if_else(flag, 12, 8), line = list(color = pal$surface, width = 2)),
            text = ~sprintf("%s<br>%s<br>study drug %s ng/mL", subjid, visit, result),
            hoverinfo = "text", showlegend = FALSE) |>
      pl_style(NULL, "study drug (ng/mL)", legend = FALSE) |>
      layout(xaxis = list(tickvals = 1:3, ticktext = paste("Week", PK_WEEKS), range = c(0.5, 3.5)),
             yaxis = list(range = c(0, 2.3)),
             shapes = list(hl(PK_LIMIT, pal$critical, "dash", 2), hl(input$alert, pal$warning, "dash"),
                           hl(LLOQ, pal$ink3, "dot", 1)),
             annotations = list(
               list(xref = "paper", x = 1, y = PK_LIMIT, text = sprintf("Threshold %.1f", PK_LIMIT),
                    showarrow = FALSE, xanchor = "right", yanchor = "bottom", font = list(size = 10, color = pal$ink2)),
               list(xref = "paper", x = 1, y = input$alert, text = sprintf("Alert %.1f", input$alert),
                    showarrow = FALSE, xanchor = "right", yanchor = "bottom", font = list(size = 10, color = pal$ink2))))
  })

  output$tbl_pk <- renderDT({
    k <- snap()$pk |> filter(!blq, conc >= input$alert) |>
      transmute(Subject = subjid, Site = site, Visit = visit, `Sample date` = sdate,
                `ng/mL` = result, `% of threshold` = round(100 * conc / PK_LIMIT)) |>
      arrange(desc(`% of threshold`))
    datatable(k, rownames = FALSE, options = dt_opts(8))
  })

  # ---- Data quality ---------------------------------------------------------
  output$kpi_dq <- renderUI({
    s <- snap(); v <- s$vis |> filter(status != "Not yet due", status != "Discontinued")
    open <- s$q |> filter(status != "Closed")
    layout_columns(col_widths = rep(3, 4),
      kpi("Visits complete in window", if (nrow(v)) sprintf("%.0f%%", 100 * mean(v$status == "Complete")) else "-",
          sprintf("%d of %d due", sum(v$status == "Complete"), nrow(v))),
      kpi("Overdue / not entered", sum(v$status == "Overdue / not entered"),
          sprintf("%d missed", sum(v$status == "Missed")),
          cls = if (sum(v$status == "Overdue / not entered")) "warn" else ""),
      kpi("Unresolved queries", nrow(open), sprintf("%d open, %d answered", sum(open$status == "Open"),
                                                     sum(open$status == "Answered"))),
      kpi("Median query age", if (nrow(open)) sprintf("%.0f days", median(open$age_days)) else "-",
          sprintf("%d older than 30 days", sum(open$age_days > 30)))
    )
  })

  output$p_status <- renderPlotly({
    v <- snap()$vis
    validate(need(nrow(v) > 0, "No subjects."))
    ids <- sort(unique(v$subjid))
    v <- v |> mutate(code = match(status, STATUS_LEVELS),
                     xi = match(visit, VISIT_ORDER), yi = match(subjid, ids))
    z <- matrix(NA, length(ids), length(VISIT_ORDER))
    txt <- matrix("", length(ids), length(VISIT_ORDER))
    z[cbind(v$yi, v$xi)] <- v$code
    txt[cbind(v$yi, v$xi)] <- sprintf("%s<br>%s<br>%s%s", v$subjid, v$visit, v$status,
                                      ifelse(is.na(v$days_overdue), "", sprintf(" (%d days)", as.integer(v$days_overdue))))
    k <- length(STATUS_LEVELS)
    cs <- list()
    for (i in seq_len(k)) {
      cs[[length(cs) + 1]] <- list((i - 1) / k, STATUS_COLS[i])
      cs[[length(cs) + 1]] <- list(i / k, STATUS_COLS[i])
    }
    h <- max(320, 18 * length(ids) + 80)
    p <- plot_ly(x = VISIT_ORDER, y = ids, z = z, type = "heatmap", text = txt, hoverinfo = "text",
                 colorscale = cs, zmin = 0.5, zmax = k + 0.5, xgap = 2, ygap = 2, showscale = FALSE,
                 height = h)
    p |> pl_style(NULL, NULL, legend = FALSE) |>
      layout(yaxis = list(autorange = "reversed", tickfont = list(size = 9), showgrid = FALSE),
             xaxis = list(side = "top", tickangle = -30, tickfont = list(size = 10), showgrid = FALSE),
             margin = list(t = 60))
  })

  output$p_queries <- renderPlotly({
    q <- snap()$q |> filter(status != "Closed")
    validate(need(nrow(q) > 0, "No unresolved queries."))
    lev <- c("0-7 days", "8-14 days", "15-30 days", "> 30 days")
    d <- q |> mutate(bucket = base::cut(age_days, c(-Inf, 7, 14, 30, Inf), labels = lev)) |>
      count(site, bucket) |> complete(site, bucket, fill = list(n = 0L))
    ord <- d |> group_by(site) |> summarise(t = sum(n)) |> arrange(t) |> pull(site)
    p <- plot_ly()
    for (i in seq_along(lev)) {
      dd <- d |> filter(bucket == lev[i])
      p <- p |> add_bars(data = dd, y = ~factor(site, levels = ord), x = ~n, orientation = "h", name = lev[i],
                         marker = list(color = pal$seq[i], line = list(color = pal$surface, width = 2)),
                         text = ~sprintf("Site %s<br>%s: %d", site, lev[i], n), hoverinfo = "text",
                         textposition = "none")
    }
    p |> pl_style("Unresolved queries", "Site") |>
      layout(barmode = "stack", legend = list(orientation = "h", y = -0.25, x = 0, traceorder = "normal"))
  })

  output$tbl_issues <- renderDT({
    s <- snap()
    v <- s$vis |> filter(status %in% c("Overdue / not entered", "Missed", "Out of window")) |>
      transmute(Type = status, Subject = subjid, Site = site, Item = visit,
                Detail = case_when(status == "Out of window" ~ sprintf("Visit %+d days from target (window +/-%d)", as.integer(offset), as.integer(window)),
                                   status == "Missed" ~ "Visit not performed",
                                   TRUE ~ sprintf("Due %s; nothing entered", format(target, "%d %b"))),
                Days = coalesce(days_overdue, 0))
    q <- s$q |> filter(status != "Closed") |>
      transmute(Type = status, Subject = subjid, Site = site, Item = paste(qid, visit, sep = " | "),
                Detail = text, Days = age_days)
    d <- bind_rows(v, q) |> arrange(desc(Days)) |> mutate(Type = badge(Type))
    datatable(d, escape = FALSE, rownames = FALSE, options = dt_opts(10))
  })
}

shinyApp(ui, server)
