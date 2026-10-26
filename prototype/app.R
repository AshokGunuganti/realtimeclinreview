# =============================================================================
#  Phase 3 Clinical Data Review  |  modular R Shiny app
#
#  Layers:  data/source (raw EDC + vendor files)
#        -> R/adapters.R   read + type sources        (spec: Sources, Source Variables)
#        -> R/derive.R     review datasets RV*         (spec: Review Datasets, Variable Mapping)
#        -> R/snapshot.R   as-of view + change log     (spec: Derivation Rules)
#        -> R/mod_*.R      one Shiny module per screen (spec: Screen Traceability)
#
#  ALL DATA ARE SIMULATED. Run:  shiny::runApp()   (from this folder)
# =============================================================================
suppressPackageStartupMessages({
  library(shiny); library(bslib); library(dplyr); library(tidyr); library(DT); library(plotly)
})
for (f in sort(list.files("R", pattern = "\\.R$", full.names = TRUE))) source(f)

SRC <- read_sources(APP_CFG$source_dir)
RV  <- derive_review(SRC, load_schedule())
SHEET_ORDER <- c("Changes", "Issues", "Site performance", "Low compliance", "Efficacy by subject", "AE summary",
                 "AE listing", "PK alerts", "Visit status", "Source checks", "Check findings")

ui <- page_navbar(
  id = "tabs", title = "Phase 3 | Clinical Data Review", fillable = FALSE,
  theme = bs_theme(version = 5, bg = PAL$surface, fg = PAL$ink, primary = PAL$blue, "border-radius" = "8px",
                   base_font = font_collection("Inter", "system-ui", "-apple-system", "Segoe UI", "sans-serif")),
  header = tags$head(tags$style(HTML(APP_CSS))),
  sidebar = controls_ui("ctl", sort(unique(RV$subj$SITEID))),
  nav_panel("What's new",          value = "new",      whatsnew_ui("new")),
  nav_panel("Efficacy",            value = "efficacy", efficacy_ui("eff", RV$sched)),
  nav_panel("Safety & PK",         value = "safety",   safety_ui("saf")),
  nav_panel("Data quality",        value = "dq",       quality_ui("dq")),
  nav_panel("Sites & compliance",  value = "sites",    sites_ui("sites")),
  nav_panel("Subject profile",     value = "profile",  profile_ui("prof")),
  nav_panel("Source checks",       value = "checks",   checks_ui("chk")),
  nav_spacer(),
  nav_item(tags$span(class = "note", "Simulated data"))
)

server <- function(input, output, session) {
  registry <- reactiveValues()
  ctrl <- controls_server("ctl", registry, SHEET_ORDER)
  snap <- reactive(review_snapshot(RV, ctrl$asof(), ctrl$site(), ctrl$pop()))
  selected <- reactiveVal(NULL)

  whatsnew_server("new",   snap, ctrl, registry)
  efficacy_server("eff",   snap, ctrl, registry)
  safety_server("saf",     snap, ctrl, registry)
  quality_server("dq",     snap, ctrl, registry)
  sites_server("sites",    snap, ctrl, registry)
  profile_server("prof",   snap, ctrl, selected)
  checks_server("chk", SRC, RV, snap, ctrl, registry)

  # Cross-module navigation: subject links and attention-panel links
  observeEvent(input$goto_subject, { selected(input$goto_subject); nav_select("tabs", "profile") })
  observeEvent(input$goto_tab, nav_select("tabs", input$goto_tab))
}

shinyApp(ui, server)
