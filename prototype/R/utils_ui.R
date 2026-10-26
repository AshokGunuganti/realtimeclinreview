# =============================================================================
#  Shared UI helpers: palette, KPI tiles, badges, subject links, plot styling,
#  Excel writer.
# =============================================================================

PAL <- list(
  blue = "#2a78d6", orange = "#eb6834", good = "#0ca30c", warning = "#fab219", serious = "#ec835a",
  critical = "#d03b3b", ink = "#0b0b0b", ink2 = "#52514e", ink3 = "#8a8983", grid = "#ecebe7",
  surface = "#fcfcfb", seq = c("#c6dcf5", "#86b4ec", "#2a78d6", "#18508f"))
STATUS_COLS <- c("Complete" = "#b6dcb6", "Out of window" = PAL$warning, "Overdue / not entered" = PAL$serious,
                 "Missed" = PAL$critical, "Not yet due" = "#e9e8e3", "Discontinued" = "#9d9c96")

APP_CSS <- "
  .sim-banner{background:#fff4dc;border:1px solid #f2d18a;color:#5a4410;font-size:.82rem;padding:.35rem .8rem;border-radius:6px;margin-bottom:.75rem}
  .kpi{border:1px solid #e7e6e2;border-radius:10px;padding:.7rem .9rem;background:#fff;height:100%}
  .kpi .lab{font-size:.72rem;color:#52514e;text-transform:uppercase;letter-spacing:.04em}
  .kpi .val{font-size:1.6rem;font-weight:650;line-height:1.15;color:#0b0b0b;white-space:nowrap}
  .kpi .sub{font-size:.78rem;color:#52514e}
  .kpi.flag{border-left:4px solid #d03b3b}.kpi.warn{border-left:4px solid #fab219}
  .badge-s{display:inline-block;padding:.1rem .45rem;border-radius:999px;font-size:.72rem;font-weight:600;white-space:nowrap}
  .b-crit{background:#fbe3e3;color:#8f1f1f}.b-warn{background:#fff1cf;color:#6b4a00}.b-ser{background:#fde8df;color:#8a3a17}
  .b-new{background:#e3eefb;color:#174b8f}.b-chg{background:#ecebe7;color:#3b3a37}.b-ok{background:#e4f3e4;color:#145c14}
  .card-header{font-weight:600;font-size:.92rem;display:flex;justify-content:space-between;align-items:center;gap:.5rem}
  .btn-xl{font-size:.75rem;padding:.2rem .55rem;color:#145c14;background:#eef6ee;border:1px solid #9fcf9f}
  .note{font-size:.78rem;color:#6b6a65}
  a.sid{color:#2a78d6;font-weight:600;text-decoration:none;white-space:nowrap;cursor:pointer}
  a.sid:hover{text-decoration:underline}
  .att{display:flex;justify-content:space-between;align-items:center;padding:.45rem 0;border-bottom:1px solid #ecebe7;gap:.5rem}
  .att a{color:#0b0b0b;text-decoration:none;cursor:pointer}.att a:hover{text-decoration:underline}
  table.dataTable{font-size:.83rem}
  .legend-row{display:flex;flex-wrap:wrap;gap:.3rem 1rem;font-size:.78rem;color:#52514e;margin-bottom:.3rem}
  .sw{display:inline-block;width:11px;height:11px;border-radius:2px;margin-right:5px;vertical-align:-1px}
  .bslib-card .card-body{min-height:auto}
"

kpi <- function(label, value, sub = NULL, cls = "") {
  shiny::div(class = paste("kpi", cls), shiny::div(class = "lab", label), shiny::div(class = "val", value),
             if (!is.null(sub)) shiny::div(class = "sub", sub))
}

BADGE_CLS <- c("SAE" = "b-crit", "PK alert" = "b-warn", "Review" = "b-ser", "Low compliance" = "b-warn",
               "New" = "b-new", "Changed" = "b-chg", "Open" = "b-ser", "Answered" = "b-warn", "Closed" = "b-ok",
               "Missed" = "b-crit", "Overdue / not entered" = "b-ser", "Out of window" = "b-warn", "Complete" = "b-ok",
               "Not yet due" = "b-chg", "Discontinued" = "b-chg", "Worsening" = "b-crit", "Run-in instability" = "b-warn",
               "No response" = "b-ser", "Needs attention" = "b-ser", "Watch" = "b-warn", "On track" = "b-ok",
               "Pass" = "b-ok", "Error" = "b-crit", "Warning" = "b-warn", "Info" = "b-new", "Withdrawn" = "b-chg",
               "Ongoing" = "b-new", "Run-in" = "b-chg")
badge <- function(x) {
  cls <- BADGE_CLS[x]; cls[is.na(cls)] <- "b-new"
  ifelse(is.na(x) | x == "", "", sprintf('<span class="badge-s %s">%s</span>', cls, htmltools::htmlEscape(x)))
}
sid_link <- function(id) sprintf(
  "<a class=\"sid\" onclick=\"Shiny.setInputValue('goto_subject','%s',{priority:'event'})\">%s</a>", id, id)
tab_link <- function(label, tab) sprintf(
  "<a onclick=\"Shiny.setInputValue('goto_tab','%s',{priority:'event'})\">%s</a>", tab, htmltools::htmlEscape(label))
strip_html <- function(x) if (is.character(x)) gsub("<[^>]+>", "", x) else x
fmt_d <- function(d) ifelse(is.na(d), "", format(d, "%d %b %Y"))
dash <- function(x, f = identity) ifelse(is.na(x), "–", f(x))

legend_row <- function(items) {
  shiny::div(class = "legend-row", lapply(names(items), function(n)
    shiny::span(shiny::span(class = "sw", style = sprintf("background:%s", items[[n]])), n)))
}

pl_style <- function(p, xtitle = NULL, ytitle = NULL, legend = TRUE) {
  ax <- list(gridcolor = PAL$grid, zeroline = FALSE, linecolor = PAL$grid,
             tickfont = list(color = PAL$ink2, size = 11), titlefont = list(color = PAL$ink2, size = 12))
  p |>
    plotly::layout(font = list(family = "Inter, system-ui, sans-serif", color = PAL$ink),
                   paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
                   xaxis = c(ax, list(title = xtitle)), yaxis = c(ax, list(title = ytitle)),
                   showlegend = legend, legend = list(orientation = "h", y = -0.22, x = 0, font = list(size = 11, color = PAL$ink2)),
                   hoverlabel = list(bgcolor = "white", bordercolor = PAL$grid, font = list(color = PAL$ink)),
                   margin = list(l = 10, r = 10, t = 25, b = 10)) |>
    plotly::config(displaylogo = FALSE, modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"))
}
hline <- function(y, col, dash = "dash", w = 1.5) list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = y, y1 = y,
                                                       line = list(color = col, dash = dash, width = w))
vline <- function(x, col, dash = "dash", w = 1.5) list(type = "line", yref = "paper", y0 = 0, y1 = 1, x0 = x, x1 = x,
                                                       line = list(color = col, dash = dash, width = w))

#' Write named list of data frames to an xlsx (About sheet first); HTML stripped, dates ISO
write_review_xlsx <- function(sheets, path, about) {
  clean <- function(df) { df <- as.data.frame(df, check.names = FALSE)
    df[] <- lapply(df, function(col) { col <- strip_html(col)
      if (inherits(col, "Date")) format(col, "%Y-%m-%d") else if (is.character(col)) gsub("&lt;", "<", gsub("&gt;", ">", gsub("&amp;", "&", col))) else col })
    df }
  nm <- function(x) substr(gsub("[\\[\\]:*?/\\\\]", "-", x, perl = TRUE), 1, 31)
  out <- c(list(About = clean(about)), lapply(sheets, clean))
  names(out) <- make.unique(nm(names(out)), sep = " ")
  writexl::write_xlsx(out, path, format_headers = TRUE)
}
