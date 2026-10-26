# Over-time chart: a parameter by visit, with individual subject lines and the
# mean with a 95% confidence interval where at least `min_n` values exist.
# Params: form, value, title, y_label, shade (visit numbers to shade, e.g. run-in),
# min_n (default 3), band (list(lo, hi) reference band).

overtime_ui <- function(id, params, study) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::card(fill = FALSE, bslib::card_header(if (is.null(params$title)) params$value else params$title),
                plotly::plotlyOutput(ns("plot"), height = "420px")),
    table_card_ui(ns("sum"), "Summary by visit")
  )
}

overtime_server <- function(id, ctx, params) {
  shiny::moduleServer(id, function(input, output, session) {
    min_n <- if (is.null(params$min_n)) 3 else as.numeric(params$min_n)
    sched <- ctx$study$schedule
    vis_label <- function(v) {
      l <- if ("VISIT" %in% names(sched)) sched$VISIT[match(v, sched$VISITNUM)] else NA
      ifelse(is.na(l), paste("Visit", v), l)
    }
    points <- shiny::reactive({
      t <- ctx$data()$tables[[params$form]]
      d <- data.frame(SUBJID = t$SUBJID, VISITNUM = t$VISITNUM, Y = suppressWarnings(as.numeric(t[[params$value]])))
      d[!is.na(d$Y) & !is.na(d$VISITNUM), , drop = FALSE]
    })
    by_visit <- shiny::reactive({ visit_summary(points(), min_n) })
    sum_tbl <- shiny::reactive({
      s <- by_visit()
      data.frame(Visit = vis_label(s$VISITNUM), n = s$N, Mean = round(s$MEAN, 2), SD = round(s$SD, 2),
                 `95% CI` = ifelse(is.na(s$LO), "", sprintf("%.2f to %.2f", s$LO, s$HI)), check.names = FALSE)
    })
    table_card_server("sum", sum_tbl, ctx, paste(params$form, params$value))
    output$plot <- plotly::renderPlotly({
      d <- points()
      s <- by_visit()
      xs <- sort(unique(c(d$VISITNUM, s$VISITNUM)))
      d$X <- match(d$VISITNUM, xs)
      s$X <- match(s$VISITNUM, xs)
      shapes <- lapply(intersect(as.numeric(unlist(params$shade)), xs), function(v) {
        i <- match(v, xs)
        list(type = "rect", xref = "x", yref = "paper", x0 = i - 0.5, x1 = i + 0.5, y0 = 0, y1 = 1,
             fillcolor = "#ecebe7", opacity = 0.6, line = list(width = 0), layer = "below")
      })
      if (!is.null(params$band)) {
        shapes[[length(shapes) + 1]] <- list(type = "rect", xref = "paper", x0 = 0, x1 = 1, y0 = params$band$lo,
                                             y1 = params$band$hi, fillcolor = "#e4f3e4", opacity = 0.5,
                                             line = list(width = 0), layer = "below")
      }
      p <- plotly::plot_ly()
      if (nrow(d)) {
        p <- plotly::add_lines(p, data = d[order(d$SUBJID, d$X), ], x = ~X, y = ~Y, split = ~SUBJID,
                               line = list(color = "rgba(138,137,131,0.35)", width = 1), hoverinfo = "text",
                               text = ~paste(SUBJID, Y), showlegend = FALSE)
      }
      s <- s[s$N >= min_n, , drop = FALSE]
      if (nrow(s)) {
        p <- plotly::add_trace(p, data = s, x = ~X, y = ~MEAN, type = "scatter", mode = "lines+markers",
                               line = list(color = "#2a78d6", width = 3), marker = list(color = "#2a78d6", size = 8),
                               error_y = list(type = "data", symmetric = FALSE, array = s$HI - s$MEAN,
                                              arrayminus = s$MEAN - s$LO, color = "#2a78d6"),
                               name = sprintf("Mean (95%% CI, n >= %d)", min_n), hoverinfo = "text",
                               text = ~sprintf("%s: mean %.2f, n = %d", vis_label(VISITNUM), MEAN, N))
      }
      plotly::layout(p, shapes = shapes,
                     xaxis = list(title = "", tickmode = "array", tickvals = seq_along(xs), ticktext = vis_label(xs)),
                     yaxis = list(title = if (is.null(params$y_label)) params$value else params$y_label),
                     legend = list(orientation = "h"))
    })
  })
}

visit_summary <- function(d, min_n) {
  if (!nrow(d)) return(data.frame(VISITNUM = numeric(), N = integer(), MEAN = numeric(), SD = numeric(),
                                  LO = numeric(), HI = numeric()))
  s <- do.call(rbind, lapply(split(d$Y, d$VISITNUM), function(y) {
    n <- length(y)
    m <- mean(y)
    sd <- if (n > 1) stats::sd(y) else NA_real_
    hw <- if (n >= max(min_n, 2)) stats::qt(0.975, n - 1) * sd / sqrt(n) else NA_real_
    data.frame(N = n, MEAN = m, SD = sd, LO = m - hw, HI = m + hw)
  }))
  cbind(VISITNUM = as.numeric(names(split(d$Y, d$VISITNUM))), s)
}
