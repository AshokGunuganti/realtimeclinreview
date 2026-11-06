# Subject timeline: every lane on one study-day scale (Day 1 = first dose, no
# day 0), drawn as plain SVG so it stays quiet and readable. Lanes come from
# the profile's `lanes` params, one of four types:
#   visits    {form, day, label, not_done: {field, value}}: visit dots; a visit
#             not done sits at its scheduled day as a hollow red dot.
#   events    {form, day, label, level, ongoing: {field, values}, serious:
#             {field, value}}: one row per event, coloured by severity (mild
#             gold, moderate orange, severe red; palette.R), a ring and SAE tag
#             when serious, and a bar to last contact while ongoing.
#   series    {form, day or date, value, below, ref, ref_label}: a line over
#             time; `below` marks values below the limit (drawn hollow on the
#             reference line).
#   intervals {form, start, end, value, ref, ref_label, unit}: a bar per
#             period, e.g. dosing compliance per diary interval.
# `ref` is a number, a study threshold name (study.yml thresholds) or a column
# whose first value is used (e.g. BASE). Every lane takes `title`, `note` and
# `colour`; without a colour, lanes other than events take the series palette
# in their configured order, so a lane keeps its colour for every subject.

paper <- c(page = "#f8fafb", line = "#dce3e8", line2 = "#c5d0d8", text = "#212529", muted = "#5f6b7a",
           accent = "#0b7285", accent_t = "#e3f1f4", red = "#c92a2a", amber = "#9a6700", amber_t = "#fdf0d2")
lane_types <- c("visits", "events", "series", "intervals")
lane_label_width <- 150
timeline_width <- 1080
event_row_height <- 19

# Study day from a date: days since first dose, counting first dose as Day 1.
study_day <- function(date, trtsdt) {
  d <- as.numeric(as.Date(substr(as.character(date), 1, 10)) - as.Date(trtsdt))
  ifelse(d >= 0, d + 1, d)
}

`%|na|%` <- function(a, b) if (is.null(a) || is.na(a)) b else a

lane_ref <- function(ref, t, thresholds) {
  if (is.null(ref)) return(NA_real_)
  if (is.numeric(ref)) return(ref)
  if (!is.null(thresholds[[ref]])) return(as.numeric(thresholds[[ref]]))
  if (ref %in% names(t)) return(suppressWarnings(as.numeric(stats::na.omit(t[[ref]])[1])))
  NA_real_
}

lane_days <- function(lane, t, trtsdt) {
  if (!is.null(lane$day)) return(suppressWarnings(as.numeric(t[[lane$day]])))
  if (is.na(trtsdt)) return(rep(NA_real_, nrow(t)))
  study_day(t[[lane$date]], trtsdt)
}

visit_points <- function(lane, t, trtsdt, schedule) {
  nd <- lane$not_done
  missed <- if (is.null(nd)) rep(FALSE, nrow(t)) else !is.na(t[[nd$field]]) & t[[nd$field]] == nd$value
  day <- lane_days(lane, t, trtsdt)
  planned <- 7 * schedule$NOMWK[match(t$VISITNUM, schedule$VISITNUM)] + 1
  day[missed & is.na(day)] <- planned[missed & is.na(day)]
  short <- if ("VISSHORT" %in% names(schedule)) schedule$VISSHORT[match(t$VISITNUM, schedule$VISITNUM)] else NA
  data.frame(DAY = day, LABEL = ifelse(is.na(short), as.character(t[[lane$label]]), short), MISSED = missed)
}

# One lane's points for one subject, ready to draw.
lane_data <- function(lane, tables, subj, adsl, schedule, thresholds) {
  if (!identical(length(lane$type), 1L) || !lane$type %in% lane_types) {
    stop("Profile lane type must be one of ", paste(lane_types, collapse = ", "), ".", call. = FALSE)
  }
  t <- tables[[lane$form]]
  t <- if (is.null(t)) data.frame() else t[t$SUBJID == subj, , drop = FALSE]
  a <- adsl[adsl$SUBJID == subj, , drop = FALSE]
  trtsdt <- if (nrow(a)) a$TRTSDT[1] else NA
  out <- list(lane = lane, n = nrow(t), ref = lane_ref(lane$ref, t, thresholds))
  if (!nrow(t)) return(c(out, list(points = data.frame())))
  og <- lane$ongoing
  pts <- switch(lane$type,
    visits = visit_points(lane, t, trtsdt, schedule),
    events = data.frame(DAY = lane_days(lane, t, trtsdt), LABEL = as.character(t[[lane$label]]),
                        LEVEL = if (is.null(lane$level)) "" else toupper(as.character(t[[lane$level]])),
                        ONGOING = if (is.null(og)) FALSE else as.character(t[[og$field]]) %in% unlist(og$values),
                        SERIOUS = if (is.null(lane$serious)) FALSE
                                  else as.character(t[[lane$serious$field]]) %in% unlist(lane$serious$value)),
    series = data.frame(DAY = lane_days(lane, t, trtsdt), VALUE = suppressWarnings(as.numeric(t[[lane$value]])),
                        BELOW = if (is.null(lane$below)) FALSE else as.logical(t[[lane$below]]) %in% TRUE),
    intervals = data.frame(START = study_day(t[[lane$start]], trtsdt), END = study_day(t[[lane$end]], trtsdt),
                           VALUE = suppressWarnings(as.numeric(t[[lane$value]]))))
  timed <- lane$type == "intervals"
  pts <- pts[if (timed) !is.na(pts$START) & !is.na(pts$END) & !is.na(pts$VALUE) else !is.na(pts$DAY), , drop = FALSE]
  c(out, list(points = pts[order(if (timed) pts$START else pts$DAY), , drop = FALSE]))
}

level_colour <- function(level) severity_colour(ifelse(nzchar(level), level, "MILD"))

# Each lane's colour: its own `colour`, else the next series slot among the
# lanes that are not events (events take severity colours).
lane_colours <- function(lanes) {
  slot <- cumsum(vapply(lanes, function(l) !identical(l$lane$type, "events"), TRUE))
  vapply(seq_along(lanes), function(i) lanes[[i]]$lane$colour %||% series_colour(slot[i]), "")
}

fmt_num <- function(v) format(signif(v, 3), trim = TRUE, drop0trailing = TRUE)

# SVG primitives, vectorised; coordinates rounded so the markup stays short.
sv_text <- function(x, y, s, size = 11, fill = paper[["muted"]], anchor = "start", weight = 400) {
  sprintf('<text x="%.1f" y="%.1f" font-size="%d" fill="%s" text-anchor="%s" font-weight="%s">%s</text>',
          x, y, size, fill, anchor, weight, htmltools::htmlEscape(s))
}
sv_line <- function(x1, y1, x2, y2, stroke = paper[["line"]], width = 1, dash = NULL) {
  sprintf('<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" stroke="%s" stroke-width="%s"%s/>', x1, y1, x2, y2, stroke,
          width, if (is.null(dash)) "" else sprintf(' stroke-dasharray="%s"', dash))
}
sv_dot <- function(x, y, r = 4, fill = paper[["accent"]], stroke = "none") {
  sprintf('<circle cx="%.1f" cy="%.1f" r="%s" fill="%s" stroke="%s" stroke-width="1.5"/>', x, y, r, fill, stroke)
}
sv_rect <- function(x, y, w, h, fill) {
  sprintf('<rect x="%.1f" y="%.1f" width="%.1f" height="%.1f" rx="2" fill="%s"/>', x, y, pmax(w, 0), pmax(h, 0), fill)
}
sv_path <- function(x, y, stroke = paper[["accent"]], width = 2) {
  sprintf('<path d="%s" fill="none" stroke="%s" stroke-width="%s" stroke-linejoin="round" stroke-linecap="round"/>',
          paste0(c("M", rep("L", length(x) - 1)), sprintf("%.1f,%.1f", x, y), collapse = " "), stroke, width)
}
# The area under a line down to `base`, filled with a fading gradient.
sv_area <- function(x, y, base, fill) {
  sprintf('<path d="M%.1f,%.1f %s L%.1f,%.1f Z" fill="%s" stroke="none"/>', x[1], base,
          paste0("L", sprintf("%.1f,%.1f", x, y), collapse = " "), x[length(x)], base, fill)
}
sv_gradient <- function(id, colour) {
  sprintf(paste0('<linearGradient id="%s" x1="0" y1="0" x2="0" y2="1"><stop offset="0" stop-color="%s" stop-opacity=".28"/>',
                 '<stop offset="1" stop-color="%s" stop-opacity="0"/></linearGradient>'), id, colour, colour)
}
# Rough width of a label in Inter, for placing labels clear of the edge.
text_width <- function(s, size) nchar(s) * size * 0.55
sae_width <- 29

# A rounded pill with text, e.g. an SAE tag.
sv_pill <- function(x, y, s, colour, size = 10) {
  w <- nchar(s) * size * 0.62 + 10
  paste0(sprintf('<rect x="%.1f" y="%.1f" width="%.1f" height="%.1f" rx="%.1f" fill="%s"/>', x, y - size, w, size + 5,
                 (size + 5) / 2, tint(colour, 0.82)),
         sv_text(x + w / 2, y - 1, s, size, paper[["text"]], "middle", 600))
}

# The whole timeline for a subject: `lanes` from lane_data(), the review date
# and last contact as study days, and `pulls` (subject_pulls()) drawn as faint
# vertical lines, those since the "changes since" date (`since_day`) in teal,
# numbered in a row along the bottom.
timeline_svg <- function(lanes, review_day, last_contact, pulls = NULL, since_day = NA) {
  cols <- lane_colours(lanes)
  has <- vapply(lanes, function(l) nrow(l$points) > 0, TRUE)
  lanes <- lanes[has]
  cols <- cols[has]
  if (!length(lanes)) return(shiny::p(class = "note", "No dated records for this subject yet."))
  days <- unlist(lapply(lanes, function(l) c(l$points$DAY, l$points$START, l$points$END)))
  d0 <- min(c(days, 1), na.rm = TRUE) - 4
  d1 <- max(c(days, review_day), na.rm = TRUE) + 6
  x <- function(d) lane_label_width + (d - d0) / (d1 - d0) * (timeline_width - lane_label_width - 20)
  ticks <- pretty(c(d0, d1), 7)
  ticks <- ticks[ticks >= d0 & ticks <= d1]
  out <- c(sv_line(x(ticks), 26, x(ticks), 26, paper[["line"]]),
           sv_text(x(ticks), 20, ifelse(ticks == 0, "Day 1", paste("Day", ticks)), anchor = "middle"))
  defs <- character()
  y <- 34
  for (i in seq_along(lanes)) {
    l <- lanes[[i]]
    l$colour <- cols[i]
    l$gradient <- paste0("tlg", i)
    defs <- c(defs, sv_gradient(l$gradient, l$colour))
    h <- lane_height(l)
    band <- if (l$lane$type == "events") "#fbf7f5" else tint(l$colour, 0.955)
    out <- c(out, sv_rect(lane_label_width - 8, y + 3, timeline_width - lane_label_width + 8, h - 6, band),
             sv_line(0, y, timeline_width, y),
             if (l$lane$type != "events") sv_rect(0, y + 13, 4, 13, l$colour),
             sv_text(if (l$lane$type == "events") 0 else 10, y + 24, l$lane$title %|na|% l$lane$form, 13, paper[["text"]],
                     weight = 600),
             if (!is.null(l$lane$note)) sv_text(if (l$lane$type == "events") 0 else 10, y + 40, l$lane$note),
             if (l$lane$type == "events") severity_key_svg(l$points, y + 58),
             draw_lane(l, x, y, h, last_contact))
    y <- y + h
  }
  out <- c(out, sv_line(0, y, timeline_width, y), sv_line(x(1), 26, x(1), y, paper[["accent"]], 1, "3 3"),
           if (!is.na(review_day)) c(sv_line(x(review_day), 26, x(review_day), y, paper[["muted"]], 1, "2 4"),
                                     sv_text(x(review_day) - 4, y - 6, "review date", 11, anchor = "end")))
  if (!is.null(pulls) && nrow(pulls)) {
    pulls <- pulls[pulls$DAY >= d0 & pulls$DAY <= d1, , drop = FALSE]
    recent <- !is.na(since_day) & pulls$DAY > since_day
    col <- ifelse(recent, paper[["accent"]], paper[["line2"]])
    # Behind every lane: drawn first so the data sits on top.
    out <- c(sv_line(x(pulls$DAY), 26, x(pulls$DAY), y, col, ifelse(recent, 1.2, 1), "1 3"), out,
             sv_text(0, y + 20, "Data pulls", 12, paper[["text"]], weight = 600),
             pull_tick_labels(x(pulls$DAY), y + 20, pulls$PULL, recent))
    y <- y + 30
  }
  shiny::HTML(sprintf('<svg class="tl" viewBox="0 0 %d %d" role="img" aria-label="Subject timeline by study day"><defs>%s</defs>%s</svg>',
                      timeline_width, round(y + 4), paste(defs, collapse = ""), paste(out, collapse = "")))
}

# Pull numbers along the bottom, skipping any that would overlap the last one shown.
pull_tick_labels <- function(xs, y, n, recent, gap = 26) {
  last <- -Inf
  keep <- logical(length(xs))
  for (i in seq_along(xs)) if (xs[i] - last >= gap || i == length(xs)) {
    keep[i] <- TRUE
    last <- xs[i]
  }
  sv_text(xs[keep], y, paste0("P", n[keep]), 10, ifelse(recent[keep], paper[["accent"]], paper[["muted"]]), "middle",
          ifelse(recent[keep], 600, 400))
}

lane_height <- function(l) {
  switch(l$lane$type, visits = 70, events = 30 + event_row_height * max(nrow(l$points), 4), series = 116, intervals = 110)
}

draw_lane <- function(l, x, top, h, last_contact) {
  switch(l$lane$type,
    visits = draw_visits(l$points, x, top, l$colour),
    events = draw_events(l$points, x, top, last_contact),
    series = draw_series(l, x, top, h),
    intervals = draw_intervals(l, x, top, h))
}

# The severities a subject's events have, as a key under the lane title.
severity_key_svg <- function(p, y) {
  lv <- unique(p$LEVEL[nzchar(p$LEVEL)])
  lv <- lv[order(match(severity_key(lv), names(severity_palette)))]
  rows <- y + (seq_along(lv) - 1) * 15
  sae_y <- y + length(lv) * 15
  c(sprintf('<rect x="0" y="%.1f" width="9" height="9" rx="2.5" fill="%s"/>', rows - 8, severity_colour(lv)),
    sv_text(14, rows, tools::toTitleCase(tolower(lv)), 11),
    if (any(p$SERIOUS)) c(sv_dot(4.5, sae_y - 3.5, 5.5, "none", paper[["muted"]]), sv_text(14, sae_y, "Serious (SAE)", 11)))
}

draw_visits <- function(p, x, top, colour = paper[["accent"]]) {
  y <- top + 34
  c(sv_line(x(min(p$DAY)), y, x(max(p$DAY)), y, tint(colour, 0.55), 2),
    sv_dot(x(p$DAY), y, 6.5, ifelse(p$MISSED, paper[["page"]], colour), ifelse(p$MISSED, paper[["red"]], paper[["page"]])),
    sv_text(x(p$DAY), y + 22, ifelse(p$MISSED, paste(p$LABEL, "missed"), p$LABEL), 11,
            ifelse(p$MISSED, paper[["red"]], paper[["muted"]]), "middle", ifelse(p$MISSED, 600, 400)))
}

draw_events <- function(p, x, top, last_contact) {
  unlist(lapply(seq_len(nrow(p)), function(i) {
    yy <- top + 22 + (i - 1) * event_row_height
    col <- level_colour(p$LEVEL[i])
    lab <- paste0(p$LABEL[i], if (nzchar(p$LEVEL[i])) paste0(" (", tolower(p$LEVEL[i]), ")"))
    bar <- p$ONGOING[i] && !is.na(last_contact) && last_contact > p$DAY[i]
    sae <- isTRUE(p$SERIOUS[i])
    width <- text_width(lab, 12) + if (sae) sae_width + 6 else 0
    # Label right of the event, unless an ongoing bar or the chart's edge is in the way.
    right <- !bar && x(p$DAY[i]) + 12 + width <= timeline_width
    lx <- if (right) x(p$DAY[i]) + 12 else x(p$DAY[i]) - 12
    ongoing <- "ongoing at last contact"
    ongoing_fits <- x(last_contact) + 8 + text_width(ongoing, 11) <= timeline_width
    c(if (bar) c(sv_rect(x(p$DAY[i]), yy - 3, x(last_contact) - x(p$DAY[i]), 6, tint(col, 0.6)),
                 if (ongoing_fits) sv_text(x(last_contact) + 8, yy + 4, ongoing, 11, paper[["muted"]])
                 else sv_text(x(last_contact), yy - 6, "ongoing", 10, paper[["muted"]], "end")),
      if (sae) sv_dot(x(p$DAY[i]), yy, 9.5, "none", col),
      sprintf('<rect x="%.1f" y="%.1f" width="12" height="12" rx="3.5" fill="%s" stroke="%s" stroke-width="2"/>',
              x(p$DAY[i]) - 6, yy - 6, col, paper[["page"]]),
      if (sae && !right) sv_pill(lx - sae_width, yy + 4, "SAE", col),
      sv_text(if (sae && !right) lx - sae_width - 6 else lx, yy + 4, lab, 12, paper[["text"]], if (right) "start" else "end"),
      if (sae && right) sv_pill(lx + text_width(lab, 12) + 6, yy + 4, "SAE", col))
  }))
}

ref_line <- function(l, x0, y, colour, unit = "") {
  c(sv_line(x0, y, timeline_width - 20, y, colour, 1, "4 3"),
    sv_text(timeline_width - 20, y - 5, paste0(trimws(paste(l$lane$ref_label %|na|% "", fmt_num(l$ref))), unit), 11,
            if (colour == paper[["line2"]]) paper[["muted"]] else colour, "end"))
}

draw_series <- function(l, x, top, h) {
  p <- l$points
  vals <- p$VALUE
  vals[p$BELOW & !is.na(l$ref)] <- l$ref
  ok <- !is.na(vals)
  if (!any(ok)) return(character())
  rng <- range(c(vals[ok], l$ref, 0), na.rm = TRUE)
  rng[2] <- rng[2] + diff(rng) * 0.15 + 1e-9
  ys <- function(v) top + h - 14 - (v - rng[1]) / diff(rng) * (h - 34)
  p <- p[ok, , drop = FALSE]
  vals <- vals[ok]
  line_pts <- !p$BELOW
  col <- l$colour %||% paper[["accent"]]
  lx <- x(p$DAY[line_pts])
  ly <- ys(vals[line_pts])
  c(if (sum(line_pts) > 1) sv_area(lx, ly, top + h - 14, sprintf("url(#%s)", l$gradient %||% "")),
    if (!is.na(l$ref)) ref_line(l, x(min(p$DAY)), ys(l$ref), paper[["line2"]]),
    if (sum(line_pts) > 1) sv_path(lx, ly, col, 2.5),
    sv_dot(x(p$DAY), ys(vals), ifelse(p$BELOW, 4, 4.5), ifelse(p$BELOW, paper[["page"]], col),
           ifelse(p$BELOW, paper[["muted"]], paper[["page"]])),
    sv_text(x(p$DAY), ys(vals) + ifelse(p$BELOW, 18, -9), ifelse(p$BELOW, "below limit", fmt_num(p$VALUE)), 11,
            ifelse(p$BELOW, paper[["muted"]], paper[["text"]]), "middle"))
}

draw_intervals <- function(l, x, top, h) {
  p <- l$points
  unit <- l$lane$unit %|na|% ""
  hi <- max(c(p$VALUE, l$ref, if (unit == "%") 100), na.rm = TRUE) * 1.12
  ys <- function(v) top + h - 12 - v / hi * (h - 26)
  low <- !is.na(l$ref) & p$VALUE < l$ref
  lane_col <- l$colour %||% paper[["accent"]]
  col <- ifelse(low, severity_palette[["MODERATE"]], lane_col)
  c(if (!is.na(l$ref)) ref_line(l, x(min(p$START)), ys(l$ref), paper[["amber"]], unit),
    sv_rect(x(p$START) + 2, ys(p$VALUE), x(p$END) - x(p$START) - 4, ys(0) - ys(p$VALUE),
            ifelse(low, tint(severity_palette[["MODERATE"]], 0.82), tint(lane_col, 0.84))),
    sv_line(x(p$START) + 2, ys(p$VALUE), x(p$END) - 2, ys(p$VALUE), col, 2.5),
    sv_text((x(p$START) + x(p$END)) / 2, ys(p$VALUE) - 6, paste0(fmt_num(p$VALUE), unit), 11,
            ifelse(low, paper[["amber"]], paper[["text"]]), "middle", 600))
}
