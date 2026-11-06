# Subject timeline: every lane on one study-day scale (Day 1 = first dose, no
# day 0), drawn as plain SVG so it stays quiet and readable. Lanes come from
# the profile's `lanes` params, one of four types:
#   visits    {form, day, label, not_done: {field, value}}: visit dots; a visit
#             not done sits at its scheduled day as a hollow red dot.
#   events    {form, day, label, level, ongoing: {field, values}}: one row per
#             event, coloured by level (severe red, moderate amber), with a bar
#             to last contact while ongoing.
#   series    {form, day or date, value, below, ref, ref_label}: a line over
#             time; `below` marks values below the limit (drawn hollow on the
#             reference line).
#   intervals {form, start, end, value, ref, ref_label, unit}: a bar per
#             period, e.g. dosing compliance per diary interval.
# `ref` is a number, a study threshold name (study.yml thresholds) or a column
# whose first value is used (e.g. BASE). Every lane takes `title` and `note`.

paper <- c(page = "#faf7f2", line = "#e6dfd3", line2 = "#d6cdbd", text = "#2b2620", muted = "#635a4f",
           accent = "#0f5e57", accent_t = "#e3efe9", red = "#a61b1b", amber = "#8f5200", amber_t = "#f7e9cc")
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
                        ONGOING = if (is.null(og)) FALSE else as.character(t[[og$field]]) %in% unlist(og$values)),
    series = data.frame(DAY = lane_days(lane, t, trtsdt), VALUE = suppressWarnings(as.numeric(t[[lane$value]])),
                        BELOW = if (is.null(lane$below)) FALSE else as.logical(t[[lane$below]]) %in% TRUE),
    intervals = data.frame(START = study_day(t[[lane$start]], trtsdt), END = study_day(t[[lane$end]], trtsdt),
                           VALUE = suppressWarnings(as.numeric(t[[lane$value]]))))
  timed <- lane$type == "intervals"
  pts <- pts[if (timed) !is.na(pts$START) & !is.na(pts$END) & !is.na(pts$VALUE) else !is.na(pts$DAY), , drop = FALSE]
  c(out, list(points = pts[order(if (timed) pts$START else pts$DAY), , drop = FALSE]))
}

level_colour <- function(level) {
  ifelse(grepl("SEVERE|LIFE", level), paper[["red"]], ifelse(grepl("MODERATE", level), paper[["amber"]], paper[["accent"]]))
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
  sprintf('<path d="%s" fill="none" stroke="%s" stroke-width="%s" stroke-linejoin="round"/>',
          paste0(c("M", rep("L", length(x) - 1)), sprintf("%.1f,%.1f", x, y), collapse = " "), stroke, width)
}

# The whole timeline for a subject: `lanes` from lane_data(), the review date
# and last contact as study days, and `pulls` (subject_pulls()) drawn as faint
# vertical lines, those since the "changes since" date (`since_day`) in teal,
# numbered in a row along the bottom.
timeline_svg <- function(lanes, review_day, last_contact, pulls = NULL, since_day = NA) {
  lanes <- Filter(function(l) nrow(l$points) > 0, lanes)
  if (!length(lanes)) return(shiny::p(class = "note", "No dated records for this subject yet."))
  days <- unlist(lapply(lanes, function(l) c(l$points$DAY, l$points$START, l$points$END)))
  d0 <- min(c(days, 1), na.rm = TRUE) - 4
  d1 <- max(c(days, review_day), na.rm = TRUE) + 6
  x <- function(d) lane_label_width + (d - d0) / (d1 - d0) * (timeline_width - lane_label_width - 20)
  ticks <- pretty(c(d0, d1), 7)
  ticks <- ticks[ticks >= d0 & ticks <= d1]
  out <- sv_text(x(ticks), 20, ifelse(ticks == 0, "Day 1", paste("Day", ticks)), anchor = "middle")
  y <- 34
  for (l in lanes) {
    h <- lane_height(l)
    out <- c(out, sv_line(0, y, timeline_width, y),
             sv_text(0, y + 22, l$lane$title %|na|% l$lane$form, 13, paper[["text"]], weight = 600),
             if (!is.null(l$lane$note)) sv_text(0, y + 39, l$lane$note),
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
  shiny::HTML(sprintf('<svg class="tl" viewBox="0 0 %d %d" role="img" aria-label="Subject timeline by study day">%s</svg>',
                      timeline_width, round(y + 4), paste(out, collapse = "")))
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
  switch(l$lane$type, visits = 70, events = 24 + event_row_height * max(nrow(l$points), 2), series = 116, intervals = 110)
}

draw_lane <- function(l, x, top, h, last_contact) {
  switch(l$lane$type,
    visits = draw_visits(l$points, x, top),
    events = draw_events(l$points, x, top, last_contact),
    series = draw_series(l, x, top, h),
    intervals = draw_intervals(l, x, top, h))
}

draw_visits <- function(p, x, top) {
  y <- top + 34
  c(sv_line(x(min(p$DAY)), y, x(max(p$DAY)), y, paper[["line2"]]),
    sv_dot(x(p$DAY), y, 6, ifelse(p$MISSED, paper[["page"]], paper[["accent"]]), ifelse(p$MISSED, paper[["red"]], "none")),
    sv_text(x(p$DAY), y + 22, ifelse(p$MISSED, paste(p$LABEL, "missed"), p$LABEL), 11,
            ifelse(p$MISSED, paper[["red"]], paper[["muted"]]), "middle", ifelse(p$MISSED, 600, 400)))
}

draw_events <- function(p, x, top, last_contact) {
  unlist(lapply(seq_len(nrow(p)), function(i) {
    yy <- top + 22 + (i - 1) * event_row_height
    col <- level_colour(p$LEVEL[i])
    lab <- paste0(p$LABEL[i], if (nzchar(p$LEVEL[i])) paste0(" (", tolower(p$LEVEL[i]), ")"))
    bar <- p$ONGOING[i] && !is.na(last_contact) && last_contact > p$DAY[i]
    c(if (bar) c(sv_line(x(p$DAY[i]), yy, x(last_contact), yy, col, 3),
                 sv_text(x(last_contact) + 8, yy + 4, "ongoing at last contact", 11, col)),
      sv_rect(x(p$DAY[i]) - 5, yy - 5, 10, 10, col),
      if (bar) sv_text(x(p$DAY[i]) - 10, yy + 4, lab, 12, paper[["text"]], "end")
      else sv_text(x(p$DAY[i]) + 10, yy + 4, lab, 12, paper[["text"]]))
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
  c(if (!is.na(l$ref)) ref_line(l, x(min(p$DAY)), ys(l$ref), paper[["line2"]]),
    if (sum(line_pts) > 1) sv_path(x(p$DAY[line_pts]), ys(vals[line_pts])),
    sv_dot(x(p$DAY), ys(vals), ifelse(p$BELOW, 4, 3.5), ifelse(p$BELOW, paper[["page"]], paper[["accent"]]),
           ifelse(p$BELOW, paper[["muted"]], "none")),
    sv_text(x(p$DAY), ys(vals) + ifelse(p$BELOW, 18, -9), ifelse(p$BELOW, "below limit", fmt_num(p$VALUE)), 11,
            ifelse(p$BELOW, paper[["muted"]], paper[["text"]]), "middle"))
}

draw_intervals <- function(l, x, top, h) {
  p <- l$points
  unit <- l$lane$unit %|na|% ""
  hi <- max(c(p$VALUE, l$ref, if (unit == "%") 100), na.rm = TRUE) * 1.12
  ys <- function(v) top + h - 12 - v / hi * (h - 26)
  low <- !is.na(l$ref) & p$VALUE < l$ref
  col <- ifelse(low, paper[["amber"]], paper[["accent"]])
  c(if (!is.na(l$ref)) ref_line(l, x(min(p$START)), ys(l$ref), paper[["amber"]], unit),
    sv_rect(x(p$START) + 2, ys(p$VALUE), x(p$END) - x(p$START) - 4, ys(0) - ys(p$VALUE),
            ifelse(low, paper[["amber_t"]], paper[["accent_t"]])),
    sv_line(x(p$START) + 2, ys(p$VALUE), x(p$END) - 2, ys(p$VALUE), col, 2),
    sv_text((x(p$START) + x(p$END)) / 2, ys(p$VALUE) - 6, paste0(fmt_num(p$VALUE), unit), 11, col, "middle", 600))
}
