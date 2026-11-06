# Chart and accent colours, one source for every screen. Colour does one job at
# a time:
#   series    identity: each profile lane (and any other set of series) takes
#             the next slot in this fixed order; colour follows the series,
#             never its rank. Validated with the dataviz palette checks on the
#             snow surface #f8fafb (lightness, chroma, colour-blind and normal
#             separation of neighbours; aqua and pink sit under 3:1, so their
#             marks always carry visible labels).
#   severity  AE severity, warm and darkening light to dark so the order reads
#             in greyscale too; every mark also says its severity in words.
#   areas     an accent per check area and sidebar group, shared by name so
#             Safety is the same colour wherever it shows.
# Warnings (low compliance, missed visits) keep the status colours in paper.

series_palette <- c(blue = "#2a78d6", aqua = "#1baf7a", violet = "#6250d6", pink = "#e87ba4", green = "#008300",
                    magenta = "#d55181")

severity_palette <- c(MILD = "#eda100", MODERATE = "#eb6834", SEVERE = "#c92a2a", "LIFE THREATENING" = "#7a1f3d",
                      FATAL = "#7a1f3d")

area_palette <- c("New and updated data" = "#0b7285", "Safety" = "#d55181", "Efficacy" = "#008300",
                  "Dosing" = "#1baf7a", "Visits" = "#2a78d6", "Study conduct" = "#2a78d6",
                  "Disposition" = "#e87ba4", "Operations" = "#e87ba4", "Data" = "#6250d6",
                  "Data quality" = "#6250d6", "Close-out" = "#5f6b7a")
neutral_accent <- "#5f6b7a"

# Slot `i` of the series palette; past the last slot, neutral (fold into "Other").
series_colour <- function(i) {
  ok <- i >= 1 & i <= length(series_palette)
  ifelse(ok, unname(series_palette[pmax(1, pmin(i, length(series_palette)))]), neutral_accent)
}

area_colour <- function(area) {
  out <- unname(area_palette[area])
  ifelse(is.na(out), neutral_accent, out)
}

severity_key <- function(level) {
  l <- toupper(gsub("[-_]", " ", as.character(level)))
  grades <- c("1" = "MILD", "2" = "MODERATE", "3" = "SEVERE", "4" = "LIFE THREATENING", "5" = "FATAL")
  g <- sub("^GRADE ", "", l)
  ifelse(g %in% names(grades), grades[g], l)
}

# Severity colour for any spelling (Mild, MODERATE, "Life-threatening", grade 1-5).
severity_colour <- function(level) {
  out <- unname(severity_palette[severity_key(level)])
  ifelse(is.na(out), neutral_accent, out)
}

is_severity <- function(values) {
  v <- unique(stats::na.omit(severity_key(values)))
  v <- setdiff(v, "MISSING")
  length(v) > 0 && all(v %in% names(severity_palette))
}

# Colours for a set of category levels: the severity scale when they are
# severities (in severity order), else series slots in sorted level order so a
# level keeps its colour when a filter drops others.
category_colours <- function(levels) {
  levels <- sort(unique(as.character(levels)))
  if (is_severity(levels)) levels <- levels[order(match(severity_key(levels), names(severity_palette)))]
  cols <- if (is_severity(levels)) severity_colour(levels) else series_colour(seq_along(levels))
  stats::setNames(ifelse(levels == "Missing", "#9aa5b1", cols), levels)
}

# A form's colour across screens: the colour of its lane in the subject
# profile, so a measure looks the same on its own chart and on the profile.
form_colour <- function(study, form, fallback = "#0b7285") {
  profile <- Filter(function(d) identical(d$module, "subject_profile"), study$config$displays)
  lanes <- if (length(profile)) profile[[1]]$params$lanes else list()
  if (!length(lanes)) return(fallback)
  cols <- lane_colours(lapply(lanes, function(l) list(lane = l)))
  hit <- which(vapply(lanes, function(l) identical(l$form, form) && !identical(l$type, "events"), TRUE))
  if (length(hit)) cols[hit[1]] else fallback
}

# A colour with transparency, for plotly fills and faint lines.
rgba <- function(colour, alpha) {
  rgb <- grDevices::col2rgb(colour)
  sprintf("rgba(%d,%d,%d,%s)", rgb[1, ], rgb[2, ], rgb[3, ], alpha)
}

# The accent as a CSS variable for a group heading's stripe and count pill.
area_style <- function(area) sprintf("--area:%s", area_colour(area))

# A colour mixed toward white, for fills behind a line or under a label.
tint <- function(colour, amount = 0.85) {
  rgb <- grDevices::col2rgb(colour)
  mixed <- round(rgb + (255 - rgb) * amount)
  grDevices::rgb(mixed[1, ], mixed[2, ], mixed[3, ], maxColorValue = 255)
}
