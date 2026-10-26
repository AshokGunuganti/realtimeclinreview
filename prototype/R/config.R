# =============================================================================
#  Study and review configuration. Values here are referenced by rule ID in
#  specs/Review_Data_Spec.xlsx (sheet "Derivation Rules").
# =============================================================================

APP_CFG <- list(
  study_id          = "DERM-301-SIM",
  source_dir        = "data/source",
  schedule_file     = "config/visit_schedule.csv",
  default_asof      = as.Date("2026-10-22"),  # demo "review as of" date (production: extract date)
  default_lookback  = 14,     # days; default "show changes since" window
  runin_days        = 56,     # DR-003 planned Day 1 = enrollment + 56 days
  itt_min_age       = 6,      # DR-004 efficacy ITT = dosed and age >= 6
  lloq              = 0.1,    # ng/mL, DR-009
  pk_limit          = 2.0,    # ng/mL systemic exposure threshold, DR-009
  pk_alert          = 1.5,    # ng/mL default alert (user adjustable)
  comp_threshold    = 80,     # %, DR-008 default (user adjustable)
  lag_target        = 5,      # days, DR-007 entry lag target; used in DR-014
  min_n             = 3,      # DR-012 minimum n to display a visit mean
  overdue_flag_days = 14,     # DR-015
  query_old_days    = 30,     # DR-015
  related_terms     = c("POSSIBLY RELATED", "RELATED")   # DR-010
)

load_schedule <- function(path = APP_CFG$schedule_file) {
  s <- utils::read.csv(path, stringsAsFactors = FALSE)
  s[order(s$VISITNUM), ]
}

IGA_LABELS <- c("-3" = "Very much worse", "-2" = "Much worse", "-1" = "Minimally worse", "0" = "No change",
                "1" = "Minimally improved", "2" = "Much improved", "3" = "Very much improved")

VISIT_STATUS <- c("Complete", "Out of window", "Overdue / not entered", "Missed", "Not yet due", "Discontinued")
