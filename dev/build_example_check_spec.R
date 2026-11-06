# Builds studies/example/check_spec.xlsx: the canned review checks plus the
# example study's own checks. Run from the repository root:
#   Rscript dev/build_example_check_spec.R
# After that the review team edits the workbook itself; this script only
# makes the starting point.

pkgload::load_all("core", quiet = TRUE)

on <- "2026-10-27"
canned <- canned_checks(changed_on = on)
# Subjects not yet dosed have visits planned from the planned Day 1.
visits <- canned$TYPE == "visit_status"
canned$PARAMS[visits] <- paste0(canned$PARAMS[visits], "; fallback=TRTSDT:PLANDY1")

study <- function(id, name, description, category, tier, type, form, field, when = NA, params = NA, message = NA,
                  severity = "warning", owner = "clinical") {
  data.frame(ID = id, NAME = name, DESCRIPTION = description, CATEGORY = category, TIER = tier, TYPE = type,
             FORM = form, FIELD = field, WHEN = when, PARAMS = params, MESSAGE = message, SEVERITY = severity,
             OWNER = owner, STATUS = "active", CHANGED_ON = on, CHANGED_BY = "study setup",
             REASON = "Study-specific check", stringsAsFactors = FALSE)
}
own <- rbind(
  study("CL-PK-001", "PK at or above alert level", "Systemic exposure at or above the study PK alert threshold",
        "Safety", "clinical", "record", "PK", "PCSTRESN", when = "PCSTRESN >= $pk_alert",
        message = "PK concentration at or above alert level", owner = "safety"),
  study("CL-PK-002", "PK above systemic exposure limit", "PK concentration at or above the study limit",
        "Safety", "clinical", "record", "PK", "PCSTRESN", when = "PCSTRESN >= $pk_limit",
        message = "PK concentration at or above the limit", severity = "error", owner = "safety"),
  study("CL-IGA-001", "IGA worsening", "Investigator Global Assessment shows worsening", "Efficacy", "clinical",
        "record", "IGA", "IGASCORE", when = "IGASCORE < 0", message = "IGA worse than baseline", severity = "info"),
  study("XF-IGA-001", "IGA entered for each post-baseline visit",
        "Visit done at a visit that schedules IGA, but no IGA record", "Data", "data", "reconcile", "SV", NA,
        when = "VISND is missing", params = "form_b=IGA; schedule_flag=IGAFL", owner = "data management"),
  study("RC-MCSS-001", "Central mCSS read received", "Visit done, but no central mCSS read from the imaging vendor yet",
        "Data", "data", "reconcile", "SV", NA, when = "VISND is missing", params = "form_b=MCSS",
        owner = "data management"),
  study("DM-DRY-001", "Diary missed doses in range", "Missed doses between 0 and doses prescribed", "Dosing", "data",
        "range", "DIARY", "DOSMISS", params = "min=0; max=DOSPRESC", severity = "error", owner = "sites"),
  study("XF-SV-001", "Post-baseline visit after first dose", "Post-baseline visit date on or before the first dose date",
        "Visits", "data", "date_order", "SV", "VISDAT", when = "VISITNUM > 2",
        params = "ref_form=EX; ref_field=EXSTDAT; op=after", owner = "sites"),
  study("RC-PK-001", "PK sample on the visit date", "PK collection date differs from the visit date, or no visit for the sample",
        "Data", "data", "reconcile", "PK", NA, params = "form_b=SV; date_a=PCDTC; date_b=VISDAT; tolerance=0",
        owner = "data management")
)
write_check_spec("studies/example/check_spec.xlsx", rbind(canned, own))
cat("Wrote studies/example/check_spec.xlsx:", nrow(canned) + nrow(own), "checks\n")
