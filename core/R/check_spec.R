# Check spec: the review checks of a study, kept in an Excel workbook the
# review team edits (one row per check), separate from the mapping spec.
# Every study starts from the canned set in canned_checks(); the team adds
# rows, changes thresholds and conditions, and retires rows (STATUS retired)
# instead of deleting them, recording who changed what and why. Programmer
# checks (the structural checks in library_checks()) are not in the spec:
# they always run.

check_spec_cols <- c("ID", "NAME", "DESCRIPTION", "CATEGORY", "TIER", "TYPE", "FORM", "FIELD", "WHEN", "PARAMS",
                     "MESSAGE", "SEVERITY", "OWNER", "STATUS", "CHANGED_ON", "CHANGED_BY", "REASON")
check_spec_status <- c("active", "retired")
check_spec_tiers <- c("clinical", "data")

#' Read a check spec
#'
#' Reads the `Checks` sheet of a check spec workbook and turns each active
#' row into a check definition (see [define_checks()]). Rows with problems
#' are left out and reported, so one mistyped row never stops the review.
#'
#' Columns of the `Checks` sheet:
#' * `ID`, `NAME`, `DESCRIPTION`: what reviewers see. IDs starting `STR-`
#'   are reserved for programmer checks.
#' * `CATEGORY`: grouping for screens and the workbook, e.g. Safety, Dosing,
#'   Visits, Disposition, Efficacy, Data.
#' * `TIER`: `clinical` (medical review: clinical flags and the review
#'   queue) or `data` (data issues for data management and sites).
#' * `TYPE`: the check type (see [check_spec_types()]).
#' * `FORM`, `FIELD`: the form and field the check looks at.
#' * `WHEN`: conditions that must all hold, separated by `;`, e.g.
#'   `AESER == Y; AESEV in MODERATE, SEVERE`. Operators: `==`, `!=`, `<`,
#'   `<=`, `>`, `>=`, `in`, `not in`, `is missing`, `is present`. A value
#'   written `$name` is the study threshold `name` from study.yml.
#' * `PARAMS`: other settings of the type, `name=value; name=value`.
#' * `MESSAGE`: the finding's message.
#' * `SEVERITY` (`error`, `warning`, `info`), `OWNER` (`data management`,
#'   `clinical`, `safety`, `sites`).
#' * `STATUS`: `active` or `retired` (retired rows stay as history and do
#'   not run).
#' * `CHANGED_ON`, `CHANGED_BY`, `REASON`: the last change to the row.
#'
#' @param xlsx Path to the check spec workbook.
#' @return A list: `checks` (definitions of the active, valid rows),
#'   `table` (every row as read) and `problems` (data frame: `ROW`, `ID`,
#'   `PROBLEM`).
#' @export
read_check_spec <- function(xlsx) {
  if (!"Checks" %in% openxlsx::getSheetNames(xlsx)) stop("Check spec has no Checks sheet.", call. = FALSE)
  df <- openxlsx::read.xlsx(xlsx, sheet = "Checks", colNames = TRUE, skipEmptyRows = TRUE)
  if (is.null(df)) df <- as.data.frame(stats::setNames(rep(list(character()), length(check_spec_cols)), check_spec_cols))
  miss <- setdiff(check_spec_cols, names(df))
  if (length(miss)) stop("Checks sheet is missing column(s): ", paste(miss, collapse = ", "), ".", call. = FALSE)
  df <- df[check_spec_cols]
  for (col in names(df)) {
    v <- trimws(as.character(df[[col]]))
    v[!is.na(v) & v == ""] <- NA
    df[[col]] <- v
  }
  df$CHANGED_ON <- spec_date_text(df$CHANGED_ON)
  df$STATUS <- ifelse(is.na(df$STATUS), "active", tolower(df$STATUS))

  checks <- list()
  problems <- data.frame(ROW = integer(), ID = character(), PROBLEM = character(), stringsAsFactors = FALSE)
  report <- function(i, msg) {
    problems[nrow(problems) + 1, ] <<- list(i + 1L, if (is.na(df$ID[i])) "" else df$ID[i], msg)
  }
  for (i in seq_len(nrow(df))) {
    r <- df[i, , drop = FALSE]
    if (!r$STATUS %in% check_spec_status) {
      report(i, paste0("STATUS must be ", paste(check_spec_status, collapse = " or "), "."))
      next
    }
    if (r$STATUS == "retired") next
    if (!is.na(r$ID) && grepl("^STR-", r$ID)) {
      report(i, "IDs starting STR- are reserved for programmer checks.")
      next
    }
    if (!is.na(r$ID) && r$ID %in% names(checks)) {
      report(i, "ID is used by another active row.")
      next
    }
    if (!is.na(r$TIER) && !r$TIER %in% check_spec_tiers) {
      report(i, paste0("TIER must be ", paste(check_spec_tiers, collapse = " or "), "."))
      next
    }
    chk <- tryCatch(spec_row_check(r), error = function(e) {
      report(i, conditionMessage(e))
      NULL
    })
    if (is.null(chk)) next
    p <- check_problems(chk)
    if (length(p)) {
      report(i, paste(sub("^[^:]*: ", "", p), collapse = " "))
      next
    }
    checks[[chk$id]] <- chk
  }
  list(checks = unname(checks), table = df, problems = problems)
}

# One spec row to a check definition.
spec_row_check <- function(r) {
  na <- function(x) if (is.na(x)) NULL else x
  type <- na(r$TYPE)
  params <- lapply(parse_args(na(r$PARAMS)), spec_value)
  for (n in names(params)) {
    if (startsWith(n, "when") || n == "not_done") params[[n]] <- parse_conditions(as.character(params[[n]]))
  }
  if (!is.null(params$fallback)) params$fallback <- parse_pairs(params$fallback)
  form <- na(r$FORM)
  field <- na(r$FIELD)
  when <- if (is.na(r$WHEN)) NULL else parse_conditions(r$WHEN)
  if (identical(type, "reconcile")) {
    params$form_a <- form
    if (!is.null(when)) params$when_a <- when
  } else {
    if (!is.null(form)) params$form <- form
    if (!is.null(when)) params$when <- when
    if (!is.null(field)) {
      if (identical(type, "missing_value") || identical(type, "future_date")) params$fields <- split_list(field)
      else if (identical(type, "visit_status")) params$date <- field
      else params$field <- field
    }
  }
  if (!is.na(r$MESSAGE)) params$message <- r$MESSAGE
  type_def <- if (is.null(type)) NULL else get0(type, envir = check_type_registry, inherits = FALSE)
  list(id = na(r$ID), type = type, name = na(r$NAME), description = na(r$DESCRIPTION),
       category = if (is.na(r$CATEGORY)) NULL else r$CATEGORY, tier = na(r$TIER),
       timing = if (is.null(type_def)) "asof" else type_def$timing, severity = na(r$SEVERITY),
       owner = na(r$OWNER), params = params,
       changed = list(on = r$CHANGED_ON, by = r$CHANGED_BY, reason = r$REASON))
}

#' Parse check conditions
#'
#' @param s Conditions separated by `;`, e.g. `"AESER == Y; PCSTRESN >= $pk_alert"`.
#' @return A list of conditions (`field`, `op`, `value`) as check types take
#'   them in `when`.
#' @export
parse_conditions <- function(s) {
  if (is.null(s) || !length(s) || is.na(s) || !nzchar(trimws(s))) return(NULL)
  parts <- trimws(strsplit(s, ";", fixed = TRUE)[[1]])
  parts <- parts[nzchar(parts)]
  ops <- c("is missing" = "missing", "is present" = "present", "not in" = "not_in", "in" = "in", "==" = "==",
           "!=" = "!=", "<=" = "<=", ">=" = ">=", "<" = "<", ">" = ">")
  lapply(parts, function(p) {
    m <- regmatches(p, regexec("^(\\S+)\\s+(is missing|is present|not in|in|==|!=|<=|>=|<|>)\\s*(.*)$", p, perl = TRUE))[[1]]
    if (!length(m)) stop("Cannot read condition '", p, "': write FIELD OPERATOR VALUE, e.g. AESER == Y.", call. = FALSE)
    op <- unname(ops[m[3]])
    val <- trimws(m[4])
    if (op %in% c("missing", "present")) return(list(field = m[2], op = op))
    if (!nzchar(val)) stop("Condition '", p, "' has no value.", call. = FALSE)
    val <- gsub("^[\"']|[\"']$", "", if (op %in% c("in", "not_in")) trimws(strsplit(val, ",", fixed = TRUE)[[1]]) else val)
    list(field = m[2], op = op, value = val)
  })
}

# PARAMS values: a comma list becomes a vector, TRUE/FALSE become logical.
spec_value <- function(v) {
  if (toupper(v) %in% c("TRUE", "FALSE")) return(toupper(v) == "TRUE")
  if (grepl(",", v, fixed = TRUE) && !grepl("[<>=]", v)) return(trimws(strsplit(v, ",", fixed = TRUE)[[1]]))
  v
}

# "A:B, C:D" to list(A = "B", C = "D").
parse_pairs <- function(v) {
  kv <- strsplit(unlist(v), ":", fixed = TRUE)
  stats::setNames(lapply(kv, function(x) trimws(x[2])), vapply(kv, function(x) trimws(x[1]), ""))
}

spec_date_text <- function(x) {
  n <- suppressWarnings(as.numeric(x))
  out <- x
  i <- !is.na(n) & n > 20000 & n < 80000                    # Excel serial date
  out[i] <- format(as.Date(n[i], origin = "1899-12-30"))
  out
}

#' Check types a check spec can use
#'
#' The help sheet of a check spec: each type, what it flags, the columns it
#' reads and its settings.
#'
#' @return A data frame: `TYPE`, `FLAGS`, `FORM`, `FIELD`, `WHEN`, `PARAMS`, `EXAMPLE`.
#' @export
check_spec_types <- function() {
  data.frame(
    TYPE = c("record", "visit_status", "future_date", "range", "missing_value", "date_order", "reconcile"),
    FLAGS = c("Every record of the form that meets WHEN (every record when WHEN is blank)",
              "Scheduled visits with a given status on the review date",
              "Dates later than the data cut (the extract date), usually typing errors",
              "Values outside a range",
              "Fields left empty",
              "A date that is not after (or before) a reference date on another form",
              "A record with no matching record on another form, or dates that differ"),
    FORM = c("Form to flag", "Visit form", "Form; blank for every form", "Form", "Form", "Form", "First form (form_a)"),
    FIELD = c("Field shown as the finding's value", "Visit date field", "Date fields; blank for every date field",
              "Field to test", "Field(s), comma-separated", "Date field", ""),
    WHEN = c(rep("Optional conditions, e.g. AESER == Y", 2), "", rep("Optional conditions, e.g. AESER == Y", 4)),
    PARAMS = c("watch=fields whose change brings the flag back for review, e.g. watch=AESEV, AEOUT",
               "statuses=Missed, Overdue / not entered, Out of window; not_done=VISND == Yes; fallback=TRTSDT:PLANDY1",
               "exclude=fields to leave out",
               "min=value or field; max=value or field",
               "",
               "ref_form=EX; ref_field=EXSTDAT; op=after (after, on_or_after, before, on_or_before)",
               "form_b=MCSS; by=SUBJID, VISITNUM; date_a=; date_b=; tolerance=0; schedule_flag=; when_b=; both_ways=TRUE"),
    EXAMPLE = c("New SAE: FORM AE, FIELD AETERM, WHEN AESER == Y",
                "Missed visit: FORM SV, FIELD VISDAT, PARAMS statuses=Missed; not_done=VISND == Yes",
                "Date after the data cut: FORM and FIELD blank",
                "Missed doses in range: FORM DIARY, FIELD DOSMISS, PARAMS min=0; max=DOSPRESC",
                "AE not coded: FORM AE, FIELD AEDECOD",
                "Visit after first dose: FORM SV, FIELD VISDAT, WHEN VISITNUM > 2, PARAMS ref_form=EX; ref_field=EXSTDAT; op=after",
                "Central read received: FORM SV, WHEN VISND is missing, PARAMS form_b=MCSS"),
    stringsAsFactors = FALSE)
}

#' Canned review checks
#'
#' The standard review checks every study starts with. They use standard
#' collected variable names (AETERM, AESER, DSDECOD, VISDAT ...) and the
#' thresholds `compliance` from study.yml; a study changes FORM, FIELD and
#' conditions to fit its forms.
#'
#' @param changed_on Date recorded on each row.
#' @return A data frame in check spec layout.
#' @export
canned_checks <- function(changed_on = Sys.Date()) {
  row <- function(id, name, description, category, tier, type, form, field, when = NA, params = NA, message = NA,
                  severity = "warning", owner = "clinical") {
    data.frame(ID = id, NAME = name, DESCRIPTION = description, CATEGORY = category, TIER = tier, TYPE = type,
               FORM = form, FIELD = field, WHEN = when, PARAMS = params, MESSAGE = message, SEVERITY = severity,
               OWNER = owner, STATUS = "active", CHANGED_ON = format(as.Date(changed_on)), CHANGED_BY = "canned set",
               REASON = "Standard check", stringsAsFactors = FALSE)
  }
  rbind(
    row("AE-NEW", "New or changed AE", "Every adverse event; comes back for review when its term, dates, severity, seriousness, causality or outcome change",
        "Safety", "clinical", "record", "AE", "AETERM", params = "watch=AEDECOD, AESTDAT, AESEV, AESER, AEREL, AEOUT",
        message = "Adverse event", severity = "info", owner = "safety"),
    row("AE-SAE", "Serious AE", "Adverse event reported as serious", "Safety", "clinical", "record", "AE", "AETERM",
        when = "AESER == Y", params = "watch=AESEV, AEREL, AEOUT", message = "Serious adverse event", severity = "error",
        owner = "safety"),
    row("AE-SEV", "Severe AE", "Adverse event of severe intensity", "Safety", "clinical", "record", "AE", "AETERM",
        when = "AESEV == SEVERE", params = "watch=AESER, AEREL, AEOUT", message = "Severe adverse event", owner = "safety"),
    row("DT-FUT", "Date after the data cut", "A collected date later than the extract date, usually a typing error",
        "Data", "data", "future_date", NA, NA, owner = "data management"),
    row("AE-COD", "AE not coded", "Adverse event entered but not yet coded", "Data", "data", "missing_value", "AE",
        "AEDECOD", message = "Awaiting coding", severity = "info", owner = "data management"),
    row("DS-DISC", "Subject discontinued", "Disposition other than completed", "Disposition", "clinical", "record",
        "DS", "DSDECOD", when = "DSDECOD != COMPLETED", message = "Subject discontinued"),
    row("EX-COMP", "Low dosing compliance", "Dosing interval with compliance below the study threshold", "Dosing",
        "clinical", "record", "DIARY", "COMPPCT", when = "COMPPCT < $compliance", message = "Compliance below threshold"),
    row("VS-MISS", "Missed visit", "Scheduled visit recorded as not done", "Visits", "clinical", "visit_status", "SV",
        "VISDAT", params = "statuses=Missed; not_done=VISND == Yes"),
    row("VS-OVER", "Overdue visit", "Scheduled visit past its window and not entered", "Visits", "data",
        "visit_status", "SV", "VISDAT", params = "statuses=Overdue / not entered; not_done=VISND == Yes", owner = "sites"),
    row("VS-WIN", "Visit out of window", "Visit done outside its window", "Visits", "data", "visit_status", "SV",
        "VISDAT", params = "statuses=Out of window; not_done=VISND == Yes", severity = "info", owner = "sites")
  )
}

#' Write a check spec workbook
#'
#' Starts a study's check spec, by default from the canned set, with a help
#' sheet of check types.
#'
#' @param path Output path (.xlsx).
#' @param checks Rows in check spec layout; defaults to [canned_checks()].
#' @return `path`, invisibly.
#' @export
write_check_spec <- function(path, checks = canned_checks()) {
  checks <- checks[check_spec_cols]
  wb <- openxlsx::createWorkbook()
  hs <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#DDEBF7", wrapText = TRUE, valign = "top")
  openxlsx::addWorksheet(wb, "Checks")
  openxlsx::writeData(wb, "Checks", checks, headerStyle = hs, keepNA = FALSE)
  openxlsx::freezePane(wb, "Checks", firstActiveRow = 2, firstActiveCol = 3)
  openxlsx::addFilter(wb, "Checks", rows = 1, cols = seq_along(check_spec_cols))
  openxlsx::setColWidths(wb, "Checks", seq_along(check_spec_cols),
                         c(11, 26, 50, 12, 9, 13, 8, 11, 30, 50, 28, 9, 16, 9, 12, 14, 30))
  openxlsx::dataValidation(wb, "Checks", cols = match("STATUS", check_spec_cols), rows = 2:1000, type = "list",
                           value = '"active,retired"')
  openxlsx::dataValidation(wb, "Checks", cols = match("TIER", check_spec_cols), rows = 2:1000, type = "list",
                           value = '"clinical,data"')
  openxlsx::dataValidation(wb, "Checks", cols = match("SEVERITY", check_spec_cols), rows = 2:1000, type = "list",
                           value = '"error,warning,info"')
  openxlsx::dataValidation(wb, "Checks", cols = match("OWNER", check_spec_cols), rows = 2:1000, type = "list",
                           value = '"clinical,safety,data management,sites"')
  openxlsx::dataValidation(wb, "Checks", cols = match("TYPE", check_spec_cols), rows = 2:1000, type = "list",
                           value = paste0('"', paste(check_spec_types()$TYPE, collapse = ","), '"'))
  openxlsx::addWorksheet(wb, "Check types")
  openxlsx::writeData(wb, "Check types", check_spec_types(), headerStyle = hs)
  openxlsx::setColWidths(wb, "Check types", 1:7, c(14, 45, 18, 26, 30, 60, 60))
  openxlsx::addWorksheet(wb, "How to")
  openxlsx::writeData(wb, "How to", data.frame(Step = c(
    "Add a check: add a row with a new ID. Copy a similar row and change FORM, FIELD and WHEN.",
    "Change a check: edit the row and fill CHANGED_ON, CHANGED_BY and REASON.",
    "Remove a check: set STATUS to retired and give the REASON. Do not delete rows: they are the history.",
    "WHEN: conditions separated by ';', e.g. AESER == Y; AESEV in MODERATE, SEVERE. $name uses a study threshold.",
    "TIER clinical: medical review (clinical flags, review queue). TIER data: data issues for data management and sites.",
    "The app lists rows it cannot read on the Data load page; the other checks keep running.")),
    headerStyle = hs)
  openxlsx::setColWidths(wb, "How to", 1, 120)
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}
