# The assistant's tools: read-only views of the review data the session shows
# (review date and site applied), plus three that open screens. Each tool is a
# plain spec, list(name, description, arguments, fun), so tests call `fun`
# directly; assistant_client() turns them into ellmer tools. Results are capped
# at `row_cap` rows and never carry withheld free text (see assistant.R). Tool
# calls run outside any reactive context, so reads go through shiny::isolate().

system_fields <- c("STUDYID", "FORMOID", "RECID", "VISITNUM", "ENTRYDT", "UPDDT", "AVAILDT", "VERSION")
subject_form <- "SUBJECTS"   # the derived subject-level table (adsl), as the tools name it
max_group_fields <- 2

assistant_tools <- function(ctx, cfg) {
  data <- function() shiny::isolate(ctx$data())
  cap <- sprintf("Most rows to return (at most %d).", cfg$row_cap)
  specs <- list(
    tool_spec("study_overview",
      paste("The study, review date, 'changes since' date and site filter in use; subject counts by status and",
            "site; every form with its description, record and subject counts; and the app's screens."),
      list(), function() study_overview(ctx, data())),
    tool_spec("describe_form",
      paste("The fields of one form: variable, label, type, whether it is withheld free text, and the values",
            "of coded fields. Use before filtering on a form you have not used yet."),
      list(form = arg("string", "Form ID, e.g. AE, or SUBJECTS for subject-level dates and status.", TRUE)),
      function(form) describe_form(ctx$study, data(), form, cfg)),
    tool_spec("get_records",
      "Records of one form, optionally for one subject and filtered. Returns labelled rows, capped.",
      list(form = arg("string", "Form ID, or SUBJECTS.", TRUE),
           subject = arg("string", "Subject ID, e.g. 101-001."),
           filters = arg("array", paste("Conditions, all of which must hold, as 'FIELD OP VALUE'. OP is ==, !=, >, >=,",
                                        "<, <=, contains or in (comma-separated values). Dates as YYYY-MM-DD.",
                                        "Examples: 'AESEV == SEVERE', 'AGE >= 60', 'FOLDER in WK12,WK24'.")),
           fields = arg("array", "Fields to return; defaults to all the reviewer would see."),
           limit = arg("integer", cap)),
      function(form, subject = NULL, filters = NULL, fields = NULL, limit = NULL)
        get_records(ctx$study, data(), form, subject, filters, fields, limit, cfg)),
    tool_spec("count_records",
      "Counts of records and subjects in one form, grouped by up to two fields, after optional filters.",
      list(form = arg("string", "Form ID, or SUBJECTS.", TRUE),
           by = arg("array", "Fields to group by (at most two); none gives one total."),
           filters = arg("array", "Conditions as in get_records.")),
      function(form, by = NULL, filters = NULL) count_records(ctx$study, data(), form, by, filters, cfg)),
    tool_spec("subject_summary",
      "One subject: key dates and status, records per form, check findings and open queries.",
      list(subject = arg("string", "Subject ID.", TRUE)),
      function(subject) subject_summary(ctx, data(), subject, cfg)),
    tool_spec("list_checks",
      paste("The review checks the study runs: ID, name, description, area, severity, findings, findings new",
            "since the 'changes since' date, and the screen that reviews them."),
      list(), function() list_checks(ctx, data())),
    tool_spec("check_findings",
      "Findings of one review check, optionally for one subject; marks those new since the 'changes since' date.",
      list(check = arg("string", "Check ID from list_checks.", TRUE), subject = arg("string", "Subject ID."),
           limit = arg("integer", cap)),
      function(check, subject = NULL, limit = NULL) check_findings(ctx, data(), check, subject, limit, cfg)),
    tool_spec("new_and_updated",
      paste("Records new, updated or deleted between the 'changes since' date and the review date, on clinical",
            "forms. Without a form: counts per form. With a form: the records and which fields changed."),
      list(form = arg("string", "Form ID for record detail."), limit = arg("integer", cap)),
      function(form = NULL, limit = NULL) new_and_updated(ctx, form, limit, cfg)),
    tool_spec("list_queries",
      "Data queries with their status (Open, Answered, Closed), form, visit and age in days.",
      list(subject = arg("string", "Subject ID."), status = arg("string", "Open, Answered or Closed."),
           limit = arg("integer", cap)),
      function(subject = NULL, status = NULL, limit = NULL) list_queries(ctx, subject, status, limit, cfg)),
    tool_spec("open_screen", "Opens a screen of the review app for the reviewer.",
      list(screen = arg("string", "Screen ID from study_overview.", TRUE)),
      function(screen) open_screen(ctx, screen), navigates = TRUE),
    tool_spec("open_check", "Opens the screen that reviews a check, with that check picked.",
      list(check = arg("string", "Check ID.", TRUE)), function(check) open_check(ctx, data(), check), navigates = TRUE),
    tool_spec("open_subject_profile", "Opens a subject's profile for the reviewer.",
      list(subject = arg("string", "Subject ID.", TRUE)),
      function(subject) open_subject(ctx, data(), subject), navigates = TRUE)
  )
  lapply(specs, logged_tool, ctx = ctx)
}

tool_spec <- function(name, description, arguments, fun, navigates = FALSE) {
  list(name = name, description = description, arguments = arguments, fun = fun, navigates = navigates)
}

arg <- function(type, desc, required = FALSE) list(type = type, desc = desc, required = required)

# Every tool call goes to the access log with the arguments given. The wrapper
# keeps the tool's own argument names, which ellmer matches to the spec.
logged_tool <- function(spec, ctx) {
  fun <- spec$fun
  args <- as.character(names(formals(fun)))
  wrapper <- function() NULL
  formals(wrapper) <- formals(fun)
  body(wrapper) <- substitute({
    given <- Filter(Negate(is.null), mget(ARGS))
    rtcrcore::log_access(ctx$con, ctx$study$id, ctx$user, "assistant", NAME,
                         if (length(given)) as.character(jsonlite::toJSON(given, auto_unbox = TRUE)) else NA_character_)
    CALL
  }, list(ARGS = args, NAME = spec$name, CALL = as.call(c(list(fun), lapply(args, as.name)))))
  spec$fun <- wrapper
  spec
}

# --- data views ---------------------------------------------------------------

form_table <- function(data, form) {
  form <- toupper(form %||% "")
  t <- if (form == subject_form) data$adsl else data$tables[[form]]
  if (is.null(t)) stop("No form '", form, "'. study_overview lists the forms.", call. = FALSE)
  if (form == subject_form) t <- t[, !grepl("SRC$", names(t)), drop = FALSE]
  t
}

shown_fields <- function(form, t, cfg) {
  f <- setdiff(names(t), system_fields)
  f[!is_free_text(toupper(form), f, cfg)]
}

# Rows as the model sees them: capped, dates as text.
capped <- function(d, limit, cfg, labels = NULL) {
  n <- suppressWarnings(min(as.integer(limit %||% cfg$row_cap), cfg$row_cap))
  if (is.na(n) || n < 1) n <- cfg$row_cap
  rows <- utils::head(d, n)
  rows[] <- lapply(rows, function(x) if (inherits(x, c("Date", "POSIXt"))) format(x) else x)
  out <- list(total_matches = nrow(d), returned = nrow(rows), rows = rows)
  if (!is.null(labels)) out$labels <- labels
  out
}

field_labels <- function(fields, study) as.list(stats::setNames(vapply(fields, col_label, "", study = study), fields))

filter_rows <- function(t, form, filters, cfg) {
  keep <- rep(TRUE, nrow(t))
  for (f in as.character(unlist(filters))) {
    m <- regmatches(f, regexec("^\\s*([A-Za-z0-9_]+)\\s*(==|!=|>=|<=|>|<|contains|in)\\s*(.*?)\\s*$", f))[[1]]
    if (!length(m)) stop("Filter '", f, "' is not 'FIELD OP VALUE'.", call. = FALSE)
    field <- toupper(m[2])
    if (!field %in% names(t)) stop("Form ", toupper(form), " has no field ", field, ".", call. = FALSE)
    if (is_free_text(toupper(form), field, cfg)) stop(field, " is withheld free text and cannot be filtered on.", call. = FALSE)
    keep <- keep & compare(t[[field]], m[3], gsub("^['\"]|['\"]$", "", m[4]))
  }
  t[keep, , drop = FALSE]
}

# One filter condition; text compares ignoring case, and NA never matches.
compare <- function(x, op, value) {
  conv <- function(v) if (inherits(x, "Date")) as.Date(v) else if (inherits(x, "POSIXt")) as.POSIXct(v, tz = "UTC")
                      else if (is.numeric(x)) suppressWarnings(as.numeric(v)) else toupper(v)
  xx <- if (is.character(x) || is.factor(x)) toupper(as.character(x)) else x
  out <- switch(op,
    "==" = xx == conv(value), "!=" = xx != conv(value), ">" = xx > conv(value), ">=" = xx >= conv(value),
    "<" = xx < conv(value), "<=" = xx <= conv(value),
    contains = grepl(toupper(value), toupper(as.character(x)), fixed = TRUE),
    "in" = xx %in% conv(trimws(strsplit(value, ",", fixed = TRUE)[[1]])))
  !is.na(out) & out
}

study_overview <- function(ctx, data) {
  study <- ctx$study
  forms <- names(data$tables)
  list(
    study = study$id, simulated = isTRUE(study$config$app$simulated),
    review_date = format(shiny::isolate(ctx$asof())), changes_since = format(shiny::isolate(ctx$since())),
    site = shiny::isolate(ctx$site()), subjects = nrow(data$adsl),
    subjects_by_status = as.list(table(data$adsl$EOSSTT)), subjects_by_site = as.list(table(data$adsl$SITEID)),
    forms = data.frame(
      form = c(subject_form, forms),
      description = c("Subject-level dates and status (derived)",
                      vapply(forms, function(f) study$mapping$forms[[f]]$description %||% f, "")),
      kind = c("derived", vapply(forms, function(f) study$mapping$forms[[f]]$type %||% "", "")),
      records = c(nrow(data$adsl), vapply(data$tables, nrow, 0L)),
      subjects = c(nrow(data$adsl), vapply(data$tables, function(t) length(unique(t$SUBJID)), 0L)),
      row.names = NULL),
    screens = data.frame(screen = names(ctx$tab_titles), title = unlist(ctx$tab_titles), row.names = NULL))
}

describe_form <- function(study, data, form, cfg) {
  t <- form_table(data, form)
  f <- setdiff(names(t), system_fields)
  withheld <- is_free_text(toupper(form), f, cfg)
  values <- vapply(seq_along(f), function(i) {
    v <- unique(stats::na.omit(as.character(t[[f[i]]])))
    coded <- is.character(t[[f[i]]]) && length(v) <= 12 && !f[i] %in% c("SUBJID", "SITEID")
    if (withheld[i] || !coded) "" else paste(sort(v), collapse = ", ")
  }, "")
  list(form = toupper(form), records = nrow(t), fields = data.frame(
    field = f, label = vapply(f, col_label, "", study = study),
    type = vapply(f, function(x) class(t[[x]])[1], ""), withheld_free_text = withheld, values = values,
    row.names = NULL))
}

get_records <- function(study, data, form, subject, filters, fields, limit, cfg) {
  t <- form_table(data, form)
  if (!is.null(subject)) t <- t[t$SUBJID %in% subject, , drop = FALSE]
  t <- filter_rows(t, form, filters, cfg)
  ok <- shown_fields(form, t, cfg)
  want <- toupper(as.character(unlist(fields)))
  keep <- if (length(want)) intersect(unique(c("SUBJID", want)), ok) else ok
  capped(t[, keep, drop = FALSE], limit, cfg, field_labels(keep, study))
}

count_records <- function(study, data, form, by, filters, cfg) {
  t <- filter_rows(form_table(data, form), form, filters, cfg)
  by <- toupper(as.character(unlist(by)))
  if (length(by) > max_group_fields) stop("Group by at most ", max_group_fields, " fields.", call. = FALSE)
  bad <- setdiff(by, names(t))
  if (length(bad)) stop("Form ", toupper(form), " has no field ", bad[1], ".", call. = FALSE)
  if (any(is_free_text(toupper(form), by, cfg))) stop("Free text is withheld and cannot be grouped on.", call. = FALSE)
  if (!length(by)) return(list(records = nrow(t), subjects = length(unique(t$SUBJID))))
  key <- do.call(paste, c(lapply(by, function(b) ifelse(is.na(t[[b]]), "(missing)", as.character(t[[b]]))), sep = "\r"))
  groups <- split(seq_len(nrow(t)), key)
  out <- do.call(rbind, lapply(names(groups), function(k) {
    i <- groups[[k]]
    cbind(as.data.frame(stats::setNames(as.list(strsplit(k, "\r", fixed = TRUE)[[1]]), by)),
          records = length(i), subjects = length(unique(t$SUBJID[i])))
  }))
  out <- out[order(-out$records), , drop = FALSE]
  rownames(out) <- NULL
  capped(out, cfg$row_cap, cfg, field_labels(by, study))
}

review_check_ids <- function(data) data$summary$ID[data$summary$TIER != "structural"]

subject_summary <- function(ctx, data, subject, cfg) {
  a <- form_table(data, subject_form)
  a <- a[a$SUBJID %in% subject, , drop = FALSE]
  if (!nrow(a)) stop("No subject ", subject, " at this site and review date.", call. = FALSE)
  n <- vapply(data$tables, function(t) sum(t$SUBJID %in% subject), 0L)
  f <- data$findings[data$findings$SUBJID %in% subject & data$findings$CHECKID %in% review_check_ids(data), , drop = FALSE]
  q <- shiny::isolate(ctx$queries())
  list(subject = capped(a, 1, cfg, field_labels(names(a), ctx$study)),
       records_per_form = as.list(n[n > 0]),
       findings_by_check = if (nrow(f)) as.list(table(f$CHECKID)) else list(),
       open_queries = if (is.null(q)) NA else sum(q$SUBJID %in% subject & q$STATUS != "Closed"))
}

list_checks <- function(ctx, data) {
  cat <- check_catalogue(ctx$study, data, shiny::isolate(new_finding_keys(ctx)), ctx$check_tabs, ctx$tab_titles)
  cat <- cat[cat$Status == "active", c("ID", "Name", "Description", "Category", "Severity", "Findings", "New", "Tab"),
             drop = FALSE]
  rownames(cat) <- NULL
  list(active_checks = nrow(cat), checks_with_findings = sum(cat$Findings > 0), checks_with_new = sum(cat$New > 0),
       checks = cat[order(-cat$New, -cat$Findings), , drop = FALSE])
}

check_findings <- function(ctx, data, check, subject, limit, cfg) {
  if (!check %in% review_check_ids(data)) stop("No review check ", check, ". list_checks lists them.", call. = FALSE)
  f <- data$findings[data$findings$CHECKID == check, , drop = FALSE]
  if (!is.null(subject)) f <- f[f$SUBJID %in% subject, , drop = FALSE]
  hide <- is_free_text(f$FORMOID, ifelse(is.na(f$FIELD), "", f$FIELD), cfg)
  message <- vapply(seq_len(nrow(f)), function(i) {
    v <- f$VALUE[i]
    if (hide[i] && !is.na(v) && nzchar(v)) gsub(v, "[withheld]", f$MESSAGE[i], fixed = TRUE) else f$MESSAGE[i]
  }, "")
  out <- data.frame(SUBJID = f$SUBJID, SITEID = f$SITEID, FORM = f$FORMOID,
                    FIELD = vapply(f$FIELD, function(x) if (is.na(x)) "" else col_label(x, ctx$study), "", USE.NAMES = FALSE),
                    VALUE = ifelse(hide, "[withheld free text]", f$VALUE), MESSAGE = message,
                    NEW = f$FINDINGKEY %in% shiny::isolate(new_finding_keys(ctx)), row.names = NULL)
  capped(out[order(!out$NEW), , drop = FALSE], limit, cfg)
}

new_and_updated <- function(ctx, form, limit, cfg) {
  ch <- shiny::isolate(ctx$changes())
  ch <- ch[ch$FORMOID %in% clinical_forms(ctx$study), , drop = FALSE]
  ch$RECSTAT <- status_label(ch$RECSTAT)
  window <- list(changes_since = format(shiny::isolate(ctx$since())), review_date = format(shiny::isolate(ctx$asof())))
  if (is.null(form)) {
    counts <- as.data.frame.matrix(table(ch$FORMOID, factor(ch$RECSTAT, c("New", "Updated", "Deleted"))))
    return(c(window, list(by_form = cbind(form = rownames(counts), counts, row.names = NULL))))
  }
  ch <- ch[ch$FORMOID == toupper(form), , drop = FALSE]
  fields <- vapply(strsplit(ch$CHGVARS, "[ ,;]+"), function(v) {
    paste(vapply(v[nzchar(v)], col_label, "", study = ctx$study), collapse = ", ")
  }, "")
  out <- data.frame(SUBJID = ch$SUBJID, SITEID = ch$SITEID, RECORD = ch$RECID, STATUS = ch$RECSTAT,
                    CHANGED_FIELDS = fields, row.names = NULL)
  c(window, capped(out, limit, cfg))
}

list_queries <- function(ctx, subject, status, limit, cfg) {
  q <- shiny::isolate(ctx$queries())
  if (is.null(q)) stop("This study has no query report.", call. = FALSE)
  if (!is.null(subject)) q <- q[q$SUBJID %in% subject, , drop = FALSE]
  if (!is.null(status)) q <- q[toupper(q$STATUS) == toupper(status), , drop = FALSE]
  keep <- intersect(c("SUBJID", "SITEID", "QRYFORM", "FOLDER", "STATUS", "QRYOPDT", "QRYANSDT", "QRYCLSDT", "AGE",
                      "DAYSTOCLOSE"), names(q))
  capped(q[order(q$STATUS == "Closed", -ifelse(is.na(q$AGE), 0, q$AGE)), keep, drop = FALSE], limit, cfg)
}

# --- navigation ---------------------------------------------------------------

open_screen <- function(ctx, screen) {
  if (!screen %in% names(ctx$tab_titles)) stop("No screen '", screen, "'. study_overview lists them.", call. = FALSE)
  ctx$goto_tab(screen)
  list(opened = ctx$tab_titles[[screen]])
}

open_check <- function(ctx, data, check) {
  if (!check %in% review_check_ids(data)) stop("No review check ", check, ".", call. = FALSE)
  ctx$goto_check(check)
  tab <- ctx$check_tabs[check]
  list(opened = if (is.na(tab)) "Review summary" else ctx$tab_titles[[tab]], check = check)
}

open_subject <- function(ctx, data, subject) {
  if (!subject %in% data$adsl$SUBJID) stop("No subject ", subject, " at this site and review date.", call. = FALSE)
  ctx$open_profile(subject)
  list(opened = paste("Subject profile for", subject))
}
