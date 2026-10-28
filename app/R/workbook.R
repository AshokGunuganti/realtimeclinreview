# Review workbook: one Excel file for the whole review on a date. A Contents
# sheet lists every check with a link to its own sheet of findings; a Patient
# profile sheet holds every subject's data in long form (one row per field),
# so filtering on Subject gives that subject's full profile.

#' Review workbook
#'
#' Writes the review data for one review date to an Excel workbook:
#'
#' * **Contents**: every review check with its category, result and finding
#'   count; the check ID links to the check's sheet. Programmer checks on
#'   loading and mapping (structural tier) are left out: they are for
#'   programmers, on the app's Data load page.
#' * **One sheet per check**: what the check does, then its findings with
#'   review status, filterable. Checks with no findings get a sheet too, so
#'   the workbook shows what was checked as well as what was found.
#' * **One sheet per monitored form** (with `since`): every record on the
#'   review date plus records deleted since `since`, coloured by status as in
#'   the 2017 workbook (new, updated, deleted), changed values highlighted and
#'   listed as earlier -> new value.
#' * **Patient profile**: subject-level data (ADSL) and every record of every
#'   clinical form, one row per field, with record status and review marks;
#'   derived fields are labelled.
#' * **About**: study, review date, site, user and time of export.
#'
#' @param rd Review data from [rtcrcore::review_data()], filtered to the site.
#' @param study The loaded study.
#' @param file Output path.
#' @param about Data frame (`Item`, `Value`) for the About sheet.
#' @param con Review store, for review status; `NULL` leaves it out.
#' @param since Earlier date for new and updated records; `NULL` leaves the
#'   monitored data sheets out.
#' @return `file`, invisibly.
#' @export
review_workbook <- function(rd, study, file, about, con = NULL, since = NULL) {
  wb <- openxlsx::createWorkbook()
  hs <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#DDEBF7", wrapText = TRUE, valign = "top")
  title <- openxlsx::createStyle(textDecoration = "bold", fontSize = 14)
  label <- openxlsx::createStyle(textDecoration = "bold")
  link <- openxlsx::createStyle(fontColour = "#2A78D6", textDecoration = "underline")
  table <- function(sheet, d, row, widths = NULL) {
    openxlsx::writeData(wb, sheet, d, startRow = row, headerStyle = hs)
    if (ncol(d)) openxlsx::addFilter(wb, sheet, rows = row, cols = seq_len(ncol(d)))
    openxlsx::freezePane(wb, sheet, firstActiveRow = row + 1)
    openxlsx::setColWidths(wb, sheet, seq_len(ncol(d)), if (is.null(widths)) "auto" else widths)
  }

  s <- rd$summary
  s <- s[s$TIER != "structural", , drop = FALSE]
  if (is.null(s$CATEGORY)) s$CATEGORY <- s$TIER
  forms <- if (is.null(since)) character() else intersect(monitored_forms(study), names(rd$tables))
  all_sheets <- check_sheet_names(c(s$ID, paste("Data", forms)))
  sheets <- all_sheets[seq_len(nrow(s))]
  data_sheets <- all_sheets[nrow(s) + seq_along(forms)]
  checks <- stats::setNames(study$checks, vapply(study$checks, function(ch) ch$id, ""))

  # Contents
  openxlsx::addWorksheet(wb, "Contents")
  openxlsx::writeData(wb, "Contents", paste(study$id, "review workbook"), startRow = 1)
  openxlsx::addStyle(wb, "Contents", title, rows = 1, cols = 1)
  site <- about$Value[about$Item == "Site"]
  site <- if (!length(site) || identical(site, "All sites")) "all sites" else paste("site", site)
  openxlsx::writeData(wb, "Contents", sprintf("Review date %s, %s. %d checks, %d %s. Click a check ID to open its sheet.",
                                              format(rd$asof), site, nrow(s), sum(s$N),
                                              if (sum(s$N) == 1) "finding" else "findings"),
                      startRow = 2)
  toc <- data.frame(`Check ID` = s$ID, Name = s$NAME, Category = s$CATEGORY, Severity = s$SEVERITY, Owner = s$OWNER,
                    Findings = s$N, Result = s$RESULT, check.names = FALSE, stringsAsFactors = FALSE)
  table("Contents", toc, 4, widths = c(14, 40, 14, 10, 18, 10, 10))
  if (nrow(s)) {
    openxlsx::writeFormula(wb, "Contents", startCol = 1, startRow = 5,
                           x = openxlsx::makeHyperlinkString(sheet = sheets, row = 1, col = 1, text = s$ID))
    openxlsx::addStyle(wb, "Contents", link, rows = 4 + seq_len(nrow(s)), cols = 1)
  }
  r <- 6 + nrow(s)
  fcs <- lapply(stats::setNames(forms, forms), function(fm) form_changes(study, rd, fm, since, rd$asof))
  if (length(forms)) {
    openxlsx::writeData(wb, "Contents", sprintf("Monitored data: new and updated records since %s", format(as.Date(since))),
                        startRow = r)
    openxlsx::addStyle(wb, "Contents", label, rows = r, cols = 1)
    legend <- c(New = change_colours[["New"]], Updated = change_colours[["Changed"]],
                `Changed value` = changed_cell_colour, Deleted = change_colours[["Deleted"]])
    openxlsx::writeData(wb, "Contents", t(names(legend)), startRow = r + 1, colNames = FALSE)
    for (i in seq_along(legend)) {
      openxlsx::addStyle(wb, "Contents", openxlsx::createStyle(fgFill = legend[[i]]), rows = r + 1, cols = i)
    }
    dt <- data.frame(Sheet = data_sheets, Form = forms,
                     Records = vapply(fcs, function(x) sum(x$status != "Deleted"), 0L),
                     New = vapply(fcs, function(x) sum(x$status == "New"), 0L),
                     Updated = vapply(fcs, function(x) sum(x$status == "Changed"), 0L),
                     Deleted = vapply(fcs, function(x) sum(x$status == "Deleted"), 0L),
                     check.names = FALSE, stringsAsFactors = FALSE)
    openxlsx::writeData(wb, "Contents", dt, startRow = r + 3, headerStyle = hs)
    openxlsx::writeFormula(wb, "Contents", startCol = 1, startRow = r + 4,
                           x = openxlsx::makeHyperlinkString(sheet = data_sheets, row = 1, col = 1, text = data_sheets))
    openxlsx::addStyle(wb, "Contents", link, rows = r + 3 + seq_along(forms), cols = 1)
    r <- r + 5 + length(forms)
  }
  openxlsx::writeFormula(wb, "Contents", startCol = 1, startRow = r,
                         x = openxlsx::makeHyperlinkString(sheet = "Patient profile", row = 1, col = 1,
                                                           text = "Patient profile"))
  openxlsx::addStyle(wb, "Contents", link, rows = r, cols = 1)
  openxlsx::writeData(wb, "Contents", "Every subject's data, one row per field: filter on Subject.",
                      startCol = 2, startRow = r)

  # One sheet per check
  f <- rd$findings
  if (!is.null(con) && nrow(f)) {
    m <- rtcrcore::apply_marks(con, study$id, data.frame(FINDINGKEY = f$FINDINGKEY, DEPHASH = f$DEPHASH), "finding")
    f$REVIEW <- m$REVIEW_STATUS
    f$MARKED <- mark_text(m)
  }
  for (i in seq_len(nrow(s))) {
    sh <- sheets[i]
    ch <- checks[[s$ID[i]]]
    openxlsx::addWorksheet(wb, sh)
    openxlsx::writeData(wb, sh, paste(s$ID[i], s$NAME[i], sep = ": "), startRow = 1)
    openxlsx::addStyle(wb, sh, title, rows = 1, cols = 1)
    chg <- ch$changed
    last <- if (is.null(chg) || is.na(chg$on)) "" else paste(c(chg$on, chg$by, chg$reason)[!is.na(c(chg$on, chg$by, chg$reason))],
                                                              collapse = " \u00b7 ")
    info <- data.frame(Item = c("Description", "Category", "Severity", "Owner", "Result", "Findings", "Last changed"),
                       Value = c(if (is.null(ch$description)) "" else ch$description, s$CATEGORY[i], s$SEVERITY[i],
                                 s$OWNER[i], s$RESULT[i], s$N[i], last), stringsAsFactors = FALSE)
    openxlsx::writeData(wb, sh, info, startRow = 2, colNames = FALSE)
    openxlsx::addStyle(wb, sh, label, rows = 2:8, cols = 1)
    openxlsx::writeFormula(wb, sh, startCol = 1, startRow = 9,
                           x = openxlsx::makeHyperlinkString(sheet = "Contents", row = 1, col = 1, text = "Back to contents"))
    openxlsx::addStyle(wb, sh, link, rows = 9, cols = 1)
    x <- f[f$CHECKID == s$ID[i], , drop = FALSE]
    if (!nrow(x)) {
      openxlsx::writeData(wb, sh, "No findings on this review date.", startRow = 11)
      openxlsx::setColWidths(wb, sh, 1:2, c(14, 60))
      next
    }
    d <- data.frame(Subject = x$SUBJID, Site = x$SITEID, Form = x$FORMOID, Record = x$RECID, Where = x$ANCHOR,
                    Field = x$FIELD, Value = x$VALUE, Message = x$MESSAGE, Severity = x$SEVERITY, Owner = x$OWNER,
                    stringsAsFactors = FALSE)
    if (!is.null(x$REVIEW)) {
      d$Review <- x$REVIEW
      d$`Marked by` <- x$MARKED
    }
    table(sh, d, 11)
  }

  # One sheet per monitored form, coloured by record status
  for (i in seq_along(forms)) {
    sh <- data_sheets[i]
    fc <- fcs[[forms[i]]]
    t <- fc$rows
    drop <- c("STUDYID", "SITEID", "SUBJID", "FORMOID", "RECID", "ENTRYDT", "UPDDT", "AVAILDT", "VERSION", "DELFL")
    cols <- setdiff(names(t), drop)
    if ("FOLDER" %in% cols && all(is.na(t$FOLDER))) cols <- setdiff(cols, "FOLDER")       # log forms have no visit
    o <- order(match(fc$status, c("New", "Changed", "Deleted", "Unchanged")), t$SUBJID)
    t <- t[o, , drop = FALSE]
    st <- fc$status[o]
    d <- data.frame(Status = ifelse(st == "Unchanged", "", status_label(st)), Changed = changed_text(t$RECID, fc$fields),
                    Subject = t$SUBJID, Site = t$SITEID, check.names = FALSE, stringsAsFactors = FALSE)
    d <- cbind(d, as.data.frame(lapply(t[cols], function(v) {
      v <- as.character(fmt_cell(v))
      v[is.na(v)] <- ""
      v
    }), stringsAsFactors = FALSE, check.names = FALSE))
    if (!is.null(con)) {
      h <- record_hash_table(study, forms[i], rd$asof)
      key <- paste0(forms[i], ":", t$RECID)
      m <- rtcrcore::apply_marks(con, study$id, data.frame(TARGET_KEY = key, DEPHASH = h$DEPHASH[match(key, h$TARGET_KEY)]),
                                 "record")
      d$Review <- m$REVIEW_STATUS
      d$`Marked by` <- mark_text(m)
    }
    openxlsx::addWorksheet(wb, sh)
    openxlsx::writeData(wb, sh, sprintf("%s: new and updated records since %s, as of %s", forms[i],
                                        format(as.Date(since)), format(rd$asof)), startRow = 1)
    openxlsx::addStyle(wb, sh, title, rows = 1, cols = 1)
    openxlsx::writeFormula(wb, sh, startCol = 1, startRow = 2,
                           x = openxlsx::makeHyperlinkString(sheet = "Contents", row = 1, col = 1, text = "Back to contents"))
    openxlsx::addStyle(wb, sh, link, rows = 2, cols = 1)
    table(sh, d, 4)
    for (k in c("New", "Changed", "Deleted")) {
      rows <- 4 + which(st == k)
      if (length(rows)) {
        openxlsx::addStyle(wb, sh, openxlsx::createStyle(fgFill = change_colours[[k]],
                                                         textDecoration = if (k == "Deleted") "strikeout" else NULL),
                           rows = rows, cols = seq_len(ncol(d)), gridExpand = TRUE)
      }
    }
    f2 <- fc$fields[fc$fields$VARIABLE %in% names(d), , drop = FALSE]
    if (nrow(f2)) {
      openxlsx::addStyle(wb, sh, openxlsx::createStyle(fgFill = changed_cell_colour),
                         rows = 4 + match(f2$RECID, t$RECID), cols = match(f2$VARIABLE, names(d)), gridExpand = FALSE)
    }
  }

  # Patient profile
  openxlsx::addWorksheet(wb, "Patient profile")
  table("Patient profile", profile_long(rd, study, con), 1, widths = c(10, 6, 9, 10, 9, 16, 12, 40, 9, 11, 14, 30))

  openxlsx::addWorksheet(wb, "About")
  openxlsx::writeData(wb, "About", about, headerStyle = hs)
  openxlsx::setColWidths(wb, "About", 1:2, c(18, 70))
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  invisible(file)
}

# Excel sheet names: at most 31 characters, none of []:*?/\, unique, and not
# clashing with the fixed sheets.
check_sheet_names <- function(ids) {
  n <- substr(gsub("[\\[\\]:*?/\\\\]", "_", ids, perl = TRUE), 1, 27)   # room for a "_99" suffix
  k <- tolower(n) %in% c("contents", "patient profile", "about")
  n[k] <- paste0(n[k], "_")
  make.unique(n, sep = "_")
}

mark_text <- function(m) {
  ifelse(is.na(m$MARK_USER), "", paste0(m$MARK_USER, ": ", m$MARK_OUTCOME,
                                        ifelse(is.na(m$MARK_COMMENT), "", paste0(" (", m$MARK_COMMENT, ")"))))
}

# Every subject's data in long form: ADSL first, then each clinical form's
# records in visit order, one row per field. Values are text so one column
# can hold dates, numbers and codes.
profile_long <- function(rd, study, con = NULL) {
  as_text <- function(v) {
    v <- fmt_cell(v)
    v <- as.character(v)
    v[is.na(v)] <- ""
    v
  }
  header <- c("STUDYID", "SITEID", "SUBJID", "FORMOID", "FOLDER", "VISITNUM", "RECID", "ENTRYDT", "UPDDT",
              "AVAILDT", "VERSION", "DELFL")
  out <- list()
  a <- rd$adsl
  if (!is.null(a) && nrow(a)) {
    vars <- setdiff(names(a), c("STUDYID", "SITEID", "SUBJID", grep("SRC$", names(a), value = TRUE)))
    out$ADSL <- data.frame(Subject = rep(a$SUBJID, each = length(vars)), Site = rep(a$SITEID, each = length(vars)),
                           Form = "ADSL", Folder = "", Visit = NA_real_, Record = rep(a$SUBJID, each = length(vars)),
                           Entered = "", Field = rep(vars, nrow(a)),
                           Value = as.vector(t(vapply(vars, function(v) as_text(a[[v]]), character(nrow(a))))),
                           Source = "Derived", stringsAsFactors = FALSE)
  }
  for (f in clinical_forms(study)) {
    t <- rd$tables[[f]]
    if (is.null(t) || !nrow(t)) next
    vars <- setdiff(names(t), header)
    if (!length(vars)) next
    derived <- setdiff(names(t), names(study$tables[[f]]))
    key <- paste0(f, ":", t$RECID)
    rec <- data.frame(Subject = t$SUBJID, Site = t$SITEID, Form = f, Folder = as_text(t$FOLDER), Visit = t$VISITNUM,
                      Record = t$RECID, Entered = as_text(t$ENTRYDT), KEY = key, stringsAsFactors = FALSE)
    if (!is.null(con)) {
      h <- record_hash_table(study, f, rd$asof)
      m <- rtcrcore::apply_marks(con, study$id, data.frame(TARGET_KEY = key, DEPHASH = h$DEPHASH[match(key, h$TARGET_KEY)]),
                                 "record")
      rec$Review <- m$REVIEW_STATUS
      rec$`Marked by` <- mark_text(m)
    }
    long <- rec[rep(seq_len(nrow(rec)), each = length(vars)), , drop = FALSE]
    long$Field <- rep(vars, nrow(t))
    long$Value <- as.vector(t(vapply(vars, function(v) as_text(t[[v]]), character(nrow(t)))))
    long$Source <- ifelse(long$Field %in% derived, "Derived", "Collected")
    out[[f]] <- long
  }
  cols <- c("Subject", "Site", "Form", "Folder", "Visit", "Record", "Entered", "Field", "Value", "Source",
            "Review", "Marked by")
  out <- lapply(out, function(d) {
    for (c in setdiff(cols, names(d))) d[[c]] <- ""
    d[cols]
  })
  d <- do.call(rbind, out)
  if (is.null(d)) return(stats::setNames(data.frame(matrix(character(), 0, length(cols))), cols))
  if (is.null(con)) d <- d[setdiff(cols, c("Review", "Marked by"))]
  form_order <- match(d$Form, c("ADSL", clinical_forms(study)))
  d <- d[order(d$Subject, form_order, d$Visit, d$Record, na.last = TRUE), , drop = FALSE]
  rownames(d) <- NULL
  d
}
