# New and updated data, as in the 2017 review workbook: every listing of a
# monitored form shows each record's status since the earlier date in its own
# colour (new, updated, deleted), and marks each changed value with its
# earlier value. The core calls a corrected record "Changed"; reviewers see
# "Updated".

change_colours <- c(New = "#d9e8fc", Changed = "#fff0c2", Deleted = "#e6e8ec")
changed_cell_colour <- "#ffcf5c"

status_label <- function(x) ifelse(!is.na(x) & x == "Changed", "Updated", x)

# Forms reviewers monitor: study.yml app: monitored, else every clinical form,
# priority forms first.
monitored_forms <- function(study) {
  m <- unlist(study$config$app$monitored)
  if (length(m)) return(intersect(m, names(study$tables)))
  f <- clinical_forms(study)
  p <- match(f, unlist(study$config$app$priority))
  f[order(is.na(p), p)]
}

# One form's records on the review date plus records deleted in the window,
# with status, changed fields and their earlier values.
form_changes <- function(study, data, form, since, asof) {
  t <- data$tables[[form]]
  raw <- study$tables[[form]]
  cl <- rtcrcore::change_log(raw, since, asof)
  if (!is.null(data$adsl)) cl <- cl[cl$SUBJID %in% data$adsl$SUBJID, , drop = FALSE]   # the site filter
  gone <- cl[cl$RECSTAT == "Deleted", intersect(names(cl), names(t)), drop = FALSE]
  rows <- if (nrow(gone)) merge_rows(t, gone) else t
  status <- cl$RECSTAT[match(rows$RECID, cl$RECID)]
  status[is.na(status)] <- "Unchanged"
  cf <- rtcrcore::change_fields(raw, since, asof)
  list(rows = rows, status = status, fields = cf[cf$RECID %in% rows$RECID, , drop = FALSE])
}

merge_rows <- function(t, extra) {
  for (c in setdiff(names(t), names(extra))) extra[[c]] <- if (nrow(extra)) NA else t[[c]][0]
  extra <- extra[names(t)]
  for (c in names(t)) if (inherits(t[[c]], "Date") && !inherits(extra[[c]], "Date")) extra[[c]] <- as.Date(extra[[c]])
  rbind(t, extra)
}

# Display cells (escaped text) with each changed value marked; hovering shows
# the earlier value.
mark_changed_cells <- function(cells, recid, fields) {
  for (i in seq_len(nrow(fields))) {
    r <- match(fields$RECID[i], recid)
    v <- fields$VARIABLE[i]
    if (is.na(r) || !v %in% names(cells)) next
    old <- if (is.na(fields$OLD[i])) "blank" else fields$OLD[i]
    cells[[v]][r] <- sprintf('<mark class="upd" title="was: %s">%s</mark>', htmltools::htmlEscape(old),
                             if (nzchar(cells[[v]][r])) cells[[v]][r] else "&nbsp;")
  }
  cells
}

# "AESEV: MILD -> MODERATE; AEOUT: ..." per record, for exports.
changed_text <- function(recid, fields) {
  vapply(recid, function(r) {
    f <- fields[fields$RECID == r, , drop = FALSE]
    if (!nrow(f)) return("")
    paste(sprintf("%s: %s \u2192 %s", f$VARIABLE, ifelse(is.na(f$OLD), "blank", f$OLD),
                  ifelse(is.na(f$NEW), "blank", f$NEW)), collapse = "; ")
  }, "", USE.NAMES = FALSE)
}

change_legend <- function() {
  sw <- function(col, label, extra = "") {
    shiny::span(class = "lg", shiny::span(class = "sw", style = sprintf("background:%s;%s", col, extra)), label)
  }
  shiny::div(class = "change-legend",
             sw(change_colours[["New"]], "New"),
             sw(change_colours[["Changed"]], "Updated"),
             sw(changed_cell_colour, "Changed value (hover for the earlier value)"),
             sw(change_colours[["Deleted"]], "Deleted", "text-decoration:line-through"))
}
