# Shared building blocks: every display uses the same table card (subject
# links, review-mark column and controls, Excel button), badges and colours.

status_colours <- c("Complete" = "#b6dcb6", "Out of window" = "#fab219", "Overdue / not entered" = "#ec835a",
                    "Missed" = "#d03b3b", "Not yet due" = "#e9e8e3", "Discontinued" = "#9d9c96")

badge_class <- c(
  "New" = "b-new", "Changed" = "b-chg", "Deleted" = "b-crit", "Unchanged" = "b-chg", "Persisting" = "b-chg",
  "Resolved" = "b-ok", "error" = "b-crit", "warning" = "b-warn", "info" = "b-new", "pass" = "b-ok",
  "reviewed" = "b-ok", "needs re-review" = "b-warn", "not reviewed" = "b-chg",
  "Complete" = "b-ok", "Out of window" = "b-warn", "Overdue / not entered" = "b-ser", "Missed" = "b-crit",
  "Not yet due" = "b-chg", "Discontinued" = "b-chg")

app_css <- "
  .kpi{border:1px solid #e7e6e2;border-radius:10px;padding:.7rem .9rem;background:#fff;height:100%}
  .kpi .lab{font-size:.72rem;color:#52514e;text-transform:uppercase;letter-spacing:.04em}
  .kpi .val{font-size:1.6rem;font-weight:650;line-height:1.15}
  .kpi .sub{font-size:.78rem;color:#52514e}
  .badge-s{display:inline-block;padding:.1rem .45rem;border-radius:999px;font-size:.72rem;font-weight:600;white-space:nowrap}
  .b-crit{background:#fbe3e3;color:#8f1f1f}.b-warn{background:#fff1cf;color:#6b4a00}.b-ser{background:#fde8df;color:#8a3a17}
  .b-new{background:#e3eefb;color:#174b8f}.b-chg{background:#ecebe7;color:#3b3a37}.b-ok{background:#e4f3e4;color:#145c14}
  .card-header{font-weight:600;font-size:.92rem;display:flex;justify-content:space-between;align-items:center;gap:.5rem}
  .note{font-size:.78rem;color:#6b6a65}
  a.sid{color:#2a78d6;font-weight:600;text-decoration:none;white-space:nowrap;cursor:pointer}
  .mark-bar{display:flex;flex-wrap:wrap;gap:.5rem;align-items:end;margin-top:.5rem}
  .mark-bar .form-group{margin-bottom:0}
  table.dataTable{font-size:.83rem}
"

badge <- function(x) {
  cls <- unname(badge_class[x])
  cls[is.na(cls)] <- "b-new"
  ifelse(is.na(x) | x == "", "", sprintf('<span class="badge-s %s">%s</span>', cls, htmltools::htmlEscape(x)))
}

sid_link <- function(id) {
  ifelse(is.na(id), "", sprintf(
    "<a class=\"sid\" onclick=\"Shiny.setInputValue('goto_subject','%s',{priority:'event'})\">%s</a>",
    htmltools::htmlEscape(id), htmltools::htmlEscape(id)))
}

kpi <- function(label, value, sub = NULL) {
  shiny::div(class = "kpi", shiny::div(class = "lab", label), shiny::div(class = "val", value),
             if (!is.null(sub)) shiny::div(class = "sub", sub))
}

fmt_cell <- function(v) {
  if (inherits(v, "POSIXct")) return(format(v, "%Y-%m-%d %H:%M", tz = "UTC"))
  if (inherits(v, "Date")) return(format(v))
  if (is.numeric(v)) return(ifelse(is.na(v), NA_character_, format(v, trim = TRUE, drop0trailing = TRUE)))
  v
}

strip_html <- function(x) if (is.character(x)) gsub("<[^>]+>", "", x) else x

# Hidden columns carry what marks need; they start with "." and are never shown or exported.
mark_cols <- function(key, dephash, subjid, version = NA_real_) {
  data.frame(.KEY = key, .DEPHASH = dephash, .SUBJID = subjid, .VERSION = version, stringsAsFactors = FALSE)
}

#' Table card
#'
#' A DT table with an Excel button and, when `mark_type` is set, review-mark
#' controls for the selected rows. Columns whose names start with `.` are
#' hidden; `.KEY`, `.DEPHASH`, `.SUBJID` and `.VERSION` feed the marks.
#'
#' @param id Module id.
#' @param title Card title.
#' @param note Optional note under the title.
#' @param mark_type `NULL`, `"record"` or `"finding"`.
#' @return UI for the card.
#' @export
table_card_ui <- function(id, title, note = NULL, mark_type = NULL) {
  ns <- shiny::NS(id)
  bslib::card(
    fill = FALSE,
    bslib::card_header(shiny::span(title), shiny::downloadButton(ns("xl"), "Excel", class = "btn-sm", icon = NULL)),
    if (!is.null(note)) shiny::p(class = "note", note),
    DT::DTOutput(ns("tbl"), fill = FALSE),
    if (!is.null(mark_type)) shiny::div(
      class = "mark-bar",
      shiny::selectInput(ns("outcome"), "Outcome", rtcrcore_outcomes(), width = "160px"),
      shiny::textInput(ns("comment"), "Comment", width = "280px"),
      shiny::actionButton(ns("mark"), "Mark selected", class = "btn-sm btn-primary"),
      shiny::actionButton(ns("clear"), "Clear mark", class = "btn-sm"))
  )
}

#' @rdname table_card_ui
#' @param data Reactive data frame (may contain HTML in display columns).
#' @param ctx App context (see [rtcr_app()]).
#' @param sheet Sheet and file name for the Excel export.
#' @param page_length Rows per page.
#' @param selection DT selection mode; defaults to multiple when marking.
#' @return A reactive with the selected rows of `data`.
#' @export
table_card_server <- function(id, data, ctx, sheet, mark_type = NULL, page_length = 10,
                              selection = if (is.null(mark_type)) "single" else "multiple") {
  shiny::moduleServer(id, function(input, output, session) {
    output$tbl <- DT::renderDT({
      d <- data()
      hidden <- which(startsWith(names(d), ".")) - 1
      DT::datatable(d, escape = FALSE, rownames = FALSE, selection = selection,
                    options = list(pageLength = page_length, scrollX = TRUE, order = list(),
                                   dom = if (nrow(d) > page_length) "ftip" else "ft",
                                   columnDefs = list(list(visible = FALSE, targets = hidden)),
                                   language = list(emptyTable = "No records.")))
    })
    output$xl <- shiny::downloadHandler(
      filename = function() paste0(gsub("[^A-Za-z0-9]+", "_", sheet), "_", format(ctx$asof(), "%Y%m%d"), ".xlsx"),
      content = function(file) {
        d <- data()
        d <- d[!startsWith(names(d), ".")]
        d[] <- lapply(d, strip_html)
        write_export(stats::setNames(list(d), substr(sheet, 1, 31)), file, ctx)
        rtcrcore::log_access(ctx$con, ctx$study$id, ctx$user, "export", basename(file),
                             paste("sheet", sheet, "as of", format(ctx$asof())))
      })
    selected <- shiny::reactive({
      i <- input$tbl_rows_selected
      if (length(i)) data()[i, , drop = FALSE] else NULL
    })
    if (!is.null(mark_type)) {
      write <- function(clear) {
        rows <- selected()
        if (is.null(rows)) {
          shiny::showNotification("Select one or more rows first.", type = "warning")
          return()
        }
        n <- 0
        for (i in seq_len(nrow(rows))) {
          ok <- tryCatch({
            if (clear) {
              rtcrcore::clear_mark(ctx$con, ctx$study$id, mark_type, rows$.KEY[i], ctx$user,
                                   comment = nz(input$comment))
            } else {
              rtcrcore::set_mark(ctx$con, ctx$study$id, mark_type, rows$.KEY[i], ctx$user, input$outcome,
                                 comment = nz(input$comment), dephash = rows$.DEPHASH[i], subjid = rows$.SUBJID[i],
                                 version = rows$.VERSION[i])
            }
            TRUE
          }, error = function(e) {
            shiny::showNotification(paste(rows$.KEY[i], conditionMessage(e)), type = "error")
            FALSE
          })
          n <- n + ok
        }
        if (n) {
          shiny::showNotification(sprintf("%s %d row(s).", if (clear) "Cleared" else "Marked", n), type = "message")
          ctx$bump_marks()
          shiny::updateTextInput(session, "comment", value = "")
        }
      }
      shiny::observeEvent(input$mark, write(FALSE))
      shiny::observeEvent(input$clear, write(TRUE))
    }
    selected
  })
}

nz <- function(x) if (is.null(x) || !nzchar(trimws(x))) NA_character_ else x

rtcrcore_outcomes <- function() c("no action", "query raised", "escalated")

# Attach review status to rows by key, re-reading marks whenever they change.
with_marks <- function(d, ctx, type) {
  ctx$marks_rev()
  items <- if (type == "finding") data.frame(FINDINGKEY = d$.KEY, DEPHASH = d$.DEPHASH) else
    data.frame(TARGET_KEY = d$.KEY, DEPHASH = d$.DEPHASH)
  m <- rtcrcore::apply_marks(ctx$con, ctx$study$id, items, type)
  d$Review <- badge(m$REVIEW_STATUS)
  d$`Marked by` <- ifelse(is.na(m$MARK_USER), "", paste0(m$MARK_USER, ": ", m$MARK_OUTCOME,
                                                         ifelse(is.na(m$MARK_COMMENT), "", paste0(" (", m$MARK_COMMENT, ")"))))
  d
}

# Excel export: the data sheet(s) plus an About sheet that records the context.
write_export <- function(sheets, file, ctx) {
  about <- data.frame(
    Item = c("Study", "Review date", "Changes since", "Site", "Extract date", "Generated (UTC)", "User", "Note"),
    Value = c(ctx$study$id, format(ctx$asof()), format(ctx$since()), ctx$site(), format(ctx$study$extract_date),
              format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC"), ctx$user,
              "Review marks are read-only here; the app is the record of review."))
  wb <- openxlsx::createWorkbook()
  hs <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#DDEBF7")
  for (nm in c(names(sheets), "About")) {
    d <- if (nm == "About") about else sheets[[nm]]
    openxlsx::addWorksheet(wb, nm)
    openxlsx::writeData(wb, nm, d, headerStyle = hs)
    openxlsx::freezePane(wb, nm, firstRow = TRUE)
    if (ncol(d)) openxlsx::addFilter(wb, nm, rows = 1, cols = seq_len(ncol(d)))
  }
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
}
