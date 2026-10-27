# Shared building blocks: every display uses the same table card (subject
# links, review-mark column and controls, Excel button), badges and colours.

status_colours <- c("Complete" = "#b6dcb6", "Out of window" = "#fab219", "Overdue / not entered" = "#ec835a",
                    "Missed" = "#d03b3b", "Not yet due" = "#e9e8e3", "Discontinued" = "#9d9c96")

badge_class <- c(
  "New" = "b-new", "Changed" = "b-chg", "Deleted" = "b-crit", "Unchanged" = "b-chg", "Persisting" = "b-chg",
  "Resolved" = "b-ok", "error" = "b-crit", "warning" = "b-warn", "info" = "b-new", "pass" = "b-ok",
  "reviewed" = "b-ok", "needs re-review" = "b-warn", "not reviewed" = "b-chg",
  "Complete" = "b-ok", "Out of window" = "b-warn", "Overdue / not entered" = "b-ser", "Missed" = "b-crit",
  "Not yet due" = "b-chg", "Discontinued" = "b-chg",
  "Needs attention" = "b-crit", "Watch" = "b-warn", "On track" = "b-ok",
  "Open" = "b-warn", "Answered" = "b-new", "Closed" = "b-ok",
  "Locked" = "b-ok", "Ready to lock" = "b-new", "Not ready" = "b-warn",
  "Frozen" = "b-ok", "Signed" = "b-new", "SDV complete" = "b-chg", "Entered" = "b-chg",
  "not run" = "b-crit", "Not coded" = "b-warn", "Consistent" = "b-ok", "Inconsistent" = "b-crit", "Older version" = "b-chg")

# Quiet look: white ground, one blue for actions, colour only where it carries
# meaning (flags, status), thin rules instead of boxes.
app_css <- "
  body{color:#16181d}
  .note{font-size:.8rem;color:#5f6673}
  .mono,.sid,.sid-big,.inbox-id{font-family:'IBM Plex Mono',ui-monospace,monospace}
  .kpi{padding:.1rem 0;height:100%}
  .kpi .lab{font-size:.78rem;color:#5f6673}
  .kpi .val{font-size:1.75rem;font-weight:600;line-height:1.2}
  .kpi .sub{font-size:.78rem;color:#5f6673}
  .badge-s{display:inline-block;padding:.05rem .45rem;border-radius:4px;font-size:.72rem;font-weight:500;white-space:nowrap}
  .b-crit{background:#fdecea;color:#a1251b}.b-warn{background:#fff3d6;color:#6b4a00}.b-ser{background:#fde8df;color:#8a3a17}
  .b-new{background:#eef2fa;color:#2459c9}.b-chg{background:#f1f2f4;color:#3b414c}.b-ok{background:#e7f4ec;color:#1f6b45}
  .card{border-color:#e6e8ec;box-shadow:none}
  .card-header{background:#fff;border-bottom-color:#eef0f3;font-weight:600;font-size:.9rem;display:flex;
    justify-content:space-between;align-items:center;gap:.5rem}
  a.sid{color:#2459c9;font-weight:500;text-decoration:none;white-space:nowrap;cursor:pointer}
  a.sid:hover{text-decoration:underline}
  .mark-bar{display:flex;flex-wrap:wrap;gap:.5rem;align-items:end;margin-top:.5rem}
  .mark-bar .form-group{margin-bottom:0}
  table.dataTable{font-size:.84rem}
  table.dataTable thead th{font-weight:500;color:#5f6673}
  .side{background:#f8f9fb}
  .side-brand{font-weight:600;padding:.1rem .6rem .9rem}
  .side-nav{display:flex;flex-direction:column;gap:1px}
  .side-group{font-size:.7rem;text-transform:uppercase;letter-spacing:.07em;color:#5f6673;padding:.9rem .6rem .25rem}
  .side-link{display:flex;justify-content:space-between;align-items:center;padding:.38rem .6rem;border-radius:6px;
    color:#3b414c;text-decoration:none;font-size:.88rem}
  .side-link:hover{background:#eef0f3;color:#16181d}
  .side-link.active{background:#e6ecf8;color:#16181d;font-weight:600}
  .side-count{font-size:.75rem;color:#2459c9;font-weight:600}
  .side-foot{font-size:.75rem;color:#5f6673;padding:1.2rem .6rem 0}
  .page-head{display:flex;flex-wrap:wrap;align-items:end;justify-content:space-between;gap:.75rem 1.5rem;
    padding-bottom:.9rem;margin-bottom:1.1rem;border-bottom:1px solid #e6e8ec}
  .page-title{font-size:1.35rem;font-weight:600;margin:0}
  .ctx-strip{display:flex;flex-wrap:wrap;gap:.5rem .75rem;align-items:end}
  .ctx-strip .form-group{margin-bottom:0}
  .ctx-strip label{font-size:.72rem;color:#5f6673;margin-bottom:.1rem}
  .ctx-strip .form-control,.ctx-strip .selectize-input{font-size:.84rem;padding-top:.3rem;padding-bottom:.3rem;min-height:0;
    line-height:1.5}
  .ctx-strip .shiny-input-container{margin-bottom:0}
  .ctx-strip .selectize-control{margin-bottom:0}
  .ctx-strip .selectize-control.form-control{padding:0;border:0;height:auto;background:none}
  .ctx-strip .selectize-input{height:32px;display:flex;align-items:center;overflow:hidden}
  .ctx-strip .selectize-input>input{font-size:.84rem}
  .load-banner{display:flex;gap:.75rem;align-items:center;padding:.45rem .8rem;margin:-.4rem 0 1rem;border-radius:6px;
    background:#fff3d6;color:#6b4a00;font-size:.85rem}
  .review-progress{display:flex;flex-wrap:wrap;gap:.4rem 1rem;align-items:center;margin-bottom:1rem}
  .review-progress .grow{flex:1;min-width:240px}
  .track{height:4px;border-radius:2px;background:#eef0f3;overflow:hidden;margin-top:.4rem}
  .track i{display:block;height:100%;background:#1f7a4d}
  .section-h{margin:1.75rem 0 .75rem;font-weight:600}
  .stack-item + .stack-item{margin-top:1rem}
  .site-head{display:flex;flex-wrap:wrap;align-items:center;gap:.4rem .9rem;margin-bottom:.75rem}
  .site-head h4{margin:0;font-weight:600}
  .inbox{display:flex;flex-wrap:wrap;border:1px solid #e6e8ec;border-radius:8px;min-height:560px}
  .inbox-list{flex:1 1 260px;max-width:330px;border-right:1px solid #e6e8ec;max-height:78vh;overflow-y:auto}
  .inbox-main{flex:999 1 480px;min-width:0;display:flex;flex-direction:column}
  .inbox-main .inbox-scroll{padding:1.1rem 1.4rem;flex:1;overflow-y:auto;max-height:66vh}
  .inbox-head{padding:1rem 1.4rem .8rem;border-bottom:1px solid #eef0f3}
  .inbox-item{display:block;padding:.6rem .9rem;border-bottom:1px solid #f0f1f4;color:inherit;text-decoration:none;
    cursor:pointer;border-left:3px solid transparent}
  .inbox-item:hover{background:#f6f7f9}
  .inbox-item.cur{background:#f2f5fb;border-left-color:#2459c9}
  .inbox-item.done{color:#5f6673}
  .inbox-row{display:flex;justify-content:space-between;gap:.5rem;align-items:baseline}
  .inbox-id{font-weight:500;font-size:.86rem}
  .inbox-chips{display:flex;flex-wrap:wrap;gap:.25rem;margin-top:.3rem}
  .inbox-sep{font-size:.72rem;text-transform:uppercase;letter-spacing:.07em;color:#5f6673;padding:.8rem .9rem .3rem}
  .inbox-empty{padding:3rem 1.4rem;color:#5f6673}
  .inbox-foot,.panel-foot{border-top:1px solid #e6e8ec;padding:.7rem 1.4rem;display:flex;flex-wrap:wrap;gap:.5rem;
    align-items:center;background:#fff}
  .inbox-foot .form-group,.panel-foot .form-group{margin-bottom:0}
  .inbox-foot .comment,.panel-foot .comment{flex:1 1 200px}
  .subj-panel{--bs-offcanvas-width:min(700px,100vw)}
  .subj-panel .offcanvas-header{border-bottom:1px solid #e6e8ec;align-items:flex-start}
  .panel-id{display:flex;flex-wrap:wrap;align-items:center;gap:.35rem}
  .sid-big{font-size:1.3rem;font-weight:600;margin-right:.3rem}
  .panel-sec{padding:.2rem 0 .8rem;margin-bottom:.8rem;border-bottom:1px solid #eef0f3}
  .panel-sec-h{display:flex;justify-content:space-between;gap:.5rem;font-weight:600;font-size:.88rem;margin-bottom:.4rem}
  .panel-sec.is-new .panel-sec-h span:first-child::before{content:'';display:inline-block;width:7px;height:7px;
    border-radius:50%;background:#2459c9;margin-right:.45rem;vertical-align:middle}
  .panel-tbl{overflow-x:auto}
  .panel-tbl table{font-size:.8rem;margin-bottom:0;background:transparent}
  .panel-tbl th{white-space:nowrap;font-weight:500;color:#5f6673}
  .panel-tbl td:first-child{white-space:nowrap}
  .panel-tbl tr.row-new td{background:#f2f5fb}
  .panel-tbl mark{background:#fff3d6;padding:0 .15rem;border-radius:3px;cursor:help}
  .panel-h{margin:1.1rem 0 .4rem;font-size:.72rem;text-transform:uppercase;letter-spacing:.07em;color:#5f6673;font-weight:600}
  .panel-all summary{display:flex;justify-content:space-between;cursor:pointer;padding:.45rem 0;font-size:.88rem;
    border-bottom:1px solid #f0f1f4}
  .panel-all[open] summary{margin-bottom:.4rem}
  .panel-tl{margin-bottom:.8rem}
  .panel-tl .tl-axis{stroke:#e6e8ec}.panel-tl .tl-day1{stroke:#2459c9;stroke-dasharray:3 3}
  .panel-tl .tl-pt{fill:#9aa1ad}.panel-tl .tl-new{fill:#2459c9}.panel-tl .tl-lab{font-size:10px;fill:#5f6673}
  .kb-hint{font-size:.75rem;color:#5f6673;margin-left:auto}
"

# Sidebar navigation, the subject panel, and keys n/p/m (next, previous, mark
# reviewed and next) in the open subject panel or else the review inbox.
app_js <- "
  function rtcrNav(a) {
    Shiny.setInputValue('nav', a.dataset.value, {priority: 'event'});
    var t = document.querySelector('.bslib-sidebar-layout > .collapse-toggle');
    if (t && window.matchMedia('(max-width: 575.98px)').matches) t.click();
  }
  Shiny.addCustomMessageHandler('navactive', function(id) {
    document.querySelectorAll('.side-link').forEach(function(a) { a.classList.toggle('active', a.dataset.value === id); });
  });
  Shiny.addCustomMessageHandler('subjpanel', function(m) {
    var oc = bootstrap.Offcanvas.getOrCreateInstance(document.getElementById(m.id));
    if (m.show) oc.show(); else oc.hide();
  });
  document.addEventListener('keydown', function(e) {
    if (e.ctrlKey || e.metaKey || e.altKey) return;
    var t = e.target;
    if (t.isContentEditable || ['INPUT', 'TEXTAREA', 'SELECT'].indexOf(t.tagName) >= 0) return;
    var k = {n: 'kb-next', p: 'kb-prev', m: 'kb-mark'}[e.key];
    if (!k) return;
    var scope = document.querySelector('.subj-panel.show') || document.querySelector('.tab-pane.active .inbox');
    var b = scope && scope.querySelector('.' + k);
    if (!b) return;
    e.preventDefault();
    b.click();
  });
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

site_link <- function(id) {
  ifelse(is.na(id), "", sprintf(
    "<a class=\"sid\" onclick=\"Shiny.setInputValue('goto_site','%s',{priority:'event'})\">%s</a>",
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

# What every export records about itself.
about_table <- function(ctx) {
  data.frame(
    Item = c("Study", "Review date", "Changes since", "Site", "Extract date", "Generated (UTC)", "User", "Note"),
    Value = c(ctx$study$id, format(ctx$asof()), format(ctx$since()), ctx$site(), format(ctx$study$extract_date),
              format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC"), ctx$user,
              "Review marks are read-only here; the app is the record of review."),
    stringsAsFactors = FALSE)
}

# Excel export: the data sheet(s) plus an About sheet that records the context.
write_export <- function(sheets, file, ctx) {
  about <- about_table(ctx)
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
