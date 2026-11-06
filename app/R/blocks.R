# Shared building blocks: every display uses the same table card (subject
# links, review-mark column and controls, Excel button), badges and colours.

status_colours <- c("Complete" = "#b6dcb6", "Out of window" = "#fab219", "Overdue / not entered" = "#ec835a",
                    "Missed" = "#d03b3b", "Not yet due" = "#e9e8e3", "Discontinued" = "#9d9c96")

badge_class <- c(
  "New" = "b-new", "Changed" = "b-upd", "Updated" = "b-upd", "Deleted" = "b-crit", "Unchanged" = "b-chg", "Persisting" = "b-chg",
  "Resolved" = "b-ok", "error" = "b-crit", "warning" = "b-warn", "info" = "b-new", "pass" = "b-ok",
  "reviewed" = "b-ok", "needs re-review" = "b-warn", "not reviewed" = "b-chg",
  "Complete" = "b-ok", "Out of window" = "b-warn", "Overdue / not entered" = "b-ser", "Missed" = "b-crit",
  "Not yet due" = "b-chg", "Discontinued" = "b-chg",
  "Needs attention" = "b-crit", "Watch" = "b-warn", "On track" = "b-ok",
  "Open" = "b-warn", "Answered" = "b-new", "Closed" = "b-ok",
  "Locked" = "b-ok", "Ready to lock" = "b-new", "Not ready" = "b-warn",
  "Frozen" = "b-ok", "Signed" = "b-new", "SDV complete" = "b-chg", "Entered" = "b-chg",
  "not run" = "b-crit", "Not coded" = "b-warn", "Consistent" = "b-ok", "Inconsistent" = "b-crit", "Older version" = "b-chg")

# Warm paper look: cream ground, one teal for actions, colour only where it carries
# meaning (flags, status), thin rules instead of boxes.
app_css <- "
  body{color:#2b2620}
  .note{font-size:.8rem;color:#635a4f}
  .mono,.sid,.sid-big,.inbox-id{font-family:'IBM Plex Mono',ui-monospace,monospace}
  .kpi{padding:.1rem 0;height:100%}
  .kpi .lab{font-size:.78rem;color:#635a4f}
  .kpi .val{font-size:1.75rem;font-weight:600;line-height:1.2}
  .kpi .sub{font-size:.78rem;color:#635a4f}
  .badge-s{display:inline-block;padding:.05rem .45rem;border-radius:4px;font-size:.72rem;font-weight:500;white-space:nowrap}
  .b-crit{background:#f6e3df;color:#a61b1b}.b-warn{background:#f7e9cc;color:#6b4a00}.b-ser{background:#fde8df;color:#8a3a17}
  .b-new{background:#e3efe9;color:#0a4540}.b-upd{background:#ffe9b3;color:#6b4a00}.b-chg{background:#ece5d8;color:#3d362e}.b-ok{background:#e4efdf;color:#2f6b2f}
  .late{color:#8f5200;font-weight:500}
  .act-head{display:flex;flex-wrap:wrap;justify-content:space-between;align-items:flex-end;gap:.75rem;margin-bottom:.9rem}
  .act-sum{margin:.25rem 0 0;font-size:.95rem}
  .act-group{display:flex;align-items:center;gap:.8rem;flex-wrap:wrap}
  .act-count{min-width:2rem;height:1.6rem;padding:0 .5rem;border-radius:.8rem;background:#ece5d8;display:inline-flex;
    align-items:center;justify-content:center;font-size:.8rem;font-weight:600}
  .act-gtitle{font-weight:600}
  .act-row{all:unset;box-sizing:border-box;width:100%;display:flex;justify-content:space-between;gap:1rem;
    padding:.6rem .4rem;border-bottom:1px solid #ece5d8;cursor:pointer}
  .act-row:hover,.act-row:focus-visible{background:#f2ede4}
  .act-main{display:flex;flex-direction:column;gap:.1rem;min-width:0}
  .act-side{display:flex;flex-direction:column;align-items:flex-end;gap:.1rem;flex:none;text-align:right}
  .act-facts{display:grid;grid-template-columns:7rem 1fr;gap:.35rem 1rem;margin-bottom:1rem}
  .act-facts dd{margin:0}
  .act-hist{list-style:none;padding:0;margin:0 0 .5rem}
  .chk-showing{margin:1.1rem 0 .5rem}
  .fc-card{margin-bottom:1.1rem}
  .fc-chips{display:flex;flex-wrap:wrap;gap:.4rem;padding:.75rem 1rem .25rem}
  .fc-chip{all:unset;cursor:pointer;padding:.25rem .65rem;border:1px solid #d6cdbd;border-radius:999px;font-size:.8rem;
    background:#faf7f2}
  .fc-chip:hover{background:#f2ede4}.fc-chip:focus-visible{outline:2px solid #0f5e57;outline-offset:1px}
  .fc-chip.on{background:#e3efe9;border-color:#0f5e57;color:#0a4540;font-weight:600}
  .fc-card .card{border:0;margin:0}
  .cat-row{display:flex;justify-content:space-between;gap:1rem;padding:.7rem .4rem;border-bottom:1px solid #ece5d8}
  .cat-row .act-main{gap:.2rem}
  .cat-row.retired{opacity:.75}
  .cat-open{white-space:nowrap;margin-top:.3rem}
  .pf-head{display:flex;flex-wrap:wrap;gap:1rem 2rem;align-items:flex-start;padding:1rem 1.15rem;margin:.6rem 0 1.1rem;
    border:1px solid #e6dfd3;border-radius:8px;background:#f2ede4}
  .pf-id{font-family:'IBM Plex Mono',ui-monospace,monospace;font-size:1.5rem;font-weight:600;line-height:1.1}
  .pf-facts{display:flex;flex-wrap:wrap;gap:.8rem 1.9rem;flex:1 1 640px}
  .pf-lab{font-size:.75rem;color:#635a4f}.pf-val{font-size:.9rem;font-weight:500;margin-top:.1rem}
  .pf-look{flex:1 1 100%;display:flex;flex-wrap:wrap;gap:.45rem;align-items:center}
  .pf-meta{margin-left:auto}
  .pf-flag{display:inline-block;padding:.1rem .5rem;border-radius:4px;font-size:.76rem;font-weight:500;white-space:nowrap}
  .pf-crit{background:#f6e3df;color:#a61b1b}.pf-warn{background:#f7e9cc;color:#8f5200}.pf-info{background:#ece5d8;color:#635a4f}
  .pf-chips{display:inline-flex;flex-wrap:wrap;gap:.3rem;margin-left:auto;margin-right:.6rem}
  .pf-timeline svg.tl{display:block;width:100%;height:auto;max-width:1180px;font-family:'IBM Plex Sans',system-ui,sans-serif}
  .card-header .note{font-weight:400}
  .pull-chip{display:inline-block;padding:.05rem .45rem;border-radius:4px;background:#ece5d8;color:#3d362e;
    font-size:.76rem;white-space:nowrap}
  .pull-chip.recent{background:#e3efe9;color:#0a4540;font-weight:600}
  .pf-bar{display:flex;flex-wrap:wrap;align-items:flex-start;gap:.5rem 1.5rem}
  .pf-bar .form-group{margin-bottom:.4rem}
  .pf-custom{flex:1 1 100%}
  .pf-custom>summary{cursor:pointer;color:#0f5e57;font-size:.88rem;font-weight:500;width:max-content}
  .pf-custom-body{display:flex;flex-wrap:wrap;gap:.5rem 2.5rem;padding:.75rem 1rem;margin-top:.4rem;
    border:1px solid #e6dfd3;border-radius:8px;background:#faf7f2}
  .pf-custom-body .shiny-options-group{columns:2;column-gap:1.5rem}
  .pf-add{display:flex;flex-wrap:wrap;align-items:flex-end;gap:.5rem}
  .pf-forms-head{display:flex;flex-wrap:wrap;justify-content:space-between;align-items:baseline;gap:.5rem}
  .pf-forms .accordion-button .act-group{flex:1}
  .pf-forms .panel-tbl table{font-size:.82rem}
  .pf-forms .panel-tbl td,.pf-forms .panel-tbl th{white-space:nowrap}
  .pf-forms-head .section-h{font-size:1rem;margin:1.6rem 0 .6rem}
  .act-hist li{padding:.45rem 0;border-top:1px solid #ece5d8}
  .act-form{display:flex;flex-wrap:wrap;gap:.25rem 1rem;align-items:end}
  .card{border-color:#e6dfd3;box-shadow:none}
  .card-header{background:#faf7f2;border-bottom-color:#ece5d8;font-weight:600;font-size:.9rem;display:flex;
    justify-content:space-between;align-items:center;gap:.5rem}
  a.sid{color:#0f5e57;font-weight:500;text-decoration:none;white-space:nowrap;cursor:pointer}
  a.sid:hover{text-decoration:underline}
  .mark-bar{display:flex;flex-wrap:wrap;gap:.5rem;align-items:end;margin-top:.5rem}
  .mark-bar .form-group{margin-bottom:0}
  table.dataTable{font-size:.84rem}
  table.dataTable thead th{font-weight:500;color:#635a4f}
  .side{background:#f2ede4}
  .side-brand{font-weight:600;padding:.1rem .6rem .9rem}
  .side-nav{display:flex;flex-direction:column;gap:1px}
  .side-group{font-size:.7rem;text-transform:uppercase;letter-spacing:.07em;color:#635a4f;padding:.9rem .6rem .25rem}
  .side-sec>summary{list-style:none;cursor:pointer;display:flex;justify-content:space-between;align-items:center;
    border-radius:6px}
  .side-sec>summary::-webkit-details-marker{display:none}
  .side-sec>summary::after{content:'';width:.42rem;height:.42rem;border-right:1.5px solid currentColor;
    border-bottom:1.5px solid currentColor;transform:rotate(-45deg);margin-right:.2rem;transition:transform .15s}
  .side-sec[open]>summary::after{transform:rotate(45deg)}
  .side-sec>summary:hover{color:#2b2620}
  .side-sec>summary:focus-visible{outline:2px solid #0f5e57;outline-offset:1px}
  .side-sec-links{display:flex;flex-direction:column;gap:1px}
  .side-link{display:flex;justify-content:space-between;align-items:center;padding:.38rem .6rem;border-radius:6px;
    color:#3d362e;text-decoration:none;font-size:.88rem}
  .side-link:hover{background:#ece5d8;color:#2b2620}
  .side-link.active{background:#e3efe9;color:#2b2620;font-weight:600}
  .side-link.side-featured{margin:.35rem 0;border:1px solid #0f5e57;color:#0a4540;font-weight:600}
  .side-link.side-featured::before{content:'\\25cf';font-size:.55rem;color:#0f5e57;margin-right:.5rem}
  .side-link.side-featured span:first-child{margin-right:auto}
  .side-link.side-featured.active{background:#0f5e57;color:#faf7f2}
  .side-link.side-featured.active::before{color:#faf7f2}
  .side-count{font-size:.75rem;color:#0f5e57;font-weight:600}
  .side-foot{font-size:.75rem;color:#635a4f;padding:1.2rem .6rem 0}
  .page-head{display:flex;flex-wrap:wrap;align-items:end;justify-content:space-between;gap:.75rem 1.5rem;
    padding-bottom:.9rem;margin-bottom:1.1rem;border-bottom:1px solid #e6dfd3}
  .page-title{font-size:1.35rem;font-weight:600;margin:0}
  .ctx-strip{display:flex;flex-wrap:wrap;gap:.5rem .75rem;align-items:end}
  .ctx-strip .form-group{margin-bottom:0}
  .ctx-strip label{font-size:.72rem;color:#635a4f;margin-bottom:.1rem}
  .ctx-strip .form-control,.ctx-strip .selectize-input{font-size:.84rem;padding-top:.3rem;padding-bottom:.3rem;min-height:0;
    line-height:1.5}
  .ctx-strip .shiny-input-container{margin-bottom:0}
  .ctx-strip .selectize-control{margin-bottom:0}
  .ctx-strip .selectize-control.form-control{padding:0;border:0;height:auto;background:none}
  .ctx-strip .selectize-input{height:32px;display:flex;align-items:center;overflow:hidden}
  .ctx-strip .selectize-input>input{font-size:.84rem}
  .load-banner{display:flex;gap:.75rem;align-items:center;padding:.45rem .8rem;margin:-.4rem 0 1rem;border-radius:6px;
    background:#f7e9cc;color:#6b4a00;font-size:.85rem}
  .review-progress{display:flex;flex-wrap:wrap;gap:.4rem 1rem;align-items:center;margin-bottom:1rem}
  .review-progress .grow{flex:1;min-width:240px}
  .track{height:4px;border-radius:2px;background:#ece5d8;overflow:hidden;margin-top:.4rem}
  .track i{display:block;height:100%;background:#2f6b2f}
  .section-h{margin:1.75rem 0 .75rem;font-weight:600}
  .stack-item + .stack-item{margin-top:1rem}
  .site-head{display:flex;flex-wrap:wrap;align-items:center;gap:.4rem .9rem;margin-bottom:.75rem}
  .site-head h4{margin:0;font-weight:600}
  .inbox{display:flex;flex-wrap:wrap;border:1px solid #e6dfd3;border-radius:8px;min-height:560px}
  .inbox-list{flex:1 1 260px;max-width:330px;border-right:1px solid #e6dfd3;max-height:78vh;overflow-y:auto}
  .inbox-main{flex:999 1 480px;min-width:0;display:flex;flex-direction:column}
  .inbox-main .inbox-scroll{padding:1.1rem 1.4rem;flex:1;overflow-y:auto;max-height:66vh}
  .inbox-head{padding:1rem 1.4rem .8rem;border-bottom:1px solid #ece5d8}
  .inbox-item{display:block;padding:.6rem .9rem;border-bottom:1px solid #ece5d8;color:inherit;text-decoration:none;
    cursor:pointer;border-left:3px solid transparent}
  .inbox-item:hover{background:#f2ede4}
  .inbox-item.cur{background:#e3efe9;border-left-color:#0f5e57}
  .inbox-item.done{color:#635a4f}
  .inbox-row{display:flex;justify-content:space-between;gap:.5rem;align-items:baseline}
  .inbox-id{font-weight:500;font-size:.86rem}
  .inbox-chips{display:flex;flex-wrap:wrap;gap:.25rem;margin-top:.3rem}
  .inbox-sep{font-size:.72rem;text-transform:uppercase;letter-spacing:.07em;color:#635a4f;padding:.8rem .9rem .3rem}
  .inbox-empty{padding:3rem 1.4rem;color:#635a4f}
  .inbox-foot,.panel-foot{border-top:1px solid #e6dfd3;padding:.7rem 1.4rem;display:flex;flex-wrap:wrap;gap:.5rem;
    align-items:center;background:#faf7f2}
  .inbox-foot .form-group,.panel-foot .form-group{margin-bottom:0}
  .inbox-foot .comment,.panel-foot .comment{flex:1 1 200px}
  .subj-panel{--bs-offcanvas-width:min(700px,100vw)}
  .subj-panel .offcanvas-header{border-bottom:1px solid #e6dfd3;align-items:flex-start}
  .panel-id{display:flex;flex-wrap:wrap;align-items:center;gap:.35rem}
  .sid-big{font-size:1.3rem;font-weight:600;margin-right:.3rem}
  .panel-sec{padding:.2rem 0 .8rem;margin-bottom:.8rem;border-bottom:1px solid #ece5d8}
  .panel-sec-h{display:flex;justify-content:space-between;gap:.5rem;font-weight:600;font-size:.88rem;margin-bottom:.4rem}
  .panel-sec.is-new .panel-sec-h span:first-child::before{content:'';display:inline-block;width:7px;height:7px;
    border-radius:50%;background:#0f5e57;margin-right:.45rem;vertical-align:middle}
  .panel-tbl{overflow-x:auto}
  .panel-tbl table{font-size:.8rem;margin-bottom:0;background:transparent}
  .panel-tbl th{white-space:nowrap;font-weight:500;color:#635a4f}
  .panel-tbl td:first-child{white-space:nowrap}

  .panel-h{margin:1.1rem 0 .4rem;font-size:.72rem;text-transform:uppercase;letter-spacing:.07em;color:#635a4f;font-weight:600}
  .panel-all summary{display:flex;justify-content:space-between;cursor:pointer;padding:.45rem 0;font-size:.88rem;
    border-bottom:1px solid #ece5d8}
  .panel-all[open] summary{margin-bottom:.4rem}
  .panel-tl{margin-bottom:.8rem}
  .panel-tl .tl-axis{stroke:#e6dfd3}.panel-tl .tl-day1{stroke:#0f5e57;stroke-dasharray:3 3}
  .panel-tl .tl-pt{fill:#8f8473}.panel-tl .tl-new{fill:#0f5e57}.panel-tl .tl-lab{font-size:10px;fill:#635a4f}
  .change-legend{display:flex;flex-wrap:wrap;gap:.3rem 1rem;font-size:.78rem;color:#635a4f;align-items:center}
  .change-legend .sw{display:inline-block;width:14px;height:11px;border-radius:2px;margin-right:.35rem;vertical-align:-1px;
    border:1px solid rgba(0,0,0,.08)}
  .listing-tools{display:flex;flex-wrap:wrap;justify-content:space-between;align-items:center;gap:.4rem 1rem;
    margin-bottom:.5rem}
  .listing-tools .form-group{margin-bottom:0}
  .listing-tools .radio-inline,.listing-tools .form-check-inline{font-size:.84rem}
  mark.upd{background:#ffcf5c;padding:0 .2rem;border-radius:3px;cursor:help;color:inherit}
  tr.row-new td{background:#d8ebe3}
  tr.row-upd td{background:#fff0c2}
  tr.row-del td{background:#e6dfd3;text-decoration:line-through}
  .js-plotly-plot .main-svg{background:transparent!important}
  table.dataTable>tbody>tr[style*=background-color]:not(.selected)>*{box-shadow:none!important;
    background-color:inherit!important}
  .form-changes + .form-changes{margin-top:1.4rem}
  .kb-hint{font-size:.75rem;color:#635a4f;margin-left:auto}
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
    document.querySelectorAll('.side-link').forEach(function(a) {
      var on = a.dataset.value === id;
      a.classList.toggle('active', on);
      var sec = on && a.closest('details.side-sec');
      if (sec) sec.open = true;
    });
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

# Headings reviewers read: each variable's label (see rtcrcore::column_labels())
# in place of its name. Hidden "." columns and unlabelled ones keep their name.
col_label <- function(var, study) {
  if (is.null(study$labels)) return(var)
  lab <- unname(study$labels[var])
  ifelse(is.na(lab) | startsWith(var, "."), var, lab)
}

label_columns <- function(d, study) {
  names(d) <- col_label(names(d), study)
  d
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
      dt <- DT::datatable(d, escape = FALSE, rownames = FALSE, selection = selection,
                          options = list(pageLength = page_length, scrollX = TRUE, order = list(),
                                         dom = if (nrow(d) > page_length) "ftip" else "ft",
                                         columnDefs = list(list(visible = FALSE, targets = hidden)),
                                         language = list(emptyTable = "No records.")))
      # Rows with a record status (.RECSTAT) take its colour: new, updated, deleted.
      if (".RECSTAT" %in% names(d)) {
        dt <- DT::formatStyle(dt, ".RECSTAT", target = "row",
                              backgroundColor = DT::styleEqual(names(change_colours), unname(change_colours)),
                              textDecoration = DT::styleEqual("Deleted", "line-through"))
      }
      dt
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
