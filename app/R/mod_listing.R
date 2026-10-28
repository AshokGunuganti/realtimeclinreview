# Listing: any form at collection grain, as in the 2017 review workbook: each
# record coloured by its status since the earlier date (new, updated,
# deleted), changed values marked with their earlier value, unresolved EDC
# queries and review marks. Params: form, columns, show ("all" or "changes":
# the records shown at first), tools (FALSE hides the legend and the switch,
# for pages that show them once for several listings).

listing_ui <- function(id, params, study) {
  ns <- shiny::NS(id)
  shiny::tagList(
    if (!isFALSE(params$tools)) shiny::div(class = "listing-tools",
               change_legend(),
               shiny::radioButtons(ns("show"), NULL, c("All records" = "all", "New and updated only" = "changes"),
                                   selected = if (identical(params$show, "changes")) "changes" else "all", inline = TRUE)),
    table_card_ui(ns("tbl"), if (is.null(params$title)) params$form else params$title,
                  "Select rows to mark them reviewed. Colours compare with the 'changes since' date.", mark_type = "record")
  )
}

listing_server <- function(id, ctx, params) {
  shiny::moduleServer(id, function(input, output, session) {
    form <- params$form
    hide <- c("STUDYID", "SITEID", "SUBJID", "FORMOID", "RECID", "ENTRYDT", "UPDDT", "AVAILDT", "VERSION", "DELFL")
    changes <- shiny::reactive(form_changes(ctx$study, ctx$data(), form, ctx$since(), ctx$asof()))
    tbl <- shiny::reactive({
      fc <- changes()
      t <- fc$rows
      status <- fc$status
      show <- if (is.null(input$show)) params$show else input$show
      if (identical(show, "changes")) {
        keep <- status != "Unchanged"
        t <- t[keep, , drop = FALSE]
        status <- status[keep]
      }
      cols <- if (length(params$columns)) unlist(params$columns) else setdiff(names(t), hide)
      blank <- vapply(t[intersect(cols, c("FOLDER", "VISITNUM"))], function(v) all(is.na(v)), logical(1))
      cols <- setdiff(cols, names(blank)[blank])                 # log forms have no visit
      cells <- as.data.frame(lapply(t[cols], function(v) {
        v <- htmltools::htmlEscape(as.character(fmt_cell(v)))
        v[is.na(v)] <- ""
        v
      }), stringsAsFactors = FALSE, check.names = FALSE)
      cells <- mark_changed_cells(cells, t$RECID, fc$fields)
      h <- ctx$record_hashes(form)
      key <- paste0(form, ":", t$RECID)
      x <- data.frame(Subject = sid_link(t$SUBJID), Site = t$SITEID,
                      Status = badge(ifelse(status == "Unchanged", "", status_label(status))),
                      stringsAsFactors = FALSE, check.names = FALSE)
      x <- cbind(x, cells)
      q <- ctx$queries()
      if (!is.null(q)) {
        open <- q[q$STATUS != "Closed" & !is.na(q$TARGET), , drop = FALSE]
        n <- table(open$TARGET)[key]
        x$Queries <- ifelse(is.na(n), "", badge(sprintf("%d open", as.integer(n))))
      }
      x$Version <- t$VERSION
      x$.RECSTAT <- status
      dep <- h$DEPHASH[match(key, h$TARGET_KEY)]
      with_marks(cbind(x, mark_cols(key, dep, t$SUBJID, t$VERSION)), ctx, "record")
    })
    table_card_server("tbl", tbl, ctx, form, mark_type = "record", page_length = 15)
    list(n_changes = shiny::reactive(sum(changes()$status != "Unchanged")))
  })
}
