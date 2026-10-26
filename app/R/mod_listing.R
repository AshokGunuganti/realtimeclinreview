# Listing: any form at collection grain, with record status since the earlier
# date, unresolved queries from the EDC and review marks.

listing_ui <- function(id, params, study) {
  ns <- shiny::NS(id)
  table_card_ui(ns("tbl"), if (is.null(params$title)) params$form else params$title,
                "Select rows to mark them reviewed. Status compares with the 'changes since' date.", mark_type = "record")
}

listing_server <- function(id, ctx, params) {
  shiny::moduleServer(id, function(input, output, session) {
    form <- params$form
    hide <- c("STUDYID", "SITEID", "SUBJID", "FORMOID", "RECID", "ENTRYDT", "UPDDT", "AVAILDT", "VERSION")
    tbl <- shiny::reactive({
      t <- ctx$data()$tables[[form]]
      cols <- if (length(params$columns)) unlist(params$columns) else setdiff(names(t), hide)
      blank <- vapply(t[intersect(cols, c("FOLDER", "VISITNUM"))], function(v) all(is.na(v)), logical(1))
      cols <- setdiff(cols, names(blank)[blank])                 # log forms have no visit
      cl <- rtcrcore::change_log(ctx$study$tables[[form]], ctx$since(), ctx$asof())
      status <- cl$RECSTAT[match(t$RECID, cl$RECID)]
      h <- ctx$record_hashes(form)
      key <- paste0(form, ":", t$RECID)
      x <- data.frame(Subject = sid_link(t$SUBJID), Site = t$SITEID, Status = badge(status),
                      stringsAsFactors = FALSE, check.names = FALSE)
      for (c in cols) x[[c]] <- fmt_cell(t[[c]])
      q <- ctx$queries()
      if (!is.null(q)) {
        open <- q[q$STATUS != "Closed" & !is.na(q$TARGET), , drop = FALSE]
        n <- table(open$TARGET)[key]
        x$Queries <- ifelse(is.na(n), "", badge(sprintf("%d open", as.integer(n))))
      }
      x$Version <- t$VERSION
      with_marks(cbind(x, mark_cols(key, h$DEPHASH[match(key, h$TARGET_KEY)], t$SUBJID, t$VERSION)), ctx, "record")
    })
    table_card_server("tbl", tbl, ctx, form, mark_type = "record", page_length = 15)
  })
}
