# Data load: the programmer checks on loading and mapping the extract
# (structural tier), check spec rows the app could not read, and checks that
# failed to run. Kept off the review screens and the review workbook; the app
# shows a one-line banner on every screen when something here needs a look.

dataload_ui <- function(id, params, study) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::p(class = "note", paste("For programmers and data management. Reviewers see a banner at the top when",
                                   "anything here may leave data missing or a check not running.")),
    table_card_ui(ns("spec"), "Check spec", "Rows of the check spec the app could not use, and checks that could not run."),
    table_card_ui(ns("sum"), "Programmer checks", "Checks on loading and mapping the extract. Click a check to list its findings."),
    table_card_ui(ns("fnd"), "Findings")
  )
}

dataload_server <- function(id, ctx, params) {
  shiny::moduleServer(id, function(input, output, session) {
    spec <- shiny::reactive({
      p <- load_problems(ctx$study, ctx$data())
      data.frame(Where = p$WHERE, Check = p$ID, Problem = htmltools::htmlEscape(p$PROBLEM), stringsAsFactors = FALSE)
    })
    table_card_server("spec", spec, ctx, "Check spec problems", selection = "none")
    summary <- shiny::reactive({
      s <- ctx$data()$summary
      s <- s[s$TIER == "structural", , drop = FALSE]
      data.frame(ID = s$ID, Name = s$NAME, Timing = s$TIMING, Severity = badge(s$SEVERITY), Findings = s$N,
                 Result = badge(s$RESULT), stringsAsFactors = FALSE)
    })
    picked <- table_card_server("sum", summary, ctx, "Programmer checks", selection = "single")
    tbl <- shiny::reactive({
      f <- ctx$data()$findings
      f <- f[f$CHECKID %in% summary()$ID, , drop = FALSE]
      p <- picked()
      if (!is.null(p)) f <- f[f$CHECKID == p$ID, , drop = FALSE]
      data.frame(Check = f$CHECKID, Severity = badge(f$SEVERITY), Subject = sid_link(f$SUBJID), Form = f$FORMOID,
                 Record = htmltools::htmlEscape(f$RECID), Field = f$FIELD, Value = htmltools::htmlEscape(f$VALUE),
                 Message = htmltools::htmlEscape(f$MESSAGE), stringsAsFactors = FALSE)
    })
    table_card_server("fnd", tbl, ctx, "Programmer findings", page_length = 15, selection = "none")
  })
}

# Everything that may leave data missing or a check not running: check spec
# rows not used, checks that failed to run, and programmer checks with errors.
load_problems <- function(study, data) {
  sp <- study$check_spec$problems
  out <- data.frame(WHERE = if (is.null(sp)) character() else sprintf("Check spec, Checks row %d", sp$ROW),
                    ID = if (is.null(sp)) character() else sp$ID,
                    PROBLEM = if (is.null(sp)) character() else sp$PROBLEM, stringsAsFactors = FALSE)
  s <- data$summary
  bad <- if (is.null(s$ERROR)) logical(nrow(s)) else !is.na(s$ERROR)
  rbind(out, data.frame(WHERE = rep("Check could not run", sum(bad)), ID = s$ID[bad], PROBLEM = s$ERROR[bad],
                        stringsAsFactors = FALSE))
}

# One line for the top of every screen, or NULL when the load is clean.
load_banner <- function(study, data) {
  p <- nrow(load_problems(study, data))
  s <- data$summary
  err <- sum(s$N[s$TIER == "structural" & s$SEVERITY == "error"])
  if (!p && !err) return(NULL)
  parts <- c(if (err) sprintf("%d data load error(s): some data may be missing", err),
             if (p) sprintf("%d check(s) not running", p))
  paste0(paste(parts, collapse = "; "), ".")
}
