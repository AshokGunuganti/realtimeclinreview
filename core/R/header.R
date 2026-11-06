#' Standard header of the collected data model
#'
#' Every collected data model (CDM) table starts with these variables, in this
#' order. The as-of engine and change log work off the header alone, so they
#' never need to know what a form contains. `FORMOID + RECID + VERSION` is
#' unique.
#'
#' @return Character vector of header variable names.
#' @export
#' @examples
#' cdm_header_vars()
cdm_header_vars <- function() {
  c("STUDYID", "SITEID", "SUBJID", "FORMOID", "FOLDER", "VISITNUM",
    "RECID", "ENTRYDT", "UPDDT", "AVAILDT", "VERSION")
}
