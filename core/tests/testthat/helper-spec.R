# A small valid spec, as data frames for the three sheets.
spec_frames <- function() {
  forms <- data.frame(
    FORMOID = c("DM", "PK"), FORMTYPE = c("subject", "vendor"), SOURCE = c("dm.csv", "pk.csv"),
    KEY = c("SUBJID", "SUBJID, VISIT"), VERSIONING = c(NA, "rows"), DESCRIPTION = c("Demographics", "PK results"))
  hdr <- function(form, src) data.frame(
    FORMOID = form,
    VARIABLE = c("STUDYID", "SITEID", "SUBJID", "ENTRYDT", "AVAILDT"),
    SOURCE_FIELDS = c("STUDYID", "SITEID", "SUBJID", src, src),
    TRANSFORM = c("direct", "direct", "direct", "date", "date"),
    ARGS = c(NA, NA, NA, "format=ISO", "format=ISO; type=datetime"),
    REQUIRED = c("Y", "Y", "Y", "Y", "Y"), NOTES = NA)
  pk <- hdr("PK", "TRANSDT")
  pk$TRANSFORM[2] <- "lookup"
  pk$SOURCE_FIELDS[2] <- "SUBJID"
  pk$ARGS[2] <- "table=dm.csv; by=SUBJID; value=SITEID"
  mapping <- rbind(
    data.frame(FORMOID = "DM", VARIABLE = NA, SOURCE_FIELDS = NA, TRANSFORM = "filter",
               ARGS = "field=SITEID; drop=999", REQUIRED = NA, NOTES = "Drop test site"),
    hdr("DM", "CREATED"),
    data.frame(FORMOID = "DM", VARIABLE = "SEXDCD", SOURCE_FIELDS = "SEX", TRANSFORM = "decode",
               ARGS = "codelist=SEX", REQUIRED = "N", NOTES = NA),
    pk,
    data.frame(FORMOID = "PK", VARIABLE = c("FOLDER", "VISITNUM", "PCSTRESN", "PCLABEL"),
               SOURCE_FIELDS = c("VISIT", "VISIT", "PCORRES", "PCTEST, PCORRESU"),
               TRANSFORM = c("direct", "visit", "number", "concat"),
               ARGS = c(NA, "by=VENDORVIS", "flag=PCBLQFL", "sep= "), REQUIRED = c("Y", "Y", "Y", "N"), NOTES = NA))
  codes <- data.frame(CODELIST = "SEX", CODE = c("M", "F"), DECODE = c("Male", "Female"))
  list(Forms = forms, Mapping = mapping, Codelists = codes)
}

write_spec <- function(frames = spec_frames()) {
  path <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(frames, path)
  path
}
