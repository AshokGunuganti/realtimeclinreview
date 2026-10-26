# =============================================================================
#  Write SIMULATED raw source files in the shape real systems deliver them:
#    - EDC (Rave-style) form extracts with audit timestamps (CREATED / UPDATED)
#    - Blinded imaging vendor transfer (MCSS), with corrections as new rows
#    - Bioanalytical lab transfer (systemic Study drug), results as text
#    - EDC query log
#  Usage:  Rscript data-raw/simulate_sources.R            (from project root)
#
#  The files hold the full simulated timeline through EXTRACT_DT. The app's
#  "review as of" date filters on audit/transfer dates to emulate successive
#  weekly extracts. In production, each weekly extract simply replaces
#  data/source/ and review-as-of = extract date.
# =============================================================================
suppressPackageStartupMessages({ library(dplyr) })
source("data-raw/sim_engine.R")

OUT        <- "data/source"
STUDYID    <- "DERM-301-SIM"
EXTRACT_DT <- as.Date("2026-12-31")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

sim  <- simulate_study()
comp <- simulate_compliance(sim)
vs   <- read.csv("config/visit_schedule.csv", stringsAsFactors = FALSE)
set.seed(7)   # source-formatting randomness only (times, verbatims, coding lag)

# ---- formatting helpers (Rave CSV conventions) -------------------------------
rdate <- function(d) ifelse(is.na(d), "", toupper(format(d, "%d %b %Y")))
rdtm  <- function(d) ifelse(is.na(d), "",
  toupper(paste(format(d, "%d %b %Y"),
                sprintf("%02d:%02d:%02d", sample(8:18, length(d), TRUE),
                        sample(0:59, length(d), TRUE), sample(0:59, length(d), TRUE)))))
keep_to_extract <- function(df, col) df[is.na(df[[col]]) | df[[col]] <= EXTRACT_DT, , drop = FALSE]
write_src <- function(df, name) {
  write.csv(df, file.path(OUT, name), row.names = FALSE, na = "")
  data.frame(FILE = name, ROWS = nrow(df))
}
fold <- setNames(vs$FOLDER, vs$VISITNUM)
fnam <- setNames(vs$VISIT, vs$VISITNUM)
vvis <- setNames(vs$VENDORVIS, vs$VISITNUM)
man  <- list()

# ---- DM: demographics / enrollment (folder SCRN) ------------------------------
dm <- sim$subj |>
  transmute(STUDYID, SITEID = site, SUBJID = subjid, FOLDER = "SCRN", FOLDERNAME = "Screening",
            AGE = age, AGEU = "YEARS", SEX = sex, ENRLDAT = rdate(runin),
            created = runin, CREATED = rdtm(runin), UPDATED = rdtm(runin)) |>
  keep_to_extract("created") |> select(-created)
man$dm <- write_src(dm, "edc_dm.csv")

# ---- EX1: first application of study drug (folder DAY1) ----------------------
ex <- sim$subj |> filter(dosed) |>
  transmute(STUDYID, SITEID = site, SUBJID = subjid, FOLDER = "DAY1", FOLDERNAME = fnam[["2"]],
            EXTRT = "STUDY DRUG TOPICAL GEL", EXSTDAT = rdate(dose_date),
            created = dose_date, CREATED = rdtm(dose_date), UPDATED = rdtm(dose_date)) |>
  keep_to_extract("created") |> select(-created)
man$ex <- write_src(ex, "edc_ex1.csv")

# ---- DS: disposition (only subjects who discontinued) ------------------------
ds <- sim$subj |> filter(!is.na(disc_date)) |>
  transmute(STUDYID, SITEID = site, SUBJID = subjid, FOLDER = "DISP", FOLDERNAME = "Disposition",
            DSPHASE = "RUN-IN", DSDECOD = "WITHDRAWAL BY SUBJECT", DSSTDAT = rdate(disc_date),
            CREATED = rdtm(disc_date), UPDATED = rdtm(disc_date))
man$ds <- write_src(ds, "edc_ds.csv")

# ---- SV: visit date / visit not done ------------------------------------------
v <- sim$vis
sv <- bind_rows(
  v |> filter(!missed) |>
    transmute(SUBJID = subjid, SITEID = site, VISITNUM = visitnum, VISDAT = rdate(vdate), VISND = "",
              VISNDRS = "", created = entry),
  v |> filter(missed) |>
    transmute(SUBJID = subjid, SITEID = site, VISITNUM = visitnum, VISDAT = "", VISND = "Y",
              VISNDRS = sample(c("SUBJECT UNAVAILABLE", "SCHEDULING CONFLICT", "ILLNESS"), n(), TRUE),
              created = target)          # site records the missed visit on the target date
) |>
  keep_to_extract("created") |>
  arrange(SUBJID, VISITNUM) |>
  transmute(STUDYID, SITEID, SUBJID, FOLDER = fold[as.character(VISITNUM)],
            FOLDERNAME = fnam[as.character(VISITNUM)], VISDAT, VISND, VISNDRS,
            CREATED = rdtm(created), UPDATED = CREATED)
man$sv <- write_src(sv, "edc_sv.csv")

# ---- IGA: investigator global assessment (post-baseline folders) -------------
iga_lab <- c("-3" = "VERY MUCH WORSE", "-2" = "MUCH WORSE", "-1" = "MINIMALLY WORSE", "0" = "NO CHANGE",
             "1" = "MINIMALLY IMPROVED", "2" = "MUCH IMPROVED", "3" = "VERY MUCH IMPROVED")
iga <- v |> filter(!missed, !is.na(iga)) |>
  keep_to_extract("entry") |>
  transmute(STUDYID, SITEID = site, SUBJID = subjid, FOLDER = fold[as.character(visitnum)],
            FOLDERNAME = fnam[as.character(visitnum)], IGADAT = rdate(vdate),
            IGASCORE = as.integer(iga), IGATXT = iga_lab[as.character(iga)],
            CREATED = rdtm(entry), UPDATED = CREATED)
man$iga <- write_src(iga, "edc_iga.csv")

# ---- Imaging vendor: blinded central MCSS (corrections arrive as new rows) ----
img_base <- v |> filter(!missed) |>
  transmute(SUBJID = subjid, VISITNUM = visitnum, PHOTODT = vdate, entry, mod, mcss, mcss_prev)
img <- bind_rows(
  img_base |> transmute(SUBJID, VISITNUM, PHOTODT, TRANSDT = entry,
                        MCSS = if_else(is.na(mod), mcss, mcss_prev), RECSTAT = "ORIGINAL"),
  img_base |> filter(!is.na(mod)) |>
    transmute(SUBJID, VISITNUM, PHOTODT, TRANSDT = mod, MCSS = mcss, RECSTAT = "CORRECTED")
) |>
  keep_to_extract("TRANSDT") |>
  arrange(SUBJID, VISITNUM, TRANSDT) |>
  transmute(STUDYID, SUBJID, VISIT = vvis[as.character(VISITNUM)],
            PHOTODT = format(PHOTODT, "%Y-%m-%d"),
            READER = sample(c("R01", "R02", "R03"), n(), TRUE),
            READDT = format(TRANSDT - sample(0:2, n(), TRUE), "%Y-%m-%d"),
            MCSS_TOTAL = sprintf("%.1f", MCSS), RECSTAT,
            TRANSDT = format(TRANSDT, "%Y-%m-%d"))
man$img <- write_src(img, "vendor_imaging_mcss.csv")

# ---- AE: adverse events (MedDRA coded; coding lags entry) --------------------
verb <- list(
  "Application site irritation" = c("irritation at application site", "skin irritation where gel applied"),
  "Application site pruritus"   = c("itching at application site", "itchy skin at gel site"),
  "Application site erythema"   = c("redness at application site", "erythema where gel applied"),
  "Acne"                        = c("acne", "acne on treated area"),
  "Dermatitis contact"          = c("contact dermatitis", "rash from contact"),
  "Nasopharyngitis"             = c("common cold", "cold symptoms"),
  "Upper respiratory tract infection" = c("URI", "upper respiratory infection"),
  "Headache" = c("headache"), "Pyrexia" = c("fever"), "Cellulitis" = c("cellulitis of leg", "cellulitis"),
  "COVID-19" = c("covid infection", "COVID-19"), "Arthralgia" = c("joint pain"))
ae <- sim$ae |>
  keep_to_extract("entry") |>
  mutate(
    AETERM  = toupper(vapply(pt, function(p) sample(verb[[p]], 1), "")),
    codedt  = entry + sample(1:8, n(), TRUE),
    AEREL   = if_else(related, sample(c("POSSIBLY RELATED", "RELATED"), n(), TRUE),
                               sample(c("NOT RELATED", "UNLIKELY RELATED"), n(), TRUE)),
    AEOUT   = sample(c("RECOVERED/RESOLVED", "RECOVERING/RESOLVING", "NOT RECOVERED/NOT RESOLVED"), n(), TRUE,
                     prob = c(.7, .15, .15)),
    upd     = if_else(!is.na(mod) & mod <= EXTRACT_DT, mod, entry)) |>
  arrange(subjid, aeseq) |>
  transmute(STUDYID, SITEID = site, SUBJID = subjid, FOLDER = "LOGAE", FOLDERNAME = "Adverse Events",
            AESPID = aeseq, AETERM, AEDECOD = pt, AESOC = soc, AECODDT = rdate(codedt),
            AESTDAT = rdate(onset), AESEV = toupper(sev), AEREL, AESER = if_else(serious, "Y", "N"), AEOUT,
            CREATED = rdtm(entry), UPDATED = if_else(upd > entry, rdtm(upd), CREATED),
            AUDITNOTE = if_else(upd > entry, toupper(modtext), ""))
man$ae <- write_src(ae, "edc_ae.csv")

# ---- Dosing diary reconciliation (post-baseline folders) ---------------------
dry <- comp |>
  left_join(v |> select(subjid, visitnum, vdate, entry, dose_date), by = c("subjid", "visitnum")) |>
  arrange(subjid, visitnum) |>
  group_by(subjid) |>
  mutate(start = lag(vdate, default = first(dose_date))) |>
  ungroup() |>
  keep_to_extract("entry") |>
  transmute(STUDYID, SITEID = substr(subjid, 1, 3), SUBJID = subjid, FOLDER = fold[as.character(visitnum)],
            FOLDERNAME = fnam[as.character(visitnum)], DRYSTDAT = rdate(start), DRYENDAT = rdate(vdate),
            DOSPRESC = days, DOSMISS = missed_doses, CREATED = rdtm(entry), UPDATED = CREATED)
man$dry <- write_src(dry, "edc_diary.csv")

# ---- Bioanalytical lab: systemic Study drug ------------------------------------
pk <- sim$pk |>
  keep_to_extract("entry") |>
  transmute(STUDYID, SUBJID = subjid, VISIT = toupper(visit),
            PCDTC = paste0(format(sdate, "%Y-%m-%d"), "T", sprintf("%02d:%02d", sample(7:11, n(), TRUE), sample(0:59, n(), TRUE))),
            PCTEST = "STUDY DRUG", PCSPEC = "WHOLE BLOOD",
            PCORRES = if_else(conc < LLOQ, sprintf("<%.3f", LLOQ), sprintf("%.3f", conc)),
            PCORRESU = "ng/mL", PCLLOQ = LLOQ, TRANSDT = format(entry, "%Y-%m-%d"))
man$pk <- write_src(pk, "vendor_pk.csv")

# ---- Query log -------------------------------------------------------------------
vnum <- setNames(vs$VISITNUM, vs$VISIT)
q <- sim$q |>
  mutate(is_ae = grepl("^AE #", visit),
         VISITNUM = ifelse(is_ae, NA_integer_, unname(vnum[visit])),
         AESPID   = ifelse(is_ae, suppressWarnings(as.integer(sub("AE #", "", visit))), NA_integer_)) |>
  keep_to_extract("opened") |>
  mutate(answered = if_else(!is.na(answered) & answered <= EXTRACT_DT, answered, as.Date(NA)),
         closed   = if_else(!is.na(closed) & closed <= EXTRACT_DT, closed, as.Date(NA))) |>
  transmute(STUDYID, SITEID = site, SUBJID = subjid, QRYID = qid,
            FORM = if_else(is_ae, "AE", "SV"),
            FOLDER = if_else(is_ae, "LOGAE", fold[as.character(VISITNUM)]),
            AESPID, QRYTEXT = text,
            QRYSTATUS = case_when(!is.na(closed) ~ "CLOSED", !is.na(answered) ~ "ANSWERED", TRUE ~ "OPEN"),
            OPENDT = rdtm(opened), ANSWERDT = rdtm(answered), CLOSEDT = rdtm(closed))
man$q <- write_src(q, "edc_queries.csv")

# ---- Manifest -------------------------------------------------------------------
manifest <- bind_rows(man) |>
  mutate(SOURCE = c("EDC", "EDC", "EDC", "EDC", "EDC", "Imaging vendor", "EDC", "EDC", "Bioanalytical lab", "EDC"),
         STUDYID = STUDYID, EXTRACT_DT = format(EXTRACT_DT, "%Y-%m-%d"),
         NOTE = "SIMULATED DATA - not actual trial data")
write.csv(manifest, file.path(OUT, "manifest.csv"), row.names = FALSE)
print(manifest[, c("FILE", "SOURCE", "ROWS")])
