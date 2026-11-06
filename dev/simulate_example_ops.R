# Adds the operational EDC reports to the example extract. All data SIMULATED.
#
# - edc_queries.csv: the query report, taken from the prototype simulation
#   (same simulated study, so every query points at an existing record).
# - edc_formstatus.csv: one row per subject, folder and form with the dates of
#   source data verification (SDV), PI signature, freeze and lock. Monitors
#   visit each site every 5 to 9 weeks and verify what was entered before.
# - edc_ae.csv: MedDRA version of each code (29.0 before September 2026, 29.1
#   after), and one verbatim term coded differently from the same term
#   elsewhere, for the coding review display.
#
# Deterministic; run from the repository root:
#   Rscript dev/simulate_example_ops.R

ext <- "studies/example/extracts/2026-12-31"
extract_date <- as.Date("2026-12-31")
set.seed(301)
invisible(Sys.setlocale("LC_TIME", "C"))

rd <- function(f) utils::read.csv(file.path(ext, f), stringsAsFactors = FALSE, colClasses = "character")
# Quote text columns only, as the EDC export does.
wr <- function(x, f, num = character()) {
  utils::write.csv(x, file.path(ext, f), row.names = FALSE, na = "", quote = which(!names(x) %in% num))
}
as_dt <- function(x) as.Date(x, "%d %b %Y")
fmt_d <- function(d) ifelse(is.na(d), NA_character_, toupper(format(d, "%d %b %Y")))
fmt_dt <- function(d) ifelse(is.na(d), NA_character_,
                             paste(toupper(format(d, "%d %b %Y")),
                                   sprintf("%02d:%02d:%02d", sample(8:18, length(d), TRUE),
                                           sample(0:59, length(d), TRUE), sample(0:59, length(d), TRUE))))

# ---- Queries -----------------------------------------------------------------
q <- utils::read.csv("prototype/data/source/edc_queries.csv", stringsAsFactors = FALSE, colClasses = "character")
q <- q[as_dt(q$OPENDT) <= extract_date, ]
wr(q, "edc_queries.csv", num = "AESPID")
open_q <- q[q$QRYSTATUS != "CLOSED", ]

# ---- AE coding ---------------------------------------------------------------
ae <- rd("edc_ae.csv")
ae$MEDDRAV <- ifelse(is.na(as_dt(ae$AECODDT)), "", ifelse(as_dt(ae$AECODDT) < as.Date("2026-09-01"), "29.0", "29.1"))
i <- which(ae$SUBJID == "112-006" & ae$AESPID == "2")      # ITCHY SKIN AT GEL SITE
ae$AEDECOD[i] <- "Pruritus"
ae$AESOC[i] <- "Skin and subcutaneous tissue disorders"
ae <- ae[c(setdiff(names(ae), c("MEDDRAV", "CREATED", "UPDATED", "AUDITNOTE")), "MEDDRAV", "CREATED", "UPDATED", "AUDITNOTE")]
wr(ae, "edc_ae.csv", num = "AESPID")

# ---- Form status -------------------------------------------------------------
forms <- c(DM = "edc_dm.csv", EX = "edc_ex1.csv", DS = "edc_ds.csv", SV = "edc_sv.csv",
           IGA = "edc_iga.csv", AE = "edc_ae.csv", DIARY = "edc_diary.csv")
inst <- do.call(rbind, lapply(names(forms), function(f) {
  x <- rd(forms[[f]])
  entry <- as_dt(x$CREATED)
  last <- pmax(entry, as_dt(x$UPDATED))
  # A log form is one form instance per subject: first entered with its first
  # row, last changed with its latest.
  g <- unique(x[c("STUDYID", "SITEID", "SUBJID", "FOLDER", "FOLDERNAME")])
  k <- match(paste(x$SUBJID, x$FOLDER), paste(g$SUBJID, g$FOLDER))
  g$FORM <- f
  g$FIRST <- as.Date(tapply(entry, k, min), origin = "1970-01-01")
  g$LAST <- as.Date(tapply(last, k, max), origin = "1970-01-01")
  g
}))
inst <- inst[order(inst$SITEID, inst$SUBJID, inst$FORM, inst$FOLDER), ]

# Monitoring visits per site, every 5 to 9 weeks from about a month after the
# site's first entry.
mon <- lapply(split(inst$FIRST, inst$SITEID), function(d) {
  v <- min(d) + sample(20:40, 1)
  while (utils::tail(v, 1) < extract_date) v <- c(v, utils::tail(v, 1) + sample(35:63, 1))
  v[v <= extract_date]
})
next_visit <- function(site, after) {
  v <- mon[[site]][mon[[site]] >= after]
  if (length(v)) v[1] else as.Date(NA)
}

n <- nrow(inst)
has_open_q <- paste(inst$SUBJID, inst$FORM, inst$FOLDER) %in% paste(open_q$SUBJID, open_q$FORM, open_q$FOLDER)
sdv <- as.Date(vapply(seq_len(n), function(k) {
  # Most forms are verified at the first visit after entry; some slip to the next.
  d <- next_visit(inst$SITEID[k], inst$LAST[k] + 3)
  if (!is.na(d) && stats::runif(1) < 0.12) d <- next_visit(inst$SITEID[k], d + 1)
  as.numeric(d)
}, numeric(1)), origin = "1970-01-01")
sign <- sdv + sample(2:21, n, TRUE)
sign[is.na(sdv) | has_open_q | stats::runif(n) > 0.85] <- NA
early <- inst$FOLDER %in% c("SCRN", "RUNIN", "DAY1", "WK04", "DISP")
freeze <- sign + sample(5:30, n, TRUE)
freeze[is.na(sign) | !early | stats::runif(n) > 0.6] <- NA
lock <- rep(as.Date(NA), n)

# Subject 109-001 withdrew in run-in: all forms verified, signed, frozen and locked.
w <- inst$SUBJID == "109-001"
sdv[w] <- inst$LAST[w] + 30
sign[w] <- sdv[w] + 5
freeze[w] <- sign[w] + 10
lock[w] <- freeze[w] + 14

cap <- function(d) { d[!is.na(d) & d > extract_date] <- NA; d }
sdv <- cap(sdv); sign <- cap(sign); freeze <- cap(freeze); lock <- cap(lock)
sign[is.na(sdv)] <- NA; freeze[is.na(sign)] <- NA; lock[is.na(freeze)] <- NA
upd <- pmax(inst$LAST, sdv, sign, freeze, lock, na.rm = TRUE)

fs <- data.frame(STUDYID = inst$STUDYID, SITEID = inst$SITEID, SUBJID = inst$SUBJID, FOLDER = inst$FOLDER,
                 FOLDERNAME = inst$FOLDERNAME, FORM = inst$FORM, SDVDT = fmt_d(sdv), SIGNDT = fmt_d(sign),
                 FREEZEDT = fmt_d(freeze), LOCKDT = fmt_d(lock), CREATED = fmt_dt(inst$FIRST), UPDATED = fmt_dt(upd),
                 stringsAsFactors = FALSE)
wr(fs, "edc_formstatus.csv")

# ---- Manifest ----------------------------------------------------------------
man <- rd("manifest.csv")
man <- man[!man$FILE %in% c("edc_queries.csv", "edc_formstatus.csv"), ]
note <- man$NOTE[1]
man <- rbind(man, data.frame(FILE = c("edc_queries.csv", "edc_formstatus.csv"), ROWS = c(nrow(q), nrow(fs)),
                             SOURCE = "EDC", STUDYID = man$STUDYID[1], EXTRACT_DT = man$EXTRACT_DT[1], NOTE = note))
man$ROWS[man$FILE == "edc_ae.csv"] <- nrow(ae)
wr(man, "manifest.csv", num = "ROWS")

cat(sprintf("queries %d (%d open or answered), form status %d (SDV %d, signed %d, frozen %d, locked %d)\n",
            nrow(q), nrow(open_q), nrow(fs), sum(!is.na(sdv)), sum(!is.na(sign)), sum(!is.na(freeze)), sum(!is.na(lock))))
