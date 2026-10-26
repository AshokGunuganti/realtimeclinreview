# =============================================================================
#  Truth engine: generates the underlying simulated study (all data SIMULATED).
#  Used only by simulate_sources.R and the round-trip test; the app never reads it.
# =============================================================================

VISITS <- data.frame(
  visit    = c("Run-in (Wk -8)", "Baseline (Day 1)", "Week 4", "Week 8", "Week 12",
               "Week 16", "Week 20", "Week 24", "Week 36", "Week 48"),
  visitnum = 1:10,
  week     = c(-8, 0, 4, 8, 12, 16, 20, 24, 36, 48),
  window   = c(7, 0, 7, 7, 7, 7, 7, 7, 14, 14),
  stringsAsFactors = FALSE
)
VISIT_ORDER <- VISITS$visit
PK_WEEKS    <- c(4, 12, 24)
LLOQ        <- 0.1   # ng/mL
MIN_N       <- 3     # minimum n to plot a visit mean
PK_LIMIT    <- 2.0   # ng/mL systemic exposure threshold

simulate_study <- function(seed = 2026) {
  set.seed(seed)
  n     <- 51
  sites <- as.character(101:113)

  subj <- data.frame(
    site  = sample(sites, n, replace = TRUE),
    age   = sample(c(sample(3:5, 2), round(runif(n - 2, 6, 58)))),
    sex   = sample(c("F", "M"), n, replace = TRUE),
    runin = sort(as.Date("2025-12-01") + sample(0:260, n, replace = TRUE)),
    stringsAsFactors = FALSE
  ) |>
    group_by(site) |>
    mutate(subjid = paste0(site, "-", sprintf("%03d", row_number()))) |>
    ungroup() |>
    mutate(
      dosed     = TRUE,
      dose_date = runin + 56,
      resp      = rnorm(n, 2.6, 0.6),           # latent Week-24 IGA response
      mcss0     = round(runif(n, 9, 18), 1),       # severity at entry
      effect    = pmin(0.85, pmax(0, rnorm(n, 0.45, 0.18)))
    )
  nd <- sample(which(subj$age >= 6), 1)            # one withdrawal in run-in
  subj$dosed[nd] <- FALSE
  subj$dose_date[nd] <- NA
  subj$disc_date <- as.Date(NA)
  subj$disc_date[nd] <- subj$runin[nd] + 20
  subj$itt <- subj$dosed & subj$age >= 6

  # ---- Visits + efficacy (QS-like) -----------------------------------------
  vis <- tidyr::crossing(subjid = subj$subjid, VISITS) |>
    left_join(subj, by = "subjid") |>
    filter(dosed | visitnum == 1) |>
    arrange(subjid, visitnum)
  m <- nrow(vis)
  vis <- vis |>
    mutate(
      target = if_else(visitnum == 1, runin, dose_date + week * 7),
      oow    = runif(m) < 0.06 & visitnum > 2,
      offset = case_when(visitnum == 2 ~ 0,
                         oow ~ sample(c(-1, 1), m, TRUE) * sample(9:16, m, TRUE),
                         TRUE ~ round(rnorm(m, 0, 2.5))),
      missed = runif(m) < 0.03 & visitnum > 2,
      vdate  = if_else(missed, as.Date(NA), target + offset),
      lag    = round(rgamma(m, 1.5, scale = 3)) + if_else(runif(m) < 0.08, sample(15:30, m, TRUE), 0L),
      entry  = vdate + lag,
      frac   = 1 - exp(-pmax(week, 0) / 7),
      mcss   = round(pmax(0, if_else(week <= 0, mcss0 + rnorm(m, 0, 0.7),
                                     mcss0 * (1 - effect * frac) + rnorm(m, 0, 0.8))), 1),
      iga    = if_else(week > 0, pmin(3, pmax(-3, round(resp * frac + rnorm(m, 0, 0.45)))), NA_real_),
      mod    = if_else(runif(m) < 0.10, entry + sample(2:25, m, TRUE), as.Date(NA)),
      mcss_prev = if_else(!is.na(mod), mcss + sample(c(-2, -1, 1, 2), m, TRUE), NA_real_)
    ) |>
    select(subjid, site, age, itt, dosed, dose_date, visit, visitnum, week, window,
           target, offset, oow, missed, vdate, entry, mcss, iga, mod, mcss_prev)

  # ---- Adverse events (AE-like) --------------------------------------------
  terms <- data.frame(
    pt   = c("Application site irritation", "Application site pruritus",
             "Application site erythema", "Acne", "Dermatitis contact", "Nasopharyngitis",
             "Upper respiratory tract infection", "Headache", "Pyrexia", "Cellulitis",
             "COVID-19", "Arthralgia"),
    soc  = c(rep("General disorders and administration site conditions", 3),
             rep("Skin and subcutaneous tissue disorders", 2),
             rep("Infections and infestations", 2), "Nervous system disorders",
             "General disorders and administration site conditions",
             rep("Infections and infestations", 2),
             "Musculoskeletal and connective tissue disorders"),
    w    = c(10, 7, 6, 6, 3, 8, 7, 5, 3, 2, 2, 2),
    prel = c(.9, .85, .85, .6, .5, .02, .02, .1, .05, .1, 0, .05),
    stringsAsFactors = FALSE
  )
  dosed <- subj |> filter(dosed)
  k <- rpois(nrow(dosed), 1.4)
  ae <- dosed[rep(seq_len(nrow(dosed)), k), c("subjid", "site", "dose_date")]
  na <- nrow(ae)
  ti <- sample(seq_len(nrow(terms)), na, TRUE, prob = terms$w)
  ae <- ae |>
    mutate(
      aeseq   = ave(seq_len(na), subjid, FUN = seq_along),
      pt      = terms$pt[ti], soc = terms$soc[ti],
      day     = sample(1:336, na, TRUE),
      onset   = dose_date + day - 1,
      sev     = sample(c("Mild", "Moderate", "Severe"), na, TRUE, prob = c(.72, .24, .04)),
      serious = runif(na) < if_else(pt == "Cellulitis", .5, .01),
      related = runif(na) < terms$prel[ti] & !serious,
      lag     = round(rgamma(na, 1.5, scale = 3)),
      entry   = onset + lag,
      mod     = if_else(runif(na) < 0.2, entry + sample(3:30, na, TRUE), as.Date(NA)),
      modtext = sample(c("Severity updated", "Outcome updated to Recovered",
                         "Action taken updated", "Causality reassessed"), na, TRUE)
    )

  # ---- Systemic study drug PK (PC-like) -------------------------------------
  pk <- vis |>
    filter(week %in% PK_WEEKS, !missed) |>
    transmute(subjid, site, visit, week, sdate = vdate) |>
    mutate(
      conc   = pmin(1.35, rlnorm(n(), log(0.22), 0.75)),
      entry  = sdate + sample(14:28, n(), TRUE)          # bioanalytical turnaround
    )
  pk$conc[order(pk$entry)[6]] <- 1.68                      # one early sample near the limit
  pk <- pk |> mutate(blq = conc < LLOQ, result = if_else(blq, "<LLOQ", sprintf("%.2f", conc)))

  # ---- Queries --------------------------------------------------------------
  qv <- vis |> filter(!missed) |>
    transmute(subjid, site, visit, entry, form = "Efficacy / visit")
  qa <- ae |> transmute(subjid, site, visit = paste0("AE #", aeseq), entry, form = "Adverse event")
  q  <- bind_rows(qv, qa)
  q  <- q[runif(nrow(q)) < 0.17, ]
  nq <- nrow(q)
  q <- q |>
    mutate(
      qid      = sprintf("Q%04d", seq_len(nq)),
      text     = sample(c("Assessment missing for visit", "Visit date inconsistent with dosing diary",
                          "Value outside expected range", "Lesion photo not uploaded",
                          "AE onset date precedes first dose", "Concomitant medication missing stop date"),
                        nq, TRUE),
      opened   = entry + sample(1:6, nq, TRUE),
      answered = if_else(runif(nq) < 0.95, opened + round(rgamma(nq, 1.6, scale = 5)), as.Date(NA)),
      closed   = if_else(!is.na(answered) & runif(nq) < 0.97, answered + sample(1:7, nq, TRUE), as.Date(NA))
    )

  list(subj = subj, vis = vis, ae = ae, pk = pk, q = q)
}


# Simulated once-daily dosing diary compliance, reconciled at each post-baseline visit.
# Separate seed so the core study simulation is unchanged.
simulate_compliance <- function(sim, seed = 99) {
  set.seed(seed)
  subj <- sim$subj |> dplyr::filter(dosed)
  adh <- ifelse(runif(nrow(subj)) < 0.15, runif(nrow(subj), 0.55, 0.78), runif(nrow(subj), 0.86, 0.99))
  names(adh) <- subj$subjid
  sim$vis |>
    dplyr::filter(dosed, week > 0, !missed) |>
    dplyr::arrange(subjid, visitnum) |>
    dplyr::group_by(subjid) |>
    dplyr::mutate(days = as.integer(vdate - dplyr::lag(vdate, default = dplyr::first(dose_date)))) |>
    dplyr::ungroup() |>
    dplyr::mutate(days = pmax(days, 1L),
                  pct  = round(100 * pmin(1, pmax(0.3, adh[subjid] + rnorm(dplyr::n(), 0, 0.04)))),
                  missed_doses = round(days * (1 - pct / 100))) |>
    dplyr::select(subjid, visitnum, days, pct, missed_doses)
}
