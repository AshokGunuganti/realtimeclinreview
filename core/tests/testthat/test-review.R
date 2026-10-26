skip_if_not_installed("RSQLite")

store <- function(env = parent.frame()) {
  con <- open_review_store(list(driver = "sqlite", path = ":memory:"))
  withr::defer(DBI::dbDisconnect(con), envir = env)
  con
}

finding <- function(key = "C-1|101-001|AE:101-001|1|AESEV", hash = "h1") {
  data.frame(FINDINGKEY = key, DEPHASH = hash, stringsAsFactors = FALSE)
}

test_that("the store is created once with its schema version", {
  con <- store()
  expect_true(all(c("store_info", "review_mark", "signoff", "audit_trail", "access_log") %in% DBI::dbListTables(con)))
  init_review_store(con)
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM store_info")$n, 1)
})

test_that("a file store persists between connections", {
  path <- tempfile(fileext = ".sqlite")
  con <- open_review_store(list(driver = "sqlite", path = basename(path)), base_dir = dirname(path))
  set_mark(con, "S1", "finding", "K1", "alice", "no action")
  DBI::dbDisconnect(con)
  con <- open_review_store(list(driver = "sqlite", path = path))
  on.exit(DBI::dbDisconnect(con))
  expect_equal(nrow(current_marks(con, "S1")), 1)
})

test_that("every table is append-only", {
  con <- store()
  set_mark(con, "S1", "finding", "K1", "alice", "no action")
  log_access(con, "S1", "alice", "view", "What's new")
  for (t in c("review_mark", "audit_trail", "access_log", "store_info")) {
    col <- if (t == "store_info") "VALUE" else "STUDYID"
    expect_error(DBI::dbExecute(con, paste("UPDATE", t, "SET", col, "= 'X'")), "append-only")
    expect_error(DBI::dbExecute(con, paste("DELETE FROM", t)), "append-only")
  }
})

test_that("marks are events; the latest one is current", {
  con <- store()
  id1 <- set_mark(con, "S1", "finding", "K1", "alice", "no action", dephash = "h1")
  set_mark(con, "S1", "finding", "K1", "bob", "query raised", "Asked site to confirm date", dephash = "h1")
  set_mark(con, "S1", "finding", "K2", "alice", "escalated", dephash = "h2")
  cur <- current_marks(con, "S1")
  expect_equal(cur$TARGET_KEY, c("K1", "K2"))
  expect_equal(cur$OUTCOME, c("query raised", "escalated"))
  expect_equal(cur$USERID[1], "bob")
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM review_mark")$n, 3)

  clear_mark(con, "S1", "finding", "K2", "alice", comment = "Marked in error")
  expect_equal(current_marks(con, "S1")$TARGET_KEY, "K1")
  expect_error(clear_mark(con, "S1", "finding", "K2", "alice"), "no mark to clear")
  expect_error(set_mark(con, "S1", "finding", "K3", "alice", "fine"), "outcome must be one of")
  expect_error(set_mark(con, "S1", "finding", "K3", "", "no action"), "user ID is required")
  expect_equal(nrow(current_marks(con, "S2")), 0)
})

test_that("a stale view cannot overwrite someone else's mark", {
  con <- store()
  id1 <- set_mark(con, "S1", "finding", "K1", "alice", "no action")
  set_mark(con, "S1", "finding", "K1", "bob", "escalated", expect_markid = id1)
  expect_error(set_mark(con, "S1", "finding", "K1", "alice", "no action", expect_markid = id1), "someone else")
  expect_equal(current_marks(con, "S1")$OUTCOME, "escalated")
})

test_that("a mark resets only when the values it depends on change", {
  con <- store()
  set_mark(con, "S1", "finding", "K1", "alice", "no action", dephash = "h1")
  items <- data.frame(FINDINGKEY = c("K1", "K2"), DEPHASH = c("h1", "h9"))
  a <- apply_marks(con, "S1", items)
  expect_equal(a$REVIEW_STATUS, c("reviewed", "not reviewed"))
  expect_equal(a$MARK_USER, c("alice", NA))
  items$DEPHASH[1] <- "h2"
  a <- apply_marks(con, "S1", items)
  expect_equal(a$REVIEW_STATUS[1], "needs re-review")
  expect_equal(a$MARK_OUTCOME[1], "no action")                # old mark still shown
})

test_that("record marks survive bookkeeping changes but not data changes", {
  con <- store()
  ae <- cdm_tbl("AE", c("101-001|1", "101-001|2"), c("101-001", "101-001"), AETERM = c("Rash", "Headache"),
                AESEV = c("MILD", "MILD"))
  h <- record_hashes(ae)
  expect_equal(h$TARGET_KEY, c("AE:101-001|1", "AE:101-001|2"))
  set_mark(con, "S1", "record", h$TARGET_KEY[1], "alice", "no action", dephash = h$DEPHASH[1], subjid = "101-001",
           version = 1)

  v2 <- ae
  v2$VERSION <- 2
  v2$UPDDT <- v2$AVAILDT <- as.Date("2026-04-01")             # new version, same data
  expect_equal(apply_marks(con, "S1", record_hashes(v2), "record")$REVIEW_STATUS, c("reviewed", "not reviewed"))
  v2$AESEV[1] <- "SEVERE"
  expect_equal(apply_marks(con, "S1", record_hashes(v2), "record")$REVIEW_STATUS[1], "needs re-review")
  v2$AESEV[1] <- NA                                           # missing is a change too
  expect_equal(apply_marks(con, "S1", record_hashes(v2), "record")$REVIEW_STATUS[1], "needs re-review")
})

test_that("sign-off needs a fresh re-authentication and records the signature", {
  con <- store()
  scope <- data.frame(FINDINGKEY = c("K2", "K1"), DEPHASH = c("h2", "h1"))
  expect_error(sign_off(con, "S1", "medical", "2026-10-22", scope, "alice", "Alice Smith", "Reviewed",
                        auth_at = Sys.time() - 600), "Re-authenticate")
  expect_error(sign_off(con, "S1", "medical", "2026-10-22", scope, "alice", "Alice Smith", "",
                        auth_at = Sys.time()), "meaning is required")
  h <- sign_off(con, "S1", "medical", "2026-10-22", scope, "alice", "Alice Smith", "Reviewed and approved",
                auth_at = Sys.time())
  s <- signoffs(con, "S1")
  expect_equal(s$SCOPE_HASH, h)
  expect_equal(s$SCOPE_N, 2)
  expect_equal(s$FULL_NAME, "Alice Smith")
  expect_match(s$SIGNED_AT, "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}\\.\\d{3}Z$")
  expect_equal(h, scope_hash(scope[2:1, ]))                   # order-independent
  scope$DEPHASH[1] <- "h3"
  expect_false(identical(h, scope_hash(scope)))
})

test_that("the last sign-off sets the default 'since' date", {
  con <- store()
  expect_equal(review_since(con, "S1", "medical", fallback = "2026-10-15"), as.Date("2026-10-15"))
  expect_error(review_since(con, "S1", "medical"), "no fallback")
  sign_off(con, "S1", "medical", "2026-10-22", finding(), "alice", "Alice Smith", "Reviewed", Sys.time())
  sign_off(con, "S1", "data management", "2026-10-29", finding(), "bob", "Bob Jones", "Reviewed", Sys.time())
  expect_equal(review_since(con, "S1", "medical", fallback = "2026-10-15"), as.Date("2026-10-22"))
  expect_equal(review_since(con, "S1", "medical", user = "bob", fallback = "2026-10-15"), as.Date("2026-10-15"))
})

test_that("the audit trail records every mark and sign-off with old and new values", {
  con <- store()
  set_mark(con, "S1", "finding", "K1", "alice", "no action")
  set_mark(con, "S1", "finding", "K1", "bob", "escalated", "Possible SAE")
  clear_mark(con, "S1", "finding", "K1", "bob")
  sign_off(con, "S1", "medical", "2026-10-22", finding(), "alice", "Alice Smith", "Reviewed", Sys.time())
  a <- audit_trail(con, "S1")
  expect_equal(a$EVENT, c("mark set", "mark set", "mark clear", "sign-off"))
  expect_equal(a$USERID, c("alice", "bob", "bob", "alice"))
  expect_true(is.na(a$OLD_VALUE[1]))
  expect_equal(a$OLD_VALUE[2], "{\"outcome\":\"no action\"}")
  expect_equal(a$NEW_VALUE[2], "{\"outcome\":\"escalated\",\"comment\":\"Possible SAE\"}")
  expect_equal(a$OLD_VALUE[3], a$NEW_VALUE[2])
  expect_match(a$NEW_VALUE[4], "scope_hash")
})

test_that("views and exports go to the access log", {
  con <- store()
  log_access(con, "S1", "alice", "export", "review_workbook.xlsx", "as of 2026-10-22")
  l <- access_log(con, "S1")
  expect_equal(l$OBJECT, "review_workbook.xlsx")
  expect_equal(nrow(audit_trail(con, "S1")), 0)
  expect_error(log_access(con, "S1", "alice", "print", "x"), "view or export")
})

test_that("compare_findings classifies new, changed, persisting and resolved", {
  old <- data.frame(FINDINGKEY = c("A", "B", "C"), DEPHASH = c("1", "2", "3"), CHECKID = "X")
  new <- data.frame(FINDINGKEY = c("A", "B", "D"), DEPHASH = c("1", "9", "4"), CHECKID = "X")
  cmp <- compare_findings(old, new)
  expect_equal(cmp$FINDINGKEY, c("A", "B", "D", "C"))
  expect_equal(cmp$FINDSTAT, c("Persisting", "Changed", "New", "Resolved"))
})
