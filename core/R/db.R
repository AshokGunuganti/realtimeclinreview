# Database layer shared by the review store and the study database: one way
# to connect (SQLite for demos and tests, Postgres for teams) and one way to
# make tables append-only.

#' Connect to a study or review database
#'
#' @param config A `driver` (`sqlite` or `postgres`) plus `path` for SQLite,
#'   or connection fields (`dbname`, `host`, `port`, `user`, `password`) for
#'   Postgres. A relative SQLite path is resolved against `base_dir`.
#' @param base_dir Folder that relative paths are resolved against.
#' @param what Name of the setting, for error messages.
#' @return A DBI connection.
#' @noRd
db_connect <- function(config, base_dir = ".", what = "database") {
  driver <- if (is.null(config$driver)) "sqlite" else config$driver
  switch(driver,
    sqlite = {
      if (!requireNamespace("RSQLite", quietly = TRUE)) stop("The sqlite ", what, " needs RSQLite.", call. = FALSE)
      path <- config$path
      if (is.null(path)) stop(what, " needs a path for sqlite.", call. = FALSE)
      if (!grepl("^(/|[A-Za-z]:)", path) && path != ":memory:") path <- file.path(base_dir, path)
      DBI::dbConnect(RSQLite::SQLite(), path)
    },
    postgres = {
      if (!requireNamespace("RPostgres", quietly = TRUE)) stop("The postgres ", what, " needs RPostgres.", call. = FALSE)
      args <- config[intersect(names(config), c("dbname", "host", "port", "user", "password"))]
      do.call(DBI::dbConnect, c(list(getExportedValue("RPostgres", "Postgres")()), args))
    },
    stop(what, " driver must be sqlite or postgres.", call. = FALSE))
}

# Statements that make `tables` reject UPDATE and DELETE (and, on Postgres,
# TRUNCATE). In production also grant the application role only INSERT and
# SELECT, so this holds even if a trigger is dropped.
append_only_ddl <- function(tables, pg) {
  if (pg) {
    fn <- paste0(
      "CREATE OR REPLACE FUNCTION rtcr_append_only() RETURNS trigger AS $$ BEGIN ",
      "RAISE EXCEPTION '% is append-only', TG_TABLE_NAME; END; $$ LANGUAGE plpgsql")
    return(c(fn, unlist(lapply(tables, function(t) c(
      paste0("DROP TRIGGER IF EXISTS ", t, "_append_only ON ", t),
      paste0("CREATE TRIGGER ", t, "_append_only BEFORE UPDATE OR DELETE ON ", t,
             " FOR EACH ROW EXECUTE FUNCTION rtcr_append_only()"),
      paste0("DROP TRIGGER IF EXISTS ", t, "_no_truncate ON ", t),
      paste0("CREATE TRIGGER ", t, "_no_truncate BEFORE TRUNCATE ON ", t,
             " FOR EACH STATEMENT EXECUTE FUNCTION rtcr_append_only()"))))))
  }
  unlist(lapply(tables, function(t) vapply(c("UPDATE", "DELETE"), function(op) {
    paste0("CREATE TRIGGER IF NOT EXISTS ", t, "_no_", tolower(op), " BEFORE ", op, " ON ", t,
           " BEGIN SELECT RAISE(ABORT, '", t, " is append-only'); END")
  }, "")), use.names = FALSE)
}

# Auto-increment primary key column type for the connection's database.
id_column <- function(con) if (is_postgres(con)) "BIGSERIAL PRIMARY KEY" else "INTEGER PRIMARY KEY AUTOINCREMENT"

is_postgres <- function(con) inherits(con, "PqConnection")

# Queries are written once, SQLite style: unquoted upper-case names and `?`
# placeholders. Postgres folds unquoted names to lower case and numbers its
# placeholders, so these helpers translate on the way in and out.
db_sql <- function(con, sql) {
  if (!is_postgres(con)) return(sql)
  parts <- strsplit(paste0(sql, " "), "?", fixed = TRUE)[[1]]
  if (length(parts) == 1) return(sql)
  trimws(paste0(c(rbind(parts[-length(parts)], paste0("$", seq_len(length(parts) - 1))), parts[length(parts)]),
                collapse = ""), "right")
}

db_get <- function(con, sql, params = NULL) {
  out <- if (is.null(params)) DBI::dbGetQuery(con, db_sql(con, sql)) else
    DBI::dbGetQuery(con, db_sql(con, sql), params = params)
  if (is_postgres(con)) names(out) <- toupper(names(out))
  out
}

db_exec <- function(con, sql, params = NULL) {
  if (is.null(params)) DBI::dbExecute(con, db_sql(con, sql)) else
    DBI::dbExecute(con, db_sql(con, sql), params = params)
}

db_append <- function(con, table, df) {
  if (is_postgres(con)) names(df) <- tolower(names(df))
  DBI::dbAppendTable(con, table, df)
}

# Inserts one row and returns the value of its `id` column, from the same
# statement, so concurrent writers never see each other's IDs.
db_insert_id <- function(con, table, row, id) {
  sql <- paste0("INSERT INTO ", table, " (", paste(names(row), collapse = ", "), ") VALUES (",
                paste(rep("?", length(row)), collapse = ", "), ") RETURNING ", id)
  db_get(con, sql, params = unname(as.list(row)))[[1]]
}
