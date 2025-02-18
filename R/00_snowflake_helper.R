#' Get user name.
#' From Chris H
#' Windows only.
user <- function() {
  strsplit(
    system("whoami /upn", intern = TRUE),
    "@",
    fixed = TRUE
  )[[1]][1]
}

#' Get database connection
#'  From Chris H
#' @param user username in the form \tt{first.last}.
#' @param db database, e.g. DATANALYTICS_DB_DEV
#' @param wh warehouse, e.g. DATAANALYTICS_WH_DEV
#' @param role AD group / role, e.g. GRP_AZURE_DA_NONPROD_REPORTINGANALYTS#'
#' @returns DBI-compliant database connection.
get_connection <- function(
    uid = user(),
    db = config::get(value = "db"),
    wh = config::get(value = "wh"),
    role = config::get(value = "role")) {
  res <- DBI::dbConnect(
    odbc::odbc(),
    .connection_string = sprintf("
    Driver=SnowflakeDSIIDriver;
    server=comcomdataplatform.australia-east.azure.snowflakecomputing.com;
    uid=%s@comcom.govt.nz;
    authenticator=externalbrowser;
    database=%s;
    warehouse=%s;
    role=%s;
    tracing=2;
    ", uid, db, wh, role)
  )
  
  res
}