library(data.table)

# load helper functions
source("R/00_snowflake_helper.R")

get_inflation_data <- function(inflation_label=NULL, quarter_filter=NULL, min_year=NULL) {
  
  # check inputs
  if (!is.null(inflation_label))
    stopifnot(inflation_label %in% c("cgpi_all", "cpi", "lci_egww", "lci_all", "ppi_all", "ppi_egww"))
  if (!is.null(quarter_filter))
    stopifnot(quarter_filter %in% 1:4)
  
  # assuming you have config.yml in parent dir
  con <- get_connection()
  
  # query the table you want e.g.
  query <- DBI::dbReadTable(con, DBI::Id(schema ="EDB_STAGING", table = "INDICES_QUARTERLY"))
  # query <- DBI::dbReadTable(con, DBI::Id(schema ="EDB_STAGING", table = "INDICES_ANNUAL"))
  DBI::dbDisconnect(con)
  
  # cast as a data.table, tidy up names, reorder data
  dt <- as.data.table(query)
  names(dt) <- tolower(names(dt))
  setorderv(dt)
  
  # remove unused columns
  dt[, unit_text := NULL][, title_text := NULL]
  
  # extract month from date to define quarter
  dt[, quarter := as.integer(substr(period_date, 6, 7)) / 3]
  dt[, year := as.integer(year(period_date))]
  
  if (!is.null(inflation_label)) dt <- dt[label == inflation_label]
  if (!is.null(min_year)) dt <- dt[year >= min_year]
  if (!is.null(quarter_filter)) dt <- dt[quarter == quarter_filter]
  
  setcolorder(dt, c("series_code", "label", "period_date", "quarter", "year",  "value"))
  dt[]
}

get_inflation_index <- function(inflation_label, quarter_filter=NULL, min_year=NULL) {
  dt <- get_inflation_data(inflation_label, quarter_filter=quarter_filter, min_year = min_year)
  setnames(dt, "value", "index")
  dt[, c("year", "quarter", "index")]
}

get_inflation_annual_rate <- function(inflation_label, quarter_filter=NULL, min_year=NULL) {
  dt <- get_inflation_index(inflation_label, quarter_filter=quarter_filter)
  dt <- merge(dt, dt[, .(year = year + 1, quarter, prev_index = index)], 
              by = c("year", "quarter"), all.x = TRUE)
  dt[, rate := (index - prev_index) / prev_index]
  
  # filter is applied after as we need on extra year of data potentially
  if (!is.null(min_year)) dt <- dt[year >= min_year]
  
  dt[, c("year", "quarter", "rate")]
}

get_inflation_real_factor <- function(inflation_label, quarter, base_year=NULL, min_year=NULL) {
  dt <- get_inflation_index(inflation_label, quarter=quarter, min_year = min_year)
  if (is.null(base_year)) base_year <- max(dt$year)
  nm <- paste0(tolower(inflation_label), "_real")
  dt[, (nm) := index[year==base_year]/index][, c("year", nm), with=F]
}

get_inflation_index("cpi")
get_inflation_index("cpi", quarter=3, min_year=2020)
get_inflation_real_factor("cpi", quarter=1)
get_inflation_real_factor("cpi", quarter=3, min_year=2010, base_year=2020)
get_inflation_annual_rate("cpi", min_year=2010)
get_inflation_annual_rate("cpi",quarter_filter=2)


get_inflation_index("lci_all", quarter=3, min_year=2020)
