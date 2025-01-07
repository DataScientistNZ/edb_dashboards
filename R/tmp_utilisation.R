library("data.table")
# library(ggplot2)
# source("R/00_inflation.R")
source("R/00_edb_status.R")
source("R/00_echarts.R")

options(scipen=999)


dt <- as.data.table(arrow::read_parquet(file.path("data", "EDB_ID__Full__2024.09.1.parquet")))
dt <- dt[network == "All"]

# not all data before 2013, simplifies quite a lot of things (particularly SAIDI...)
dt <- dt[disc_yr >= 2013]

# below is arguably wrong
dt <- dt[disc_yr == obs_yr | disc_yr == fcast_yr]

# rename EDBs as necessary
dt[edb == "Eastland Network", edb := "Firstlight Network"]

# focus on right schedule
dt <- dt[schedule == "SCHEDULE 12b: REPORT ON FORECAST CAPACITY" &
           description %in% c("Current Peak Load (MVA)", "Installed Firm Capacity (MVA)")]

col_keep <- c("edb", "disc_yr", "value", "sub_category")
dt_peak <- dt[description %in% c("Current Peak Load (MVA)")][, col_keep, with=F]
# dt_peak[, existing_peak_flag := 1]
setnames(dt_peak, "value", "peak")
dt_capacity <- dt[description %in% c("Installed Firm Capacity (MVA)")][, col_keep, with=F]
# dt_capacity[, existing_capacity_flag := 1]
setnames(dt_capacity, "value", "capacity")

dt_p <- merge(dt_peak, dt_capacity, all=T)

##### FILTER OUT PARTIAL INFO !?
missing_peak <- dt_p[(capacity == 0 | is.na(capacity)), 
                     .(missing_peak = sum(peak, na.rm=T)), by=c("edb", "disc_yr")]
nonmissing_peak <- dt_p[!(is.na(peak) | peak == 0 | capacity == 0 | is.na(capacity)), 
                        .(nonmissing_peak = sum(peak, na.rm=T)), by=c("edb", "disc_yr")]
dt_m <- merge(nonmissing_peak, missing_peak, all=T)[is.na(missing_peak), missing_peak := 0][
  is.na(nonmissing_peak), nonmissing_peak := 0]
dt_m[, missing_idx := round(missing_peak / (missing_peak + nonmissing_peak), 4)]
dt_m <- merge(dt_m, 
              get_edb_status()[edb == "Eastland Network", edb := "Firstlight Network"][],
              by="edb") 

field <- "missing_idx"
my_status <- "NonExempt"
eplot_line(dt_m[status == my_status], x = "disc_yr", y = field, groupby = "edb") |>
  e_legend(
    orient = 'vertical',
    right = 0,
    top = "middle"
  ) |>
  # e_y_axis(formatter = e_axis_formatter("percent", digits = 0)) |>
  e_grid(right = 200, left=50, bottom=30) |>
  e_title(paste0(field, " (", my_status, ")")) 

my_status <- "Exempt"
eplot_line(dt_m[status == my_status], x = "disc_yr", y = field, groupby = "edb") |>
  e_legend(
    orient = 'vertical',
    right = 0,
    top = "middle"
  ) |>
  e_grid(right = 200, left=50, bottom=30) |>
  e_title(paste0(field, " (", my_status, ")"))


# add status
dt_p <- merge(dt_p, 
              get_edb_status()[edb == "Eastland Network", edb := "Firstlight Network"][],
              by="edb") 

# make a clean copy only using substations with both peak demand and capacity info
dt_p_clean <- data.table(dt_p[!(is.na(peak) | peak == 0 | capacity == 0 | is.na(capacity))])


dt_p <- dt_p[, .(peak = sum(peak, na.rm = T), capacity = sum(capacity, na.rm = T)), 
             by=c("edb", "disc_yr", "status")]
dt_p[, "utilisation" := peak / capacity]

dt_p_clean <- dt_p_clean[, .(peak = sum(peak, na.rm = T), capacity = sum(capacity, na.rm = T)), 
                         by=c("edb", "disc_yr", "status")]
dt_p_clean[, "utilisation" := peak / capacity]


#### UTILISATION
field <- "utilisation"
my_status <- "NonExempt"
eplot_line(dt_p[status == my_status], x = "disc_yr", y = field, groupby = "edb") |>
  e_legend(
    orient = 'vertical',
    right = 0,
    top = "middle"
  ) |>
  e_grid(right = 200, left=50, bottom=30) |>
  e_title(paste0(field, " (", my_status, ")"))

my_status <- "Exempt"
eplot_line(dt_p[status == my_status], x = "disc_yr", y = field, groupby = "edb") |>
  e_legend(
    orient = 'vertical',
    right = 0,
    top = "middle"
  ) |>
  e_grid(right = 200, left=50, bottom=30) |>
  e_title(paste0(field, " (", my_status, ")"))


#### peak
field <- "peak"
my_status <- "NonExempt"
eplot_line(dt_p[status == my_status], x = "disc_yr", y = field, groupby = "edb") |>
  e_legend(
    orient = 'vertical',
    right = 0,
    top = "middle"
  ) |>
  e_grid(right = 200, left=50, bottom=30) |>
  e_title(paste0(field, " (", my_status, ")"))

my_status <- "Exempt"
eplot_line(dt_p[status == my_status], x = "disc_yr", y = field, groupby = "edb") |>
  e_legend(
    orient = 'vertical',
    right = 0,
    top = "middle"
  ) |>
  e_grid(right = 200, left=50, bottom=30) |>
  e_title(paste0(field, " (", my_status, ")"))


#### demand
field <- "capacity"
my_status <- "NonExempt"
eplot_line(dt_p[status == my_status], x = "disc_yr", y = field, groupby = "edb") |>
  e_legend(
    orient = 'vertical',
    right = 0,
    top = "middle"
  ) |>
  e_grid(right = 200, left=50, bottom=30) |>
  e_title(paste0(field, " (", my_status, ")"))

my_status <- "Exempt"
eplot_line(dt_p[status == my_status], x = "disc_yr", y = field, groupby = "edb") |>
  e_legend(
    orient = 'vertical',
    right = 0,
    top = "middle"
  ) |>
  e_grid(right = 200, left=50, bottom=30) |>
  e_title(paste0(field, " (", my_status, ")"))


# loads of capacity with 0
dt[edb=="Westpower"][disc_yr==2013][, c("edb", "disc_yr", "sub_category", "description", "value"), with=F]
dt[edb=="EA Networks"][disc_yr==2013][, c("edb", "disc_yr", "sub_category", "description", "value"), with=F]
