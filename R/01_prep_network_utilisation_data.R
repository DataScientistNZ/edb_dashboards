library(data.table)
source(file.path(here::here(), "R", "00_edb_status.R"))

dt <- as.data.table(arrow::read_parquet(file.path(
  here::here(), "data", "EDB_ID__Full__2024.09.1.parquet")))
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

dt_pc <- merge(dt_peak, dt_capacity, all=T)

# add status
dt_pc <- merge(dt_pc, 
               get_edb_status()[edb == "Eastland Network", edb := "Firstlight Network"][],
               by="edb") 

fwrite(dt_pc, file.path(here::here(), "data", "utilisation_data.csv"))
