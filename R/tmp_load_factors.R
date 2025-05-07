library("data.table")
source("R/00_inflation.R")
source("R/00_edb_status.R")

options(scipen=999)
library(ggplot2)
source("R/00_echarts.R")

# dt <- as.data.table(arrow::read_parquet(file.path("data", "EDB_ID__Full__2024.05.01.parquet")))
dt <- as.data.table(arrow::read_parquet(file.path("data", "EDB_ID__Full__2024.09.1.parquet")))
dt <- dt[network == "All"]

# not all data before 2013, simplifies quite a lot of things (particularly SAIDI...)
dt <- dt[disc_yr >= 2013]
dt <- dt[disc_yr <= 2023]

# below is arguably wrong
dt <- dt[disc_yr == obs_yr]

# rename EDBs as necessary
dt[edb == "Eastland Network", edb := "Firstlight Network"]

# dt <- merge(dt, get_edb_status()) # add status

unique(dt$edb)
unique(dt$schedule)


View(dt[edb == 'Wellington Electricity' & schedule == "SCHEDULE 12b: REPORT ON FORECAST CAPACITY" & disc_yr==2024])

# View(dt[edb=="Aurora Energy" & disc_yr == 2023 & schedule == "SCHEDULE 9e: REPORT ON NETWORK DEMAND" &
#      network == "All"])


# dt[edb=="Aurora Energy" & disc_yr == 2023 & schedule == "SCHEDULE 9e: REPORT ON NETWORK DEMAND" &
# network == "All" & description == "Load factor"]


dt_p <- data.table(dt[schedule == "SCHEDULE 9e: REPORT ON NETWORK DEMAND" &
                        description == "Load factor"])

# add status
dt_p <- merge(dt_p, 
              get_edb_status()[edb == "Eastland Network", edb := "Firstlight Network"][]) 


my_status <- "NonExempt"
eplot_line(dt_p[status == my_status], x = "disc_yr", y = "value", groupby = "edb") |>
  e_legend(
    orient = 'vertical',
    right = 0,
    top = "middle"
  ) |>
  e_grid(right = 200, left=50, bottom=30) |>
  e_title(paste0("Load factor (", my_status, ")"))



my_status <- "Exempt"
eplot_line(dt_p[status == my_status], x = "disc_yr", y = "value", groupby = "edb") |>
  e_legend(
    orient = 'vertical',
    right = 0,
    top = "middle"
  ) |>
  e_grid(right = 200, left=50, bottom=30) |>
  e_title(paste0("Load factor (", my_status, ")"))