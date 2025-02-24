library("data.table")
# library(ggplot2)
# source("R/00_inflation.R")
source("R/00_edb_status.R")
source("R/00_echarts.R")

options(scipen=999)

dt <- as.data.table(arrow::read_parquet(file.path("data", "EDB_ID__Full__2024.09.1.parquet")))

# rename as per new name in old data
# we should really do that in the published data...
dt[edb == "Eastland Network", edb := "Firstlight Network"]

# filter all data (we focus on data for overall EDB, not sub-network)
dt <- dt[network == "All"]

# not all data before 2013, simplifies quite a lot of things (particularly SAIDI...)
dt <- dt[disc_yr >= 2013]

all_years <- sort(unique(dt$disc_yr))

# below is arguably wrong
dt <- dt[disc_yr == obs_yr | disc_yr == fcast_yr]

unique(dt$schedule)


# extract total opex
dt_opex <- dt[schedule == "SCHEDULE 6b: REPORT ON OPERATIONAL EXPENDITURE FOR THE DISCLOSURE YEAR" & 
                category == "Operational expenditure"]
dt_opex[, opex := value * 1000]
dt_opex <- dt_opex[, c("disc_yr", "edb", "opex"), with=F]
stopifnot(nrow(dt_opex) == length(all_years) * 29)


# extract total capex
dt_capex <- dt[schedule == "SCHEDULE 6a: REPORT ON CAPITAL EXPENDITURE FOR THE DISCLOSURE YEAR" & 
                 category == "Capital expenditure" & 
                 sub_category == "Capital expenditure"]
dt_capex[, capex := value * 1000]
dt_capex <- dt_capex[, c("disc_yr", "edb", "capex"), with=F]
stopifnot(nrow(dt_capex) == length(all_years) * 29)


# extract rab
dt_rab <- dt[schedule == "SCHEDULE 4: REPORT ON VALUE OF THE REGULATORY ASSET BASE (ROLLED FORWARD)" & 
                 category == "Total opening RAB value" & sub_category == "RAB"]
dt_rab[, rab_open := value * 1000]
dt_rab <- dt_rab[, c("disc_yr", "edb", "rab_open"), with=F]
stopifnot(nrow(dt_rab) == length(all_years) * 29)


# extract depreciation
dt_deprec <- dt[schedule == "SCHEDULE 4: REPORT ON VALUE OF THE REGULATORY ASSET BASE (ROLLED FORWARD)" & 
               category == "Total depreciation" & sub_category == "RAB"]
dt_deprec[, deprec := value * 1000]
dt_deprec <- dt_deprec[, c("disc_yr", "edb", "deprec"), with=F]
stopifnot(nrow(dt_deprec) == length(all_years) * 29)


# extract line length
dt_line_length <- dt[schedule == "SCHEDULE 9c: REPORT ON OVERHEAD LINES AND UNDERGROUND CABLES" & 
                       sub_category == "Total circuit length (km)" & description == "Total circuit length (for supply)"]
dt_line_length[, line_length := value]
dt_line_length <- dt_line_length[, c("disc_yr", "edb", "line_length"), with=F]
stopifnot(nrow(dt_line_length) == length(all_years) * 29)


# extract nb of customers
dt_nb_connections <- dt[schedule == "SCHEDULE 8: REPORT ON BILLED QUANTITIES AND LINE CHARGE REVENUES" &
                        category=="Total" & description == "Average no. of ICPs in disclosure year"]
dt_nb_connections[, nb_connections := value]
dt_nb_connections <- dt_nb_connections[, c("disc_yr", "edb", "nb_connections"), with=F]
stopifnot(nrow(dt_nb_connections) == length(all_years) * 29)

# dt_tmp <- data.table(edb=sort(unique(dt$edb)))
# fwrite(dt_tmp, "data/edb_regulatorystatus_and_peergroup.csv")
# table(dt[startsWith(edb, "Eastland")]$disc_yr)

# load edb static data & initialise aggregation
dt_edb_info <- fread(file.path("data", "edb_regulatorystatus_and_peergroup.csv"))
dt_all <- merge(setorderv(unique(dt[, c("disc_yr", "edb"), with=F]), c("disc_yr", "edb"))[], 
                dt_edb_info, by="edb")

# below: merge all data together - perform basic check when adding one info
dt_list <- list(dt_opex, dt_capex, dt_rab, dt_deprec, dt_line_length, dt_nb_connections)
for (i in 1:length(dt_list)) {
  expected_nrow <- nrow(dt_all)
  dt_all <- merge(dt_all, dt_list[[i]], by = c("disc_yr", "edb"))
  stopifnot(nrow(dt_all) == expected_nrow)
}
dt_all

# add flow of capital services
# Care: methodological issue here: it's all done in nominal terms
# To fully make sense in the CEPA framework, should use:
# - opex: 0.4 LCI and 0.6 PPI    - flow_of_capital_services:  cgpi
# issue because once data aggregated, can't apply seperate inflation indices to its components
dt_all[, flow_capital_services := deprec + (rab_open + capex*0.5) * 0.056]

# add totex and totex_fcs
dt_all[, totex := opex + capex]
dt_all[, totex_fcs := opex + flow_capital_services]
dt_all[, density := nb_connections/line_length]

# add icp50_line50 normalisation
dt_all[, icp50_line50 := nb_connections^0.5 * line_length^0.5]




################################################################################
###        Artificially add network utilisation prepared elsewhere
###############################################################################

# load utilisation data
# this file is obtained by running 01_prep_network_utilisation_data.R
dt_pc <- fread(file.path(here::here(), "data", "utilisation_data.csv"))
dt_pc[edb=="Scanpower"]


# make a clean copy only using substations with both peak demand and capacity info
dt_pc_clean <- data.table(dt_pc[!(is.na(peak) | peak == 0 | capacity == 0 | is.na(capacity))])

# create datasets with aggregated substations (keep clean filter)
dt_agg_clean <- dt_pc_clean[, .(peak = sum(peak, na.rm = T), capacity = sum(capacity, na.rm = T)), 
                            by=c("edb", "disc_yr", "status")]
dt_agg_clean[, "network_utilisation" := peak / capacity]

# merge network utilisation in previous dataset
# care, we don't seem to have scanpower data for network utilisation at all... that sucks
dt_all <- merge(dt_all, dt_agg_clean[, c("edb", "disc_yr", "network_utilisation"), with=F], all.x=T)


###############################################################################
# Save overall prepared data
fwrite(dt_all, file.path("data", "generic_purpose_edb_data.csv"))


dt_all