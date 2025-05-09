dt_all_edb <- dt[, .(nb_connections = sum(nb_connections),
                     veg_saidi_total = sum(veg_saidi * nb_connections),
                     veg_saifi_total = sum(veg_saifi * nb_connections),
                     overhead_length = sum(overhead_length), 
                     veg_mgt_opex = sum(veg_mgt_opex)), by="disc_yr"]
dt_all_edb[, mn_per_outage := veg_saidi_total/veg_saifi_total]

overall_mn_per_outage <- sum(dt_all_edb$veg_saidi_total)/sum(dt_all_edb$veg_saifi_total)
# 174 mn per interruption

price_mn_outage_1 <- 0.01
price_mn_outage_2 <- 0.15


dt_all_edb[, cost_outages_total_1 := price_mn_outage_1 * veg_saidi_total]
dt_all_edb[, cost_outages_total_2 := price_mn_outage_2 * veg_saidi_total]

dt_all_edb[, norm_cost_outages_1 := cost_outages_total_1/overhead_length]
dt_all_edb[, norm_cost_outages_2 := cost_outages_total_2/overhead_length]


dt_all_edb[, better_norm_cost_outages_1 := price_mn_outage_1 * veg_saifi_total * overall_mn_per_outage / overhead_length]
dt_all_edb[, better_norm_cost_outages_2 := price_mn_outage_2 * veg_saifi_total * overall_mn_per_outage / overhead_length]




################################################

# todo: include inflation early!!!
dt[, cost_outages_via_saidi_1 := veg_saidi * nb_connections * price_mn_outage_1]
dt[, cost_outages_via_saidi_2 := veg_saidi * nb_connections * price_mn_outage_2]

# dt[, norm_cost_outages_via_saidi_1 := veg_saidi * nb_connections * price_mn_outage_1 * overall_mn_per_outage / overhead_length]
# dt[, norm_cost_outages_via_saidi_2 := veg_saidi * nb_connections * price_mn_outage_2 * overall_mn_per_outage / overhead_length]


my_bar_gplot(dt, y_numerator="cost_outages_via_saidi_1", y_denominator="overhead_length", 
             groupby="PAT_peergroup", plot_disc_yr=latest_year)


my_bar_gplot(dt, y_numerator="cost_outages_via_saidi_2", y_denominator="overhead_length", 
             groupby="PAT_peergroup", plot_disc_yr=latest_year)


my_bar_gplot(dt, y_numerator="cost_outages_via_saidi_1", y_denominator="overhead_length", 
             groupby="PAT_peergroup", plot_disc_yr=overall_period)


my_bar_gplot(dt, y_numerator="cost_outages_via_saidi_2", y_denominator="overhead_length", 
             groupby="PAT_peergroup", plot_disc_yr=overall_period)






# todo: include inflation early!!!
dt[, cost_outages_via_saifi_1 := veg_saifi * nb_connections * price_mn_outage_1 * overall_mn_per_outage]
dt[, cost_outages_via_saifi_2 := veg_saifi * nb_connections * price_mn_outage_2 * overall_mn_per_outage]

dt[, norm_cost_outages_via_saifi_1 := veg_saifi * nb_connections * price_mn_outage_1 * overall_mn_per_outage / overhead_length]
dt[, norm_cost_outages_via_saifi_2 := veg_saifi * nb_connections * price_mn_outage_2 * overall_mn_per_outage / overhead_length]


my_bar_gplot(dt, y_numerator="cost_outages_via_saifi_1", y_denominator="overhead_length", 
             groupby="PAT_peergroup", plot_disc_yr=latest_year)


my_bar_gplot(dt, y_numerator="cost_outages_via_saifi_2", y_denominator="overhead_length", 
             groupby="PAT_peergroup", plot_disc_yr=latest_year)



dt[, veg_mgt_opex_w_saifi_cost_1 := veg_mgt_opex + cost_outages_via_saifi_1]
dt[, veg_mgt_opex_w_saifi_cost_2 := veg_mgt_opex + cost_outages_via_saifi_2]


my_bar_gplot(dt, y_numerator="veg_mgt_opex", y_denominator="overhead_length", 
             groupby="PAT_peergroup", plot_disc_yr=latest_year)

my_bar_gplot(dt, y_numerator="veg_mgt_opex_w_saifi_cost_1", y_denominator="overhead_length", 
             groupby="PAT_peergroup", plot_disc_yr=latest_year)

my_bar_gplot(dt, y_numerator="veg_mgt_opex", y_denominator="overhead_length", 
             groupby="PAT_peergroup", plot_disc_yr=latest_year)

my_bar_gplot(dt, y_numerator="veg_mgt_opex_w_saifi_cost_2", y_denominator="overhead_length", 
             groupby="PAT_peergroup", plot_disc_yr=latest_year) 


#############################################################

price_mn_outage_2 <- 0.15
dt[, cost_outages_via_saifi_1 := veg_saifi * nb_connections * price_mn_outage_1 * overall_mn_per_outage]
dt[, cost_outages_via_saifi_2 := veg_saifi * nb_connections * price_mn_outage_2 * overall_mn_per_outage]
dt[, norm_cost_outages_via_saifi_1 := veg_saifi * nb_connections * price_mn_outage_1 * overall_mn_per_outage / overhead_length]
dt[, norm_cost_outages_via_saifi_2 := veg_saifi * nb_connections * price_mn_outage_2 * overall_mn_per_outage / overhead_length]

my_group <- unique(dt$PAT_peergroup)[4]
display_var <- c("veg_mgt_opex", "cost_outages_via_saifi_2")
my_disc_yr <- 2024

names(display_var) <- c("normalised veg. mgt cost", "quality penalty")

keep_var <- c("veg_mgt_opex", "cost_outages_via_saifi_1", "cost_outages_via_saifi_2", 
              "cost_outages_via_saidi_1", "cost_outages_via_saidi_2")
qual_vars <- c("edb", "disc_yr", "PAT_peergroup")

# Reshape to long format
dt_long <- melt(dt, id.vars = qual_vars, measure.vars = keep_var, 
                variable.name = "desc", value.name = "value")

dt_long <- merge(dt_long, dt[, c(qual_vars, "overhead_length"), with=F], by=qual_vars)

dt_long[, norm_value := value / overhead_length]

dt_long[, desc := factor(desc, levels = c(display_var[2], display_var[1]))]

# dt_long[desc %in% display_var & PAT_peergroup == my_group & disc_yr == my_disc_yr]
dt_long[desc %in% display_var & PAT_peergroup == my_group & disc_yr == my_disc_yr & edb %in% c("Firstlight Network", "Marlborough Lines"),
        .(norm_value = sum(norm_value)), by="edb"]


ggplot(dt_long[desc %in% display_var & PAT_peergroup == my_group & disc_yr == my_disc_yr], 
       aes(x = edb, y = norm_value, fill = desc)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Reverse the axes
  labs(title = "",
       y = " $ / km of overhead",
       x = "",
       fill = "",
       subtitle = paste0(my_group, "  -  ", my_disc_yr)) +
  scale_fill_manual(values = c("brown2", "cyan4"), 
                    labels = c("veg_mgt_opex" = "normalised veg. mgt expense",
                               "cost_outages_via_saifi_2" = "normalised cost outages (26$ per outage)")) +
  ggtitle("Normalised vegetation management cost benchmark") +
  theme_minimal() +
  theme(legend.position = "bottom")

dt_avg <- dt_long[, .(norm_value = mean(norm_value)), by=c("edb","PAT_peergroup", "desc")]
ggplot(dt_avg[desc %in% display_var & PAT_peergroup == my_group], 
       aes(x = edb, y = norm_value, fill = desc)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Reverse the axes
  labs(title = "",
       y = " $ / km of overhead",
       x = "",
       fill = "",
       subtitle = paste0(my_group, "  -  ", my_disc_yr)) +
  scale_fill_manual(values = c("brown2", "cyan4"), 
                    labels = c("veg_mgt_opex" = "normalised veg. mgt expense",
                               "cost_outages_via_saifi_2" = "normalised cost outages (26$ per outage)")) +
  ggtitle("Normalised vegetation management cost benchmark") +
  theme_minimal() +
  theme(legend.position = "bottom")


dt_long[desc %in% display_var & PAT_peergroup == my_group & disc_yr == my_disc_yr]

###############################################

my_group <- unique(dt$PAT_peergroup)[4]
display_var <- c("veg_mgt_opex", "cost_outages_via_saidi_2")
my_disc_yr <- 2023

names(display_var) <- c("normalised veg. mgt cost", "quality penalty")

keep_var <- c("veg_mgt_opex", "cost_outages_via_saifi_1", "cost_outages_via_saifi_2", 
              "cost_outages_via_saidi_1", "cost_outages_via_saidi_2")
qual_vars <- c("edb", "disc_yr", "PAT_peergroup")

# Reshape to long format
dt_long <- melt(dt, id.vars = qual_vars, measure.vars = keep_var, 
                variable.name = "desc", value.name = "value")

dt_long <- merge(dt_long, dt[, c(qual_vars, "overhead_length"), with=F], by=qual_vars)

dt_long[, norm_value := value / overhead_length]

dt_long[, desc := factor(desc, levels = c(display_var[2], display_var[1]))]


ggplot(dt_long[desc %in% display_var & PAT_peergroup == my_group & disc_yr == my_disc_yr], 
       aes(x = edb, y = norm_value, fill = desc)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Reverse the axes
  labs(title = "",
       y = " $ / km of overhead",
       x = "",
       fill = "",
       subtitle = paste0(my_group, "  -  ", my_disc_yr)) +
  scale_fill_manual(values = c("brown2", "cyan4"), 
                    labels = c("veg_mgt_opex" = "normalised veg. mgt expense",
                               "cost_outages_via_saidi_2" = "normalised cost outages (0.15$ per mn)")) +
  ggtitle("Normalised vegetation management cost benchmark") +
  theme_minimal() +
  theme(legend.position = "bottom")

dt_avg <- dt_long[, .(norm_value = mean(norm_value)), by=c("edb","PAT_peergroup", "desc")]
ggplot(dt_avg[desc %in% display_var & PAT_peergroup == my_group], 
       aes(x = edb, y = norm_value, fill = desc)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Reverse the axes
  labs(title = "",
       y = " $ / km of overhead",
       x = "",
       fill = "",
       subtitle = paste0(my_group, "  -  ", "[2013-2023]")) +
  scale_fill_manual(values = c("brown2", "cyan4"), 
                    labels = c("veg_mgt_opex" = "normalised veg. mgt expense",
                               "cost_outages_via_saidi_2" = "normalised cost outages (0.15$ per mn)")) +
  ggtitle("Normalised vegetation management cost benchmark") +
  theme_minimal() +
  theme(legend.position = "bottom")




# 0.54 * 0.28
# 
# 0.15 per mn <- cost_2
# 0.01 per mn <- cost_1




