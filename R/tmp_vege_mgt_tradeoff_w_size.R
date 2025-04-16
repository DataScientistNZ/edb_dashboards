# load resources and set common variables
library(data.table)
library(ggplot2)
source(file.path(here::here(), "R", "00_adhoc_charts.R"))
options(scipen=999)

# load prepared data
dt <- fread(file.path(here::here(), "data", "generic_purpose_edb_data.csv"))

# define generic descriptors
latest_year <- max(dt$disc_yr)
overall_period <- paste0(min(dt$disc_yr), "-", latest_year)

# Add average of any metric per edb over period
time_ft <- "disc_yr"
qualitative_constant_ft <- c("edb", "status", "PAT_peergroup")
dt_edb_avg <- dt[, lapply(.SD, mean, na.rm = TRUE), by = qualitative_constant_ft, 
                 .SDcols = setdiff(names(dt), c(time_ft, qualitative_constant_ft))]

dt_edb_avg[, (time_ft) := overall_period]
setcolorder(dt_edb_avg, names(dt))

my_scatter_gplot(data.table(dt)[veg_saidi == 0, veg_saidi := 1], x_var="veg_saidi", y_numerator="veg_mgt_opex", 
                 y_denominator="line_length", groupby="PAT_peergroup", plot_disc_yr=latest_year, x_log_scale = T)

my_scatter_gplot(data.table(dt)[veg_saifi == 0, veg_saifi := 0.001], x_var="veg_saifi", y_numerator="veg_mgt_opex", 
                 y_denominator="line_length", groupby="PAT_peergroup", plot_disc_yr=latest_year)

my_scatter_gplot(data.table(dt)[veg_saifi == 0, veg_saifi := 0.001], x_var="veg_saifi", y_numerator="veg_mgt_opex", 
                 y_denominator="line_length", groupby="PAT_peergroup", plot_disc_yr=latest_year, x_log_scale = F)

my_scatter_gplot(data.table(dt)[veg_saifi == 0, veg_saifi := 0.001], x_var="veg_saifi", y_numerator="veg_mgt_opex", 
                 y_denominator="line_length", groupby="PAT_peergroup", plot_disc_yr=overall_period)

my_scatter_gplot(data.table(dt)[veg_saifi == 0, veg_saifi := 0.001], x_var="veg_saifi", y_numerator="veg_mgt_opex", 
                 y_denominator="line_length", groupby="PAT_peergroup", plot_disc_yr=overall_period, x_log_scale = F)


# simplified version of the scatter plot that does not make as many assumptions
# was necessary to deal with modifying obtained plot
my_vsimple_scatter_gplot <- function(dt, x_var, y_var, groupby, 
                                    title=NULL, x_log_scale=T, size_var=NULL) {
  
  p <- ggplot(dt, aes(x = get(x_var), y = get(y_var)))
  if (is.null(size_var)) {
    p <- p + geom_point(aes(color=get(groupby)), size=2, alpha=0.8)
  } else {
    p <- p + geom_point(aes(color=get(groupby), size=get(size_var)), alpha=0.8)
  }
  p <- p + theme_minimal() +
    ylim(c(0, NA)) + 
    xlab(x_var) +
    ggrepel::geom_text_repel(aes(label=edb, color=get(groupby)), size=3, alpha=0.8, show.legend=FALSE) +
    labs(title = "geom_text_repel()", alpha=0.8) +
    theme(legend.title = element_blank())
  if (!x_var %in% c("density") & x_log_scale) p <- p + coord_trans(x='log10')

  p <- p + ylab(paste0(y_var))
  
  if (!is.null(title)) {
    p <- p + ggtitle(title)
  } else {
    p <- p + ggtitle(paste0(y_var))
  }
  
  p
}

dt_plot <- data.table(dt)[veg_saifi == 0, veg_saifi := 0.001][, "veg_mgt_opex/overhead_length" := veg_mgt_opex/overhead_length][
  , "veg_mgt_opex/icp" := veg_mgt_opex/nb_connections]
my_vsimple_scatter_gplot(dt_plot[disc_yr == latest_year], 
                         x_var="veg_saifi", y_var="veg_mgt_opex/overhead_length", 
                         groupby="PAT_peergroup", size_var = "veg_mgt_opex/icp")

dt_plot <- data.table(dt_edb_avg)[veg_saifi == 0, veg_saifi := 0.001][, "veg_mgt_opex / overhead_length" := veg_mgt_opex/overhead_length][
  , "veg_mgt_opex / icp" := veg_mgt_opex/nb_connections]
my_vsimple_scatter_gplot(dt_plot[disc_yr == overall_period], 
                         x_var="veg_saifi", y_var="veg_mgt_opex / overhead_length", 
                         groupby="PAT_peergroup", size_var = "veg_mgt_opex / icp", x_log_scale = F)

summary(glm(data=dt_plot, formula="veg_mgt_opex  ~ veg_saifi + PAT_peergroup + overhead_length"))

summary(glm(data=dt_plot, formula="veg_mgt_opex  ~ veg_saifi + line_length + overhead_length"))
summary(glm(data=dt_plot, formula="veg_mgt_opex  ~ veg_saifi + overhead_length"))
summary(glm(data=dt_plot, formula="veg_mgt_opex  ~ veg_saifi + line_length"))

dt_plot <- data.table(dt_edb_avg)[veg_saifi == 0, veg_saifi := 0.001][, "veg_mgt_opex / overhead_length" := veg_mgt_opex/overhead_length][
  , "veg_mgt_opex / icp" := veg_mgt_opex/nb_connections]
my_vsimple_scatter_gplot(dt_plot[disc_yr == overall_period], 
                         x_var="overhead_length", y_var="veg_mgt_opex", 
                         groupby="PAT_peergroup", size_var = "veg_mgt_opex / icp", x_log_scale = T)

dt_plot[, veg_saidi_next := lag(veg_saidi, 1)]
dt_plot[, veg_saifi_next := lag(veg_saifi, 1)]

dt_plot[, veg_saifi_next_next := lag(veg_saifi, 2)]
dt_plot[, veg_saifi_next_next_next := lag(veg_saifi, 3)]



summary(glm(data=dt_plot, formula="veg_mgt_opex / overhead_length ~ veg_saifi + edb"))
summary(glm(data=dt_plot, formula="veg_mgt_opex / overhead_length ~ veg_saifi_next + edb"))
summary(glm(data=dt_plot, formula="veg_mgt_opex / overhead_length ~ veg_saifi_next_next + edb"))
summary(glm(data=dt_plot, formula="veg_mgt_opex / overhead_length ~ veg_saifi_next_next_next + edb"))
summary(glm(data=dt_plot, formula="veg_mgt_opex  ~ veg_saifi + overhead_length + edb"))
summary(glm(data=dt_plot, formula="veg_mgt_opex  ~ veg_saifi + overhead_length"))

summary(glm(data=dt_plot, formula="veg_mgt_opex / line_length ~ veg_saifi + edb"))
summary(glm(data=dt_plot, formula="veg_mgt_opex  ~ veg_saifi + line_length + edb"))
summary(glm(data=dt_plot, formula="veg_mgt_opex  ~ veg_saifi + line_length"))


summary(glm(data=dt_plot, formula="veg_mgt_opex / overhead_length ~ veg_saidi + edb"))
summary(glm(data=dt_plot, formula="veg_mgt_opex  ~ veg_saidi + overhead_length + edb"))
summary(glm(data=dt_plot, formula="veg_mgt_opex  ~ veg_saidi + overhead_length"))

summary(glm(data=dt_plot, formula="veg_mgt_opex / line_length ~ veg_saidi + edb"))
summary(glm(data=dt_plot, formula="veg_mgt_opex  ~ veg_saidi + line_length + edb"))
summary(glm(data=dt_plot, formula="veg_mgt_opex  ~ veg_saidi + line_length"))

sapply(unique(dt_plot$edb), function(edb_nm) cor(
  dt_plot[edb==edb_nm]$`veg_mgt_opex/overhead_length`,dt_plot[edb==edb_nm]$veg_saifi))


sapply(setdiff(unique(dt_plot$edb), "Electricity Invercargill"), function(edb_nm) cor(
  +   dt_plot[edb==edb_nm]$`veg_mgt_opex/overhead_length`,dt_plot[edb==edb_nm]$veg_saifi))


source("R/00_inflation_data_helper.R")
dt_r <- data.table(dt)
dt_r <- merge(dt_r, get_inflation_real_factor("cpi", quarter=1, min_year=2013), by.x="disc_yr", by.y="year")
dt_r[, veg_mgt_opex := veg_mgt_opex * cpi_real]

dt_tmp <- data.table(dt)
# dt_tmp <- data.table(dt_r)
setorderv(dt_tmp, c("edb", "disc_yr"), order=-1)
dt_tmp[, `:=`(veg_saifi_next1 = lag(veg_saifi, 1),
              veg_saifi_next2 = lag(veg_saifi, 2), 
              veg_saifi_next3 = lag(veg_saifi, 3), 
              veg_saifi_next4 = lag(veg_saifi, 4)), by="edb"]
# should we remove late years (missing anyway for lagged variables to compare more fairly?)
# dt_tmp <- dt_tmp[disc_yr %in% sort(unique(dt$disc_yr))[1:(length(unique(dt$disc_yr))-4)]]
setorderv(dt_tmp, c("edb", "disc_yr"), order=1)
# dt_tmp[edb == "Aurora Energy"]

ft_nm <- "veg_saifi"
cor_res <- sapply(unique(dt_tmp$edb), function(edb_nm) cor(
  dt_tmp[edb==edb_nm]$`veg_mgt_opex`/dt_tmp[edb==edb_nm]$`overhead_length`,
  dt_tmp[edb==edb_nm][[ft_nm]], use = "pairwise.complete.obs"))
dt_cor <- data.table(edb=names(cor_res), my_ft_nm=cor_res)
setnames(dt_cor, "my_ft_nm", ft_nm)
for (ft_nm in c("veg_saifi_next1", "veg_saifi_next2", "veg_saifi_next3", "veg_saifi_next4")) {
  cor_res <- sapply(unique(dt_tmp$edb), function(edb_nm) cor(
    dt_tmp[edb==edb_nm]$`veg_mgt_opex`/dt_tmp[edb==edb_nm]$`overhead_length`,
    dt_tmp[edb==edb_nm][[ft_nm]], use = "pairwise.complete.obs"))
  dt_cor_tmp <- data.table(edb=names(cor_res), my_ft_nm=cor_res)
  setnames(dt_cor_tmp, "my_ft_nm", ft_nm)
  dt_cor <- merge(dt_cor, dt_cor_tmp)
  
}

dt_cor_long <- melt(dt_cor, id.vars = "edb", 
                    variable.name = "measure", 
                    value.name = "correlation")

# Reverse the order of edb
dt_cor_long$edb <- factor(dt_cor_long$edb, levels = rev(unique(dt_cor_long$edb)))
# dt_cor_long <- dt_cor_long[!is.na(correlation)] # rm invercargill as always 0 veg_saifi


ggplot(dt_cor_long, aes(measure, edb)) +
  geom_tile(aes(fill = correlation)) +
  geom_text(aes(label = round(correlation, 2))) +
  theme_minimal() +
  scale_fill_gradient2(low = "deepskyblue3", mid = "white", high = "brown2", midpoint = 0) + 
  ggtitle("Correlation between vegetation cost and outcomes")




dt_cor_long[, .(mean = mean(correlation, na.rm=T)), by=c("measure")]

edb_nm <- "Nelson Electricity"


summary(glm(data=dt, formula="veg_mgt_opex  ~ veg_saifi + PAT_peergroup + overhead_length"))

summary(glm(data=dt, formula="I(log(veg_mgt_opex/overhead_length)) ~ I(log(veg_saifi)) + PAT_peergroup "))


summary(glm(data=dt[veg_mgt_opex!=0 & veg_saifi !=0 & (!is.na(veg_saifi))], 
            formula="I(log(veg_mgt_opex/overhead_length)) ~ I(log(veg_saifi)) + I(log(PAT_peergroup))"))


dt[veg_mgt_opex!=0 & veg_saifi !=0 & (!is.na(veg_saifi))]
dt_tmp[veg_mgt_opex!=0 & veg_saifi !=0 & (!is.na(veg_saifi))]

summary(glm(data=dt_tmp[veg_mgt_opex!=0 & veg_saifi !=0], 
            formula="I(log(veg_mgt_opex/overhead_length)) ~ I(log(veg_saifi)) + edb"))
summary(glm(data=dt_tmp[veg_mgt_opex!=0 & veg_saifi !=0 & !is.na(veg_saifi_next1)], 
            formula="I(log(veg_mgt_opex/overhead_length)) ~ I(log(veg_saifi_next1)) + edb"))

summary(glm(data=dt_tmp[veg_mgt_opex!=0 & veg_saifi !=0 & (!is.na(veg_saifi_next1)) &
                          veg_saifi_next1 != 0], 
            formula="I(log(veg_mgt_opex/overhead_length)) ~ I(log(veg_saifi_next1)) + edb"))

summary(glm(data=dt_tmp[veg_mgt_opex!=0 & veg_saifi !=0 & (!is.na(veg_saifi_next2)) &
                          veg_saifi_next2 != 0], 
            formula="I(log(veg_mgt_opex/overhead_length)) ~ I(log(veg_saifi_next2)) + edb"))

summary(glm(data=dt_tmp[veg_mgt_opex!=0 & veg_saifi !=0 & 
                          (!is.na(veg_saifi_next2)) &
                          veg_saifi_next2 != 0], 
            formula="I(log(veg_mgt_opex/overhead_length)) ~ I(veg_saifi_next2) + edb"))

summary(glm(data=dt_tmp[veg_mgt_opex!=0 & veg_saifi !=0 & (!is.na(veg_saifi_next2)) &
                          veg_saifi_next2 != 0], 
            formula="I(log(veg_mgt_opex/overhead_length)) ~ I(veg_saifi_next2)"))

m <- estimatr::lm_robust(as.formula("I(log(veg_mgt_opex/overhead_length)) ~ I(veg_saifi_next2) + edb"), 
                         data = dt_tmp[veg_mgt_opex!=0 & veg_saifi !=0 & (!is.na(veg_saifi_next2)) &
                                         veg_saifi_next2 != 0], se_type='HC0')

summary(m)


m

estimatr::lm_robust(as.formula("I(log(veg_mgt_opex/overhead_length)) ~ I(veg_saifi_next2)"), 
                    data = dt_tmp[veg_mgt_opex!=0 & veg_saifi !=0 & (!is.na(veg_saifi_next2)) &
                                    veg_saifi_next2 != 0], se_type='HC0')
my_reg_var <- "veg_saifi_next1"

m <- estimatr::lm_robust(as.formula(paste0("I(log(veg_mgt_opex)) ~ I(", my_reg_var,") + I(log(overhead_length))")), 
                         data = dt_tmp[veg_mgt_opex!=0 & (!is.na(get(my_reg_var))) &
                                         get(my_reg_var) != 0], se_type='HC0')

m <- estimatr::lm_robust(as.formula(paste0("I(log(veg_mgt_opex/overhead_length)) ~ I(", my_reg_var,")")), 
                         data = dt_tmp[veg_mgt_opex!=0 & (!is.na(get(my_reg_var))) &
                                          get(my_reg_var) != 0], se_type='HC0')

m

dt_tmp[veg_mgt_opex!=0 & veg_saifi !=0 & (!is.na(veg_saifi_next1))]$veg_saifi_next1

m_res <- data.table(broom::tidy(m))
m_res[, stars := ifelse(p.value < 0.001, "***",
                        ifelse(p.value < 0.01, "**",
                               ifelse(p.value < 0.05, "*", "")))]
m_res <- m_res[, .(term, estimate, std.error, stars)]
# m_res <- m_res[!grepl("edb", term), .(term, estimate, std.error, stars)]

m_res

# 
# ?scale_fill_gradient()
# 
# edb_nm <- "Aurora Energy"
# edb_nm <- "Electricity Invercargill"
# 
# dt_tmp[edb== "Aurora Energy"]

# cor(c(1,2,3,4), c(1,3,7,NA), use = "pairwise.complete.obs")
# cor(c(1,2,3), c(1,3,7), use = "pairwise.complete.obs")


