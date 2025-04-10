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



summary(glm(data=dt_plot, formula="veg_mgt_opex / overhead_length ~ veg_saifi + edb"))
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
  dt_plot$`veg_mgt_opex/overhead_length`,dt_plot$veg_mgt_opex))

       