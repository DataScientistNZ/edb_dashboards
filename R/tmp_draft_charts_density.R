library(data.table)
library(ggplot2)

options(scipen=999)

dt <- fread("data/generic_purpose_edb_data.csv")

latest_year <- max(dt$disc_yr)
overall_period <- paste0(min(dt$disc_yr), "-", latest_year)

y_numerator <- "totex_fcs"
# y_denominator <- "nb_connections"
y_denominator <- "icp50_line50"
x_var <- "density"
# x_var <- "nb_connections"
# x_var <- "line_length"
# x_var <- "icp50_line50"
groupby <- "PAT_peergroup"

plot_disc_yr <- latest_year
# plot_disc_yr <- overall_period

# generic scatter plot we'll keep using everywhere
my_scatter_gplot <- function(dt, x_var, y_numerator, y_denominator, groupby, plot_disc_yr) {
  dt_plot <- data.table(dt)[, `:=`(metric = get(y_numerator)/get(y_denominator), 
                                   x = get(x_var))][,  c("disc_yr", "edb", groupby,
                                                         "x", "metric"), with=F]
  dt_plot2 <- dt_plot[, .(x = mean(x), metric = mean(metric)), 
                      by=c("edb", groupby)][, disc_yr := overall_period]
  dt_plot <- rbind(dt_plot, dt_plot2)
  
  p <- ggplot(dt_plot[disc_yr == plot_disc_yr],
              aes(x = x, y = metric)) +
    geom_point(aes(color=get(groupby)), size=2, alpha=0.8) +
    theme_minimal() +
    ylab(paste0(y_numerator, " / ", y_denominator)) + 
    ylim(c(0, NA)) + 
    xlab(x_var) +
    ggrepel::geom_text_repel(aes(label=edb, color=get(groupby)), size=3, alpha=0.8, show.legend=FALSE) +
    # geom_smooth(method = "lm", se = FALSE, color = "black", alpha=0.1) +
    labs(title = "geom_text_repel()", alpha=0.8) +
    ggtitle(paste0(y_numerator, " / ", y_denominator, "   (", plot_disc_yr, ")")) +
    theme(legend.title = element_blank())
  if (!x_var %in% c("density")) p <- p + coord_trans(x='log10')
  p
}

# generic bar plot we'll keep using everywhere
my_bar_gplot <- function(dt, y_numerator, y_denominator, groupby, plot_disc_yr) {
  dt_plot <- data.table(dt)[, `:=`(metric = get(y_numerator)/get(y_denominator))
                            ][,  c("disc_yr", "edb", groupby, "metric"), with=F]
  dt_plot2 <- dt_plot[, .(metric = mean(metric)), 
                      by=c("edb", groupby)][, disc_yr := overall_period]
  dt_plot <- rbind(dt_plot, dt_plot2)
  # hack to have desired order
  dt_plot[ , my_order := match(get(groupby), sort(unique(dt_plot[[groupby]]))) * 1000000 + metric]
  
  dt_plot <- dt_plot[order(my_order)]
  
  ggplot(dt_plot[disc_yr == plot_disc_yr], aes(x = metric, y = reorder(edb, my_order, desc=T), fill = get(groupby))) +
    geom_bar(stat = "identity", alpha=0.8, position = position_dodge()) + 
    theme_minimal() +
    labs(
      title = paste0(y_numerator, " / ", y_denominator, "   (", plot_disc_yr, ")"),
      x = "",
      y = "",
      fill = "") + 
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.text.y = element_text(margin = margin(r = -8))
    )
}


my_scatter_gplot(dt, x_var="density", y_numerator="opex", y_denominator = "icp50_line50", 
                 groupby="status", plot_disc_yr=overall_period)




my_scatter_gplot(dt, x_var="nb_connections", y_numerator="nb_connections", y_denominator = "line_length", 
                 groupby="PAT_peergroup", plot_disc_yr=overall_period)



my_scatter_gplot(dt, x_var="density", y_numerator="totex_fcs", y_denominator = "icp50_line50", 
                 groupby="PAT_peergroup", plot_disc_yr=overall_period)


my_bar_gplot(dt, y_numerator="totex_fcs", y_denominator= "icp50_line50", 
             groupby="PAT_peergroup", plot_disc_yr=overall_period)


my_scatter_gplot(dt, x_var="unplanned_saidi", y_numerator="totex_fcs", y_denominator = "icp50_line50", 
                 groupby="PAT_peergroup", plot_disc_yr=overall_period)

my_scatter_gplot(dt, x_var="unplanned_saidi", y_numerator="totex_fcs", y_denominator = "nb_connections", 
                 groupby="PAT_peergroup", plot_disc_yr=overall_period)

my_scatter_gplot(dt, x_var="unplanned_saidi", y_numerator="totex_fcs", y_denominator = "line_length", 
                 groupby="PAT_peergroup", plot_disc_yr=overall_period)

my_scatter_gplot(dt, x_var="unplanned_saidi", y_numerator="nb_connections", y_denominator = "line_length", 
                 groupby="PAT_peergroup", plot_disc_yr=overall_period)

# dt_plot_agg <- dt_plot[disc_yr == plot_disc_yr, .(metric = mean(metric)), by=c(groupby)]
# 
# ggplot(dt_plot_agg, aes(x = metric, y = edb, fill = get(groupby))) +
#   geom_bar(stat = "identity", alpha=0.8, position = position_dodge()) + 
#   theme_minimal() +
#   labs(
#     title = paste0(y_numerator, " / ", y_denominator, "   (", plot_disc_yr, ")"),
#     x = "",
#     y = "",
#     fill = "") + 
#   theme(
#     panel.grid.major.y = element_blank(),
#     panel.grid.minor.y = element_blank(),
#     axis.text.y = element_text(margin = margin(r = -8))
#   )
    