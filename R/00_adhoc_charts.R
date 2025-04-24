source(file.path(here::here(), "R/00_echarts.R"))

# the below charts are called in the quarto dashboards

# these charts should ideally be rewritten with echarts4r to facilitate reading
# lack of time and priorities on quantity rather than quality means it's unlikely to
# happen in the foreseeable future
# define scatter chart we'll keep using everywhere
my_scatter_gplot <- function(dt, x_var, y_numerator, y_denominator=NULL, groupby, 
                             plot_disc_yr, title=NULL, x_log_scale=T) {
  
  if (!is.null(y_denominator)) {
    dt_plot <- data.table(dt)[, `:=`(metric = get(y_numerator)/get(y_denominator), 
                                     x = get(x_var))][,  c("disc_yr", "edb", groupby,
                                                           "x", "metric"), with=F]
  } else {
    dt_plot <- data.table(dt)[, `:=`(metric = get(y_numerator), x = get(x_var))][
      , c("disc_yr", "edb", groupby, "x", "metric"), with=F]
  }
  
  dt_plot2 <- dt_plot[, .(x = mean(x), metric = mean(metric)), 
                      by=c("edb", groupby)][, disc_yr := overall_period]
  dt_plot <- rbind(dt_plot, dt_plot2)
  
  p <- ggplot(dt_plot[disc_yr == plot_disc_yr],
              aes(x = x, y = metric)) +
    geom_point(aes(color=get(groupby)), size=2, alpha=0.8) +
    theme_minimal() +
    ylim(c(0, NA)) + 
    xlab(x_var) +
    ggrepel::geom_text_repel(aes(label=edb, color=get(groupby)), size=3, alpha=0.8, show.legend=FALSE) +
    # geom_smooth(method = "lm", se = FALSE, color = "black", alpha=0.1) +
    labs(title = "geom_text_repel()", alpha=0.8) +
    theme(legend.title = element_blank())
  if (!x_var %in% c("density") & x_log_scale) p <- p + coord_trans(x='log10')

  if (!is.null(y_denominator)) {
    p <- p + ylab(paste0(y_numerator, " / ", y_denominator))
  } else {
    p <- p + ylab(paste0(y_numerator))
  }
  
  if (!is.null(title)) {
    p <- p + ggtitle(title)
  } else {
    if (!is.null(y_denominator)) {
      p <- p + ggtitle(paste0(y_numerator, " / ", y_denominator, "   (", plot_disc_yr, ")"))
    } else {
      p <- p + ggtitle(paste0(y_numerator, "   (", plot_disc_yr, ")"))
    }
  }
  
  p
}

# define bar chart we'll keep using everywhere
my_bar_gplot <- function(dt, y_numerator, y_denominator=NULL, groupby, plot_disc_yr, flipped_axes=T) {
  
  if (!is.null(y_denominator)) {
    dt_plot <- data.table(dt)[, `:=`(metric = get(y_numerator)/get(y_denominator))
    ][,  c("disc_yr", "edb", groupby, "metric"), with=F]
  } else {
    dt_plot <- data.table(dt)[, `:=`(metric = get(y_numerator))][,  c("disc_yr", "edb", groupby, "metric"), with=F]
  }
  
  dt_plot2 <- dt_plot[, .(metric = mean(metric)), 
                      by=c("edb", groupby)][, disc_yr := overall_period]
  dt_plot <- rbind(dt_plot, dt_plot2)
  # hack to have desired order
  dt_plot[ , my_order := match(get(groupby), sort(unique(dt_plot[[groupby]]))) * 1000000 + metric]
  
  dt_plot <- dt_plot[order(my_order)]
  
  if (flipped_axes) {
    p <- ggplot(dt_plot[disc_yr == plot_disc_yr], 
                aes(x = metric, y = reorder(edb, my_order, desc=T), fill = get(groupby))) +
      theme_minimal() + 
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.y = element_text(margin = margin(r = -8))
      )
  } else {
    p <- ggplot(dt_plot[disc_yr == plot_disc_yr], 
                aes(y = metric, x = reorder(edb, my_order, desc=T), fill = get(groupby))) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(margin = margin(r = 8))
      )
  }
  
  p <- p + geom_bar(stat = "identity", alpha=0.8, position = position_dodge()) + 
    labs(
      # title = paste0(y_numerator, " / ", y_denominator, "   (", plot_disc_yr, ")"),
      x = "",
      y = "",
      fill = "")
  
  if (!is.null(y_denominator)) {
    p <- p + ggtitle(paste0(y_numerator, " / ", y_denominator, "   (", plot_disc_yr, ")"))
  } else {
    p <- p + ggtitle(paste0(y_numerator, "   (", plot_disc_yr, ")"))
  }
  
  p
}

# simplified version of the scatter plot that don't make as many assumptions
# was necessary to deal with modifying obtained plot
my_simple_scatter_gplot <- function(dt, x_var, y_numerator, y_denominator=NULL, groupby, 
                                    title=NULL, x_log_scale=T) {
  
  if (!is.null(y_denominator)) {
    dt_plot <- data.table(dt)[, `:=`(metric = get(y_numerator)/get(y_denominator), 
                                     x = get(x_var))][,  c("disc_yr", "edb", groupby,
                                                           "x", "metric"), with=F]
  } else {
    dt_plot <- data.table(dt)[, `:=`(metric = get(y_numerator), x = get(x_var))][
      , c("disc_yr", "edb", groupby, "x", "metric"), with=F]
  }
  
  p <- ggplot(dt_plot,
              aes(x = x, y = metric)) +
    geom_point(aes(color=get(groupby)), size=2, alpha=0.8) +
    theme_minimal() +
    ylim(c(0, NA)) + 
    xlab(x_var) +
    ggrepel::geom_text_repel(aes(label=edb, color=get(groupby)), size=3, alpha=0.8, show.legend=FALSE) +
    # geom_smooth(method = "lm", se = FALSE, color = "black", alpha=0.1) +
    labs(title = "geom_text_repel()", alpha=0.8) +
    theme(legend.title = element_blank())
  if (!x_var %in% c("density") & x_log_scale) p <- p + coord_trans(x='log10')
  
  if (!is.null(y_denominator)) {
    p <- p + ylab(paste0(y_numerator, " / ", y_denominator))
  } else {
    p <- p + ylab(paste0(y_numerator))
  }
  
  if (!is.null(title)) {
    p <- p + ggtitle(title)
  } else {
    if (!is.null(y_denominator)) {
      p <- p + ggtitle(paste0(y_numerator, " / ", y_denominator))
    } else {
      p <- p + ggtitle(paste0(y_numerator))
    }
  }
  
  p
}


# creates a dual axis chart
# left made of green bars, right red dots and lines
# both y-axis start at zero
my_dual_axis_times_series_gplot <- function(dt, x_var, y1_var, y2_var, x_var_nm=NULL, 
                                            y1_var_nm=NULL, y2_var_nm=NULL, y1_m_dollars=T, 
                                            y2_m_dollars=F) {
  
  # allow customisation of displayed names
  if (is.null(x_var_nm)) x_var_nm <- x_var
  if (is.null(y1_var_nm)) y1_var_nm <- y1_var
  if (is.null(y2_var_nm)) y2_var_nm <- y2_var
  
  # dual axis trick
  dt_plot <- data.table(dt)[, scaled_y2 := get(y2_var) * max(get(y1_var)) / max(get(y2_var))] 
  
  p <- ggplot(dt_plot, aes(x = disc_yr)) +
    geom_bar(aes(y = get(y1_var)), stat = "identity", fill = "cyan4", alpha = 0.9) +  
    geom_line(aes(y = scaled_y2, group = 1), color = "brown2", size = 1) +  
    geom_point(aes(y = scaled_y2), color = "brown2", size = 3) +  
    scale_x_continuous(breaks = scales::pretty_breaks(n = 6), name=paste0(x_var_nm)) +
    scale_y_continuous(
      labels = ifelse(y1_m_dollars, scales::label_dollar(scale = 1e-6, suffix = "m"), 
                      scales::label_number()),
      name = paste0(y1_var_nm),  
      limits = c(0, max(dt_plot[[y1_var]]))*1.1,
      sec.axis = sec_axis(
        trans = ~ . * max(dt_plot[[y2_var]]) / max(dt_plot[[y1_var]]), 
        name = paste0(y2_var_nm),
        labels = ifelse(y2_m_dollars, scales::label_dollar(scale = 1e-6, suffix = "m"), 
                        scales::label_number())
      )  
    ) +  
    theme_minimal() +
    theme(
      axis.title.y = element_text(color = "cyan4", size = 12, face = "bold"),
      axis.title.y.right = element_text(color = "brown2", size = 12, face = "bold")
    )

  p
}

my_single_axis_bar_plot <- function(dt, x_var, y1_var, y2_var, x_var_nm=NULL, 
                                    y1_var_nm=NULL, y2_var_nm=NULL, y_m_dollars=T) {
  
  # Allow customization of displayed names
  if (is.null(x_var_nm)) x_var_nm <- x_var
  if (is.null(y1_var_nm)) y1_var_nm <- y1_var
  if (is.null(y2_var_nm)) y2_var_nm <- y2_var
  
  # Transform data into long format using melt
  dt_long <- melt(data.table(dt), 
                  id.vars = x_var, 
                  measure.vars = c(y1_var, y2_var), 
                  variable.name = "Metric", 
                  value.name = "Value")
  
  # Update Metric column to display custom names
  dt_long[, Metric := fifelse(Metric == y1_var, y1_var_nm, y2_var_nm)]
  
  # Create ggplot
  p <- ggplot(dt_long, aes(x = get(x_var), y = Value, fill = Metric)) +
    geom_bar(stat = "identity", position = position_dodge(), alpha = 0.9) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 6), name = x_var_nm) +
    scale_y_continuous(
      labels = ifelse(y_m_dollars, scales::label_dollar(scale = 1e-6, suffix = "m"), 
                      scales::label_number()),
      limits = c(0, max(dt_long[["Value"]]))*1.1,
      name = paste0(ifelse(y_m_dollars, paste0("$"), ""))
    ) +
    theme_minimal() +
    theme(
      legend.title = element_blank(),
      # legend.text = element_text(size = 10),
    )
  
  return(p)
}

my_dual_axis_times_series_lines_gplot <- function(dt, x_var, y1_var, y2_var, x_var_nm=NULL, 
                                            y1_var_nm=NULL, y2_var_nm=NULL, y1_m_dollars=T, 
                                            y2_m_dollars=F) {
  
  # Allow customization of displayed names
  if (is.null(x_var_nm)) x_var_nm <- x_var
  if (is.null(y1_var_nm)) y1_var_nm <- y1_var
  if (is.null(y2_var_nm)) y2_var_nm <- y2_var
  
  # Dual axis trick: scale y2 to align with y1
  dt_plot <- data.table(dt)[, scaled_y2 := get(y2_var) * max(get(y1_var)) / max(get(y2_var))] 
  
  # Create ggplot with similar styles for both variables
  p <- ggplot(dt_plot, aes(x = get(x_var))) +
    geom_line(aes(y = get(y1_var), group = 1), color = "cyan4", size = 1) +  
    geom_point(aes(y = get(y1_var)), color = "cyan4", size = 3) +  
    geom_line(aes(y = scaled_y2, group = 1), color = "brown2", size = 1) +  
    geom_point(aes(y = scaled_y2), color = "brown2", size = 3) +  
    scale_x_continuous(breaks = scales::pretty_breaks(n = 6), name = paste0(x_var_nm)) +
    scale_y_continuous(
      labels = ifelse(y1_m_dollars, scales::label_dollar(scale = 1e-6, suffix = "m"), 
                      scales::label_number()),
      name = paste0(y1_var_nm),  
      limits = c(0, max(dt_plot[[y1_var]])) * 1.2,
      sec.axis = sec_axis(
        trans = ~ . * max(dt_plot[[y2_var]]) / max(dt_plot[[y1_var]]), 
        name = paste0(y2_var_nm),
        labels = ifelse(y2_m_dollars, scales::label_dollar(scale = 1e-6, suffix = "m"), 
                        scales::label_number())
      )  
    ) +  
    theme_minimal() +
    theme(
      axis.title.y = element_text(color = "cyan4", size = 12, face = "bold"),
      axis.title.y.right = element_text(color = "brown2", size = 12, face = "bold")
    )
  
  return(p)
}


# my_dual_axis_times_series_gplot(dt[edb=="Orion NZ"], "disc_yr", "veg_mgt_opex", "emergencies_opex", y2_m_dollars=T)
# my_dual_axis_times_series_lines_gplot(dt[edb=="Orion NZ"], "disc_yr", "veg_mgt_opex", "emergencies_opex", y2_m_dollars=T)

# 
# my_single_axis_bar_plot(dt[edb=="Orion NZ"], x_var="disc_yr", y1_var="veg_mgt_opex", 
#                         y2_var="emergencies_opex")
# 
# my_single_axis_bar_plot(dt[edb=="Orion NZ"], x_var="disc_yr", y1_var="emergencies_opex", 
#                         y2_var="veg_mgt_opex")


# 
# my_dual_axis_times_series_gplot(dt_lags[edb=="Orion NZ"], "disc_yr", "veg_mgt_opex_real",
#                                 "veg_saidi", x_var_nm="year", y1_var_nm="vegetation mgt expense",
#                                 y2_var_nm="vegetation SAIDI")
