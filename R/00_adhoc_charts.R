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
