library(data.table)
library(dplyr)
library(echarts4r)

# returns 20 colours we're okay using (distinctive enough)
my_ecolors <- function() {
  # colours picked from https://medialab.github.io/iwanthue/
  # or https://sashamaps.net/docs/resources/20-colors/
  c('#e6194B', '#3cb44b', '#ffe119', '#4363d8', 
    '#f58231', '#911eb4', '#42d4f4', '#f032e6', 
    '#bfef45', '#fabed4', '#469990', '#dcbeff', 
    '#9A6324', '#fffaa1', '#800000', '#aaffc3', 
    '#808000', '#ffd8b1', '#000075', '#a9a9a9')
}


# generate nice eplot
# x must behave nicely as a factor... recommend to sort beforehands
# y must be a numeric
eplot_bar_groupby <- function(dt, x, y, groupby=NULL, stack=T, x_lab=NULL, y_lab=NULL, decimals=3, magic=T) {
  stopifnot(!"x__" %in% c(y, groupby)) # we'll create x__, we f*ck it up if it means something already
  if (is.null(y_lab)) y_lab <- y
  if (is.null(x_lab)) x_lab <- x
  dt_p <- data.table(dt)[, x__ := factor(get(x))]   # copy input data, create x__ variable as a factor
  dt_p[, (y) := round(get(y), decimals)]   # round data to help display / hover text
  p <- dt_p |> group_by(get(groupby)) |> e_charts(x__)
  if (stack) p <- p |> e_bar_(y, stack="total", areaStyle=list())
  else p <- p |> e_bar_(y, areaStyle=list())  # dodge
  p <- p |>  e_axis(x__, axis="y", name=y_lab, nameLocation="middle", nameGap=30) |> 
    e_axis(x__, axis="x", name=x_lab, nameLocation="middle", nameGap=25, axisPointer=list(show=TRUE))
  p <- p |> e_tooltip(trigger="axis", axisPointer = list(type="cross"))
  p <- p |> e_legend(bottom=0) |> e_toolbox_feature(feature=c("dataView", "saveAsImage"))
  if (magic) p <- e_toolbox_feature(p, feature = "magicType", type=list("line", "bar"))
  p
}


eplot_line <- function(dt, x, y, groupby=NULL, x_lab=NULL, y_lab=NULL, decimals=3,
                       x_as_factor=T, x_include_zero=T, y_include_zero = T) {
  
  # hack to display x as 2016 not 2,016
  label_hack <- list(
    formatter = htmlwidgets::JS(
      'function(value, index){
            return value;
        }'
    )
  )
  
  if (is.null(y_lab)) y_lab <- y
  if (is.null(x_lab)) x_lab <- x
  
  dt_p <- data.table(dt)  # copy input data
  
  # round data to help display / hover text
  dt_p[, (y) := round(get(y), decimals)]
  if (is.numeric(dt_p[[x]])) dt_p[, (x) := round(get(x), decimals)]
  
  p <- dt_p
  if (!is.null(groupby)) {
    p <- p |> group_by(across(all_of(groupby)))
  }
  p <- p |> e_charts_(x) |>
    e_line_(y, symbolSize = 10, emphasis = list(focus = "series")) |>
    e_tooltip(trigger = "axis") |>
    e_x_axis(name = x_lab, nameLocation = "middle", nameGap = 30, axisLabel = label_hack) |>
    e_y_axis(name = y_lab, nameLocation = "middle", nameGap = 30) |> 
    e_toolbox() |>
    e_toolbox_feature(feature = c("saveAsImage", "restore", "dataZoom", "dataView")) |>
    e_aria(enabled = TRUE)
  
  # add the legend only if a group by has been used?
  if (is.null(groupby)) {
    p <- p |> e_legend(show = FALSE)
  } else {
    p <- p |> e_legend(bottom = 0)
  }

  p <- p |> e_color(my_ecolors())
  
  if (!x_include_zero) p <- p |> e_x_axis(scale=T)
  if (!y_include_zero) p <- p |> e_y_axis(scale=T)
  
  if (x_as_factor) p <- p |> e_x_axis_(x)

  p
}

# hack  to control the behaviour of hover text
my_eplot_hover_text_JS <- function(x_lab, y_lab, groupby) {
  if (!is.null(groupby)){
    return(paste0("
      function(params) {
        return '<span style=\"color:' + params.color + '\">●</span>' + 
               ' <strong>", groupby, "</strong> ' + params.seriesName + '<br>' +
               '<strong>", x_lab, "</strong> ' + params.value[0] + '<br>' +
               '<strong>", y_lab, "</strong> ' + params.value[1] + '<br>'
      }
    "))
  }
  paste0("
      function(params) {
        return '<span style=\"color:' + params.color + '\">●</span>' + 
               ' <strong>", x_lab, "</strong> ' + params.value[0] + '<br>' +
               '<span style=\"color:' + params.color + '\">●</span>' + 
               ' <strong>", y_lab, "</strong> ' + params.value[1] + '<br>'
      }
    ")
}


eplot_scatter <- function(dt, x, y, groupby=NULL, x_lab=NULL, y_lab=NULL, decimals=3,
                          x_include_zero=T, y_include_zero = T) {
  
  stopifnot(x %in% names(dt))
  stopifnot(y %in% names(dt))
  
  if (is.null(y_lab)) y_lab <- y
  if (is.null(x_lab)) x_lab <- x
  
  dt_p <- data.table(dt)  # copy input data
  
  # round data to help display / hover text
  dt_p[, (y) := round(get(y), decimals)]
  if (is.numeric(dt_p[[x]])) dt_p[, (x) := round(get(x), decimals)]
  
  p <- dt_p
  if (!is.null(groupby)) p <- p |> group_by(across(all_of(groupby)))
  p <- p |>
    e_charts_(x) |>
    e_scatter_(y, symbol_size = 10, emphasis = list(focus = "item")) |>
    e_x_axis(name = x_lab, nameLocation = "middle", nameGap = 30) |>
    e_y_axis(name = y_lab, nameLocation = "middle", nameGap = 30) |>
    e_tooltip(trigger = "item", formatter = htmlwidgets::JS(
      my_eplot_hover_text_JS(x_lab, y_lab, groupby))) |>
    # e_tooltip(trigger = "item") |>
    e_toolbox() |>
    e_toolbox_feature(feature = c("saveAsImage", "restore", "dataZoom", "dataView")) |>
    e_aria(enabled = TRUE)
  
  # add the legend only if a group by has been used?
  if (is.null(groupby)) {
    p <- p |> e_legend(show = FALSE)
  } else {
    p <- p |> e_legend(bottom = 0)
  }
  if (!x_include_zero) p <- p |> e_x_axis(scale=T)
  if (!y_include_zero) p <- p |> e_y_axis(scale=T)
  
  p
}

# slight variation of pretty_breaks to handle little amount of points well
my_pretty_breaks <- function(x) {
  x_breaks <- scales::breaks_pretty()(x)
  if (length(unique(x)) < length(x_breaks)) {
    x_breaks <- scales::breaks_pretty(n=length(unique(x))-1)(x)
  }
  x_breaks
}


# function that uses heuristics to decide whether zero should be included in the representation
# WIP
# heuristics_should_include_zero <- function(x) {
#   if (!is.numeric(x)) return(F)
#   min_x <- min(x)
#   max_x <- max(x)
#   range_x <- max_x - min_x
# }



# ## example / debug below

# dt <- melt(data.table(mtcars)[, .(avg_mpg = mean(mpg), avg_hp = mean(hp)), by=cyl], id.var="cyl")
# 
# eplot_bar_groupby(dt, "cyl", "value", "variable", x_lab = "")
# eplot_bar_groupby(dt, "cyl", "value", "variable", stack = F)
# 
# dt <- melt(data.table(mtcars)[, .(avg_mpg = mean(disp), avg_hp = mean(hp)), by=cyl], id.var="cyl")


# eplot_line(dt, "cyl", "value", groupby="variable", y_lab = "")
# eplot_line(dt, "value", "cyl", groupby="variable", y_lab = "")
# eplot_line(dt, "value", "cyl", groupby="variable", y_lab = "", x_as_factor = F)
# eplot_line(dt, "value", "cyl", groupby="variable", y_lab = "", x_as_factor = F, 
#            x_include_zero = F, y_include_zero = F)
# eplot_line(dt[variable=="avg_mpg"], "cyl", "value", y_lab = "")
# 
# 
# dt[, year := c(2002:(2002+5))]
# eplot_line(dt[variable=="avg_mpg"], "year", "value", y_lab = "")
# 
# 
# eplot_scatter(mtcars, x="mpg", y="wt")
# eplot_scatter(mtcars, x="mpg", y="wt", groupby="cyl")
