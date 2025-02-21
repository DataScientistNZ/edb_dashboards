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

my_scatter_gplot(dt, x_var="density", y_numerator="opex", y_denominator = "icp50_line50", 
                 groupby="status", plot_disc_yr=overall_period)






my_scatter_gplot(dt, x_var="nb_connections", y_numerator="nb_connections", y_denominator = "line_length", 
                 groupby="PAT_peergroup", plot_disc_yr=overall_period)


"""
When published in March 2019, the below specifications for PAT peer grouping have been shared with EDBs:

* Group one: Large with major city Large EDBs with greater than 70,000 ICPs serving one of New Zealand's largest cities with a population over 200,000. Comprised of: Orion NZ, Vector, WEL Networks, Wellington Electricity 
* Group two: Large with secondary city Large EDBs with greater than 70,000 ICPs serving a New Zealand city with a population of between 100,000 and 200,000. Comprised of: Aurora Energy, Powerco, Unison Networks 
* Group three: Medium regional Medium sized EDBs with between 30,000 and 70,000 ICPs serving a city or town with a population of between 10,000 and 100,000 Comprised of: Alpine Energy, Counties Power, Electra, Mainpower NZ, Network Tasman 
* Group four: Intermediate regional Intermediate sized EDBs with between 10,000 and 30,000 ICPs serving a city or town with a population of between 10,000 and 100,000. Comprised of: EA Networks, Eastland Network, Horizon Energy, Marlborough Lines, Network Waitaki, Northpower, Waipa Networks 
* Group five: Medium rural Medium or intermediate sized EDBs with between 10,000 and 70,000 ICPs serving smaller towns or areas with populations under 10,000. Comprised of: OtagoNet, The Lines Company, The Power Company, Top Energy, Westpower 
* Group six: Small underground Small or intermediate sized EDBs with up to 30,000 ICPs with a significant proportion of the network underground. Comprised of: Electricity Invercargill, Nelson Electricity 
* Group seven: Small and rural Smaller sized EDBs with less than 10,000 ICPs serving smaller towns or areas with populations under 10,000. Comprised of: Buller Electricity, Centralines, Scanpower 


I believe the best way to understand and display the choice of existing peers is the below figure.
Is is very clear that most of the logic can be explained as a trade off between the two displayed variables:
- mainly nb of customers (explicitely referred in the rules above)
- density as a proxy for rurality (itself actually being a proxy for the line_length vs nb_connection trade off!)

* Large EDB with major city: high number of connections and high density
* Large EDB with secondary city: high number of connections, medium-high density
* Medium regional EDB: medium number of connections, medium density
* Intermediate Regional EDB: medium-low number of connections, medium density
* Medium rural EDB: medium-low number of connections, low density
* Small rural EDB: low number of connections, low density
* Small underground EDB: low number of connections, high density

Minor issue, but let's mention one outlier: Northpower.
This EDB which should have been labelled as Medium Regional EDB instead of Intemediate EDB.
This should have been the case according to the specifications for the PAT peer grouping and it's also clear visually.
"""



my_scatter_gplot(dt, x_var="density", y_numerator="totex_fcs", y_denominator = "icp50_line50", 
                 groupby="PAT_peergroup", plot_disc_yr=overall_period)
