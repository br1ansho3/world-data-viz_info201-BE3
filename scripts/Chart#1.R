library(ggplot2)
library(dplyr)
# Read in csv file for Chart 1
small_countries <- read.csv("data/WDVP Datasets - small countries are beautiful.csv", stringsAsFactors = F)
small_countries_edited <- small_countries[5:76,]

# GDP per km2 vs %foreign born 
gdp_area_vs_foreign_born <- function(dataframe_choice) {
  ggplot(data = dataframe_choice, mapping = aes(x = GDP.per.km2, 
                                         y = X..foreign.born.population) +
           geom_bar(stat = "identity"))
}