library(ggplot2)
library(dplyr)
library(plotly)

# Getting Rid of Column Names
gdp_area_vs_foreign_born <- function(dataframe_choice) {
  small_countries_edited <- dataframe_choice[5:76, ]
  # Getting Rid of NA values and selecting columns
  for_function <- small_countries_edited %>%
    select(indicator, GDP, X..foreign.born.population) %>%
    filter(
      !is.na(indicator), !is.na(GDP),
      !is.na(X..foreign.born.population)
    ) %>%
    mutate(
      GDP = as.numeric(as.character(GDP)),
      X..foreign.born.population = as.numeric(as.character(
        X..foreign.born.population
      ))
    )
  # Plotting and Setting Layout
  plot_ly(
    data = for_function, x = ~ for_function$GDP,
    y = ~ for_function$X..foreign.born.population,
    text = for_function$indicator, hoverinfo = "text", color = ~GDP
  ) %>%
    layout(
      title = "GDP vs % Foreign Born",
      xaxis = list(title = "GDP (Billions)"),
      yaxis = list(title = "% Foreign Born"),
      annotations = list(title = "GDP (Billions of $")
    )
}
