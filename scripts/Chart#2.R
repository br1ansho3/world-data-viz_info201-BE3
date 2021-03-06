get_second_chart <- function(data) {
  data <- data[-1:-4, ]
  data_interest <- data %>%
    select(
      indicator, health.expenditure,
      human.development.index
    ) %>%
    filter(!is.na(health.expenditure) & !is.na(human.development.index)) %>%
    mutate(
      health_expenditure = as.numeric(gsub(",", "", health.expenditure)),
      human_development_index = as.numeric(human.development.index)
    )
  g <- ggplot(data = data_interest) +
    geom_col(mapping = aes(
      x = indicator, y = human_development_index,
      fill = health_expenditure
    )) +
    theme(
      axis.title.x = element_blank(), axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    ) +
    labs(fill = "Health Expenditure 2015", y = "Human Development Index 2017") +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1))
  ggplotly(g)
}
