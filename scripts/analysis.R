#Where are smaller nations innovating
#Are any outlier countries worth our attention?
#Can we learn anything from the trends and patterns of these dataset?
#Do these countries derive power from their agility? Or is innovation more risky?

#basic information 
#format: basic_info$feature$max$value
summarize_information <- function(df) {
  find_metadata <- function(feature) {
    specific_table <- df[, c("indicator", feature)]
    specific_table[, feature] <- as.numeric(gsub(",", "", df[, feature]))
    specific_table
    max <- specific_table[(specific_table[, feature]) ==
                            max(specific_table[, feature], na.rm = TRUE) &
                            (!is.na((specific_table[, feature]))), ]
    min <- specific_table[(specific_table[, feature]) ==
                            min(specific_table[, feature], na.rm = TRUE) &
                            (!is.na((specific_table[, feature]))), ]
    avg <- mean(specific_table[, feature], na.rm = TRUE)
    list(max = max, min = min, avg = avg)
  }
  find_metadata("total.foreign.born.population.")

  # list of all features
  features <- colnames(small_countries_df)[-1]
  values <- lapply(features, find_metadata)
  names(values) <- features

  #other info
  #countries that have a complete dataset
  complete_rows <- complete.cases(small_countries_df)
  complete_country <- small_countries_df[complete_rows, "indicator"]
  num_complete <- length(complete_country)

  values$other <- list(complete_country = complete_country,
                       num_complete = num_complete)
  values
}

