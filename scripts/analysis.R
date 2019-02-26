#Where are smaller nations innovating
#Are any outlier countries worth our attention?
#Can we learn anything from the trends and patterns of these dataset?
#Do these countries derive power from their agility? Or is innovation more risky?
library(dplyr)
library(lazyeval)


small_countries_df <- read.csv('data/WDVP Datasets - small countries are beautiful.csv', stringsAsFactors = F, na.strings = "-")

#remove first 4 rows
small_countries_df <- small_countries_df[5:nrow(small_countries_df), ]
#remove all rows that have a missing value


#basic information 
#basic_info$feature#max#country/value
find_metadata <- function(df, feature) {
  specific_table <- df[, c("indicator", feature)]
  specific_table[, feature] <- as.numeric(gsub(",", "", df[, feature]))
  specific_table
  max <- specific_table[(specific_table[, feature]) == 
                          max(specific_table[, feature], na.rm = TRUE) & 
                          (!is.na((specific_table[, feature]))),]
  min <- specific_table[(specific_table[, feature]) == 
                          min(specific_table[, feature], na.rm = TRUE) & 
                          (!is.na((specific_table[, feature]))),]
  avg <- mean(specific_table[, feature], na.rm = TRUE)
  list(max = max, min = min, avg = avg)
}




#countries that have a complete dataset
complete_rows <- complete.cases(small_countries_df)
complete_small_countries <- small_countries_df[complete_rows, "indicator"]



#TEST AREA
find_metadata <- function(df, feature) {
  specific_table <- df[, c("indicator", feature)]
  specific_table[, feature] <- as.numeric(gsub(",", "", df[, feature]))
  specific_table
  max <- specific_table[(specific_table[, feature]) == 
                          max(specific_table[, feature], na.rm = TRUE) & 
                          (!is.na((specific_table[, feature]))),]
  min <- specific_table[(specific_table[, feature]) == 
                          min(specific_table[, feature], na.rm = TRUE) & 
                          (!is.na((specific_table[, feature]))),]
  avg <- mean(specific_table[, feature], na.rm = TRUE)
  list(max = max, min = min, avg = avg)
}
is.na(specific_table[, feature])
specific_table <- find_metadata(small_countries_df, "GDP.1")
test <- find_metadata(small_countries_df, "GDP.1")
#TESTING AREA
hi <- "GDP.1"
specific_table <- small_countries_df %>% 
  select_("indicator", hi) #%>% 
  #mutate_(hi = gsub(",", "", hi)) 
specific_table[, hi] <- as.numeric(specific_table[ , hi])
max <- specific_table %>% 
  
  filter_("GDP" == max(GDP, na.rm = TRUE))
# comparison 
na.omit(specific_table[,hi]) ==max(specific_table[, hi], na.rm = TRUE)
specific_table[na.omit(specific_table[, hi]) == max(specific_table[, hi], na.rm = TRUE),] 
filter_(specific_table, hi == paste0("max(", hi, ", na.rm = TRUE)")) #doesn't work
