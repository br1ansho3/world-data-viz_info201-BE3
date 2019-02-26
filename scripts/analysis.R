#Where are smaller nations innovating
#Are any outlier countries worth our attention?
#Can we learn anything from the trends and patterns of these dataset?
#Do these countries derive power from their agility? Or is innovation more risky?
library(dplyr)

small_countries_df <- read.csv('data/WDVP Datasets - small countries are beautiful.csv', stringsAsFactors = F, na.strings = "-")

#remove first 4 rows
small_countries_df <- small_countries_df[5:nrow(small_countries_df), ]
#remove all rows that have a missing value


#basic information 
#basic_info$feature#max#country/value
find_metadata <- function(df, feature) {
  specific_table <- df %>% 
    select_("indicator", feature) %>% 
    mutate_(feature = gsub(",", "", feature))
  specific_table[ , feature] <- as.numeric(specific_table[ , feature])
  
  max <- specific_table[na.omit(specific_table[, feature]) == 
                          max(specific_table[, feature], na.rm = TRUE), ] 
  
  #returns the vector of the values of the feature and the country
  #returns list max/avg/min with list of country/value
}
print(find_metadata(small_countries_df, "GDP"))

#countries that have a complete dataset
complete_rows <- complete.cases(small_countries_df)
complete_small_countries <- small_countries_df[complete_rows, "indicator"]




#TESTING AREA
hi <- "GDP"
specific_table <- small_countries_df %>% 
  select_("indicator", hi) %>% 
  mutate_("GDP" = gsub(",", "", hi)) 
specific_table[, hi] <- as.numeric(specific_table[ , hi])
max <- specific_table %>% 
  
  filter_("GDP" == max(GDP, na.rm = TRUE))
# comparison 
specific_table[na.omit(specific_table[,hi]) ==max(specific_table[, hi], na.rm = TRUE),]
specific_table[na.omit(specific_table[, hi]) == max(specific_table[, hi], na.rm = TRUE),] 
filter_(specific_table, hi == paste0("max(", hi, ", na.rm = TRUE)")) #doesn't work
