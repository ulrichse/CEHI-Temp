
library(data.table)
library(lubridate)

setwd("C:/Users/ulric/OneDrive - Appalachian State University/Documents/RStudio/CEHI Temp")
getwd()

###DATA PREP####

df <- read.csv("data/Final Temp Datasets/full_nc_temp.csv")
df <- df %>% #Remove X column 
  select(-X)
setDT(df)

# Convert region_code to character to avoid type mismatch
df[, region_code := as.character(region_code)]

df[Temperature == -999.99, Temperature := NA]

## Filter temp date for 2011-2019

df$Date2 <- as.Date(df$Date, format = "%Y-%d-%m")
temp_filter <- df %>%
  filter(Date2 >= as.Date("2011-01-01") & Date2 <= as.Date("2019-12-31"))

# Function to calculate the region-specific percentile threshold 
calculate_region_percentile_threshold <- function(data) {
  unique_regions <- unique(data$region_code)
  percentiles <- c(0.85, 0.9, 0.95, 0.99)
  results <- data.frame(region_code = unique_regions)
  
  for (percentile in percentiles) {
    percentile_column <- character(length(unique_regions))
    for (i in seq_along(unique_regions)) {
      region_code <- unique_regions[i]
      temperatures <- data[data$region_code == region_code, ]$Temperature
      percentile_value <- quantile(temperatures, probs = percentile, na.rm = TRUE)
      percentile_column[i] <- percentile_value
    }
    column_name <- paste0("percentile_", percentile * 100)
    results[column_name] <- as.numeric(percentile_column)
  }
  
  return(results)
}

result <- calculate_region_percentile_threshold(temp_filter)

temp_filter2 <- temp_filter %>%
  left_join(result, by=c('region_code'))

write.csv(temp_filter2, "data/thresholds_2011_2019.csv")

temp_filter2$Date_chr <- temp_filter2$Date
temp_filter2$Date <- temp_filter2$Date2
temp_filter2 <- temp_filter2 %>%
  select(-c(Date_chr, Date2))

# Calculate above_pct_thresholds

your_dataframe <- your_dataframe %>%
  mutate(above_threshold = ifelse(daily_temperature > threshold_temperature, 1, 0))

# If you prefer TRUE/FALSE instead of 1/0, you can use:

temp_filter2 <- temp_filter2 %>%
  mutate(
    above_pct_85 = ifelse(Temperature > percentile_85, 1, 0),
    above_pct_90 = ifelse(Temperature > percentile_90, 1, 0),
    above_pct_95 = ifelse(Temperature > percentile_95, 1, 0),
    above_pct_99 = ifelse(Temperature > percentile_99, 1, 0),
  )

percentiles <- temp_filter2

percentiles <- percentiles %>%
  group_by(region_code) %>%
  arrange(Date) %>%
  mutate(
    two_days_85 = ifelse(above_pct_85 == 1 & lag(above_pct_85) == 1 & lead(above_pct_85) == 0, 1, 0),
    three_days_85 = ifelse(above_pct_85 == 1 & lag(above_pct_85, 2) == 1 & lag(above_pct_85) == 0 & lead(above_pct_85) == 0, 1, 0),
    four_days_85 = ifelse(above_pct_85 == 1 & lag(above_pct_85, 3) == 1 & lag(above_pct_85, 2) == 0 & lag(above_pct_85) == 0 & lead(above_pct_85) == 0, 1, 0)
  ) %>%
  ungroup()

percentiles <- percentiles %>%
  group_by(region_code) %>%
  arrange(Date) %>%
  mutate(
    two_days_90 = ifelse(above_pct_90 == 1 & lag(above_pct_90) == 1 & lead(above_pct_90) == 0, 1, 0),
    three_days_90 = ifelse(above_pct_90 == 1 & lag(above_pct_90, 2) == 1 & lag(above_pct_90) == 0 & lead(above_pct_90) == 0, 1, 0),
    four_days_90 = ifelse(above_pct_90 == 1 & lag(above_pct_90, 3) == 1 & lag(above_pct_90, 2) == 0 & lag(above_pct_90) == 0 & lead(above_pct_90) == 0, 1, 0)
  ) %>%
  ungroup()

percentiles <- percentiles %>%
  group_by(region_code) %>%
  arrange(Date) %>%
  mutate(
    two_days_95 = ifelse(above_pct_95 == 1 & lag(above_pct_95) == 1 & lead(above_pct_95) == 0, 1, 0),
    three_days_95 = ifelse(above_pct_95 == 1 & lag(above_pct_95, 2) == 1 & lag(above_pct_95) == 0 & lead(above_pct_95) == 0, 1, 0),
    four_days_95 = ifelse(above_pct_95 == 1 & lag(above_pct_95, 3) == 1 & lag(above_pct_95, 2) == 0 & lag(above_pct_95) == 0 & lead(above_pct_95) == 0, 1, 0)
  ) %>%
  ungroup()

percentiles <- percentiles %>%
  group_by(region_code) %>%
  arrange(Date) %>%
  mutate(
    two_days_99 = ifelse(above_pct_99 == 1 & lag(above_pct_99) == 1 & lead(above_pct_99) == 0, 1, 0),
    three_days_99 = ifelse(above_pct_99 == 1 & lag(above_pct_99, 2) == 1 & lag(above_pct_99) == 0 & lead(above_pct_99) == 0, 1, 0),
    four_days_99 = ifelse(above_pct_99 == 1 & lag(above_pct_99, 3) == 1 & lag(above_pct_99, 2) == 0 & lag(above_pct_99) == 0 & lead(above_pct_99) == 0, 1, 0)
  ) %>%
  ungroup()

# Print the updated dataframe
head(percentiles)
fwrite(percentiles, "data/heatwave_duration_thresholds_2011_2019.csv")






