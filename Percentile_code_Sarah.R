
library(data.table)
library(lubridate)
library(dplyr)

getwd()
setwd("C:/Users/ulric/OneDrive - Appalachian State University/Documents/RStudio/CEHI Temp")

# Read the CSV file
df <- fread("D:/Final Temp Datasets/avgTemp_NC.csv")
names(df)

# Convert Date to Date type
df$Date <- as.Date(df$Date)

# Create a function to calculate percentiles for each region
calculate_heatwave <- function(df, region_column, temperature_column, percentiles) {
  df_heatwave <- df %>%
    group_by(across(all_of(region_column))) %>%
    mutate(
      pct_75 = quantile(Temperature, 0.75, na.rm = TRUE),
      pct_85 = quantile(Temperature, 0.85, na.rm = TRUE),
      pct_90 = quantile(Temperature, 0.90, na.rm = TRUE),
      pct_95 = quantile(Temperature, 0.95, na.rm = TRUE),
      pct_975 = quantile(Temperature, 0.975, na.rm = TRUE),
      pct_99 = quantile(Temperature, 0.99, na.rm = TRUE),
      above_pct_75 = as.numeric(Temperature > pct_75),
      above_pct_85 = as.numeric(Temperature > pct_85),
      above_pct_90 = as.numeric(Temperature > pct_90),
      above_pct_95 = as.numeric(Temperature > pct_95),
      above_pct_975 = as.numeric(Temperature > pct_975),
      above_pct_99 = as.numeric(Temperature > pct_99)
    ) %>%
    ungroup()
  
  return(df_heatwave)
}

# Specify the region and temperature columns
region_column <- "region_code"
temperature_column <- "Temperature"

# Specify the percentiles
percentiles <- c(0.75, 0.85, 0.90, 0.95, 0.975, 0.99)

# Calculate heatwave and store the result in a new dataframe
df_heatwave <- calculate_heatwave(df, region_column, temperature_column, percentiles)

# Print the head of the new dataframe
head(df_heatwave)

# Write off percentile csv
write.csv(df_heatwave, "data/heatwave_percentiles.csv")
