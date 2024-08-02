library(data.table)
library(dplyr)

# Read the CSV file
percentiles <- fread("D:/Final Temp Datasets/heatwave_percentiles.csv")
###2 days in a row
# Assuming your dataframe is percentiles
percentiles <- percentiles %>%
  group_by(region_code) %>%
  arrange(Date) %>%
  mutate(occurs_two_days_in_row = ifelse(above_pct_90 == 1 & lag(above_pct_90) == 1, 1, 0)) %>%
  ungroup()

# Print the updated dataframe
head(percentiles)
table(percentiles$occurs_two_days_in_row)

# Assuming your dataframe is df_heatwave
fwrite(percentiles, "D:/Final Temp Datasets/heatwave_percentiles_2day_90th.csv")

###2 days and 3 days but only last day labelled

library(data.table)
library(dplyr)

library(data.table)
library(dplyr)

# Read the CSV file
percentiles <- fread("D:/Final Temp Datasets/heatwave_percentiles.csv")

# Assuming your dataframe is percentiles
percentiles <- percentiles %>%
  group_by(region_code) %>%
  arrange(Date) %>%
  mutate(
    occurs_two_days_in_row = ifelse(above_pct_90 == 1 & lag(above_pct_90) == 1 & lead(above_pct_90) == 0, 1, 0),
    occurs_three_days_in_row = ifelse(above_pct_90 == 1 & lag(above_pct_90, 2) == 1 & lag(above_pct_90) == 1 & lead(above_pct_90) == 0, 1, 0),
    occurs_four_days_in_row = ifelse(above_pct_90 == 1 & lag(above_pct_90, 3) == 1 & lag(above_pct_90, 2) == 1 & lag(above_pct_90) == 1 & lead(above_pct_90) == 0, 1, 0)
  ) %>%
  ungroup()

# Print the updated dataframe
head(percentiles)
fwrite(percentiles, "D:/Final Temp Datasets/heatwave_percentiles_2_3_4day_90th.csv")