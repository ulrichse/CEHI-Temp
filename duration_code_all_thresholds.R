
library(data.table)
library(dplyr)

getwd()
setwd("C:/Users/ulric/OneDrive - Appalachian State University/Documents/RStudio/CEHI Temp")

# Read the CSV file
percentiles <- read.csv("data/heatwave_percentiles.csv")

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
    two_days_75 = ifelse(above_pct_75 == 1 & lag(above_pct_75) == 1 & lead(above_pct_75) == 0, 1, 0),
    three_days_75 = ifelse(above_pct_75 == 1 & lag(above_pct_75, 2) == 1 & lag(above_pct_75) == 0 & lead(above_pct_75) == 0, 1, 0),
    four_days_75 = ifelse(above_pct_75 == 1 & lag(above_pct_75, 3) == 1 & lag(above_pct_75, 2) == 0 & lag(above_pct_75) == 0 & lead(above_pct_75) == 0, 1, 0)
  ) %>%
  ungroup()

percentiles <- percentiles %>%
  group_by(region_code) %>%
  arrange(Date) %>%
  mutate(
    two_days_85 = ifelse(above_pct_85 == 1 & lag(above_pct_85) == 1 & lead(above_pct_85) == 0, 1, 0),
    three_days_85 = ifelse(above_pct_85 == 1 & lag(above_pct_85, 2) == 1 & lag(above_pct_85) == 0 & lead(above_pct_85) == 0, 1, 0),
    four_days_85 = ifelse(above_pct_85 == 1 & lag(above_pct_85, 3) == 1 & lag(above_pct_85, 2) == 0 & lag(above_pct_85) == 0 & lead(above_pct_85) == 0, 1, 0)
  ) %>%twoabove_pct_90  0
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
    two_days_975 = ifelse(above_pct_975 == 1 & lag(above_pct_975) == 1 & lead(above_pct_975) == 0, 1, 0),
    three_days_975 = ifelse(above_pct_975 == 1 & lag(above_pct_975, 2) == 1 & lag(above_pct_975) == 0 & lead(above_pct_975) == 0, 1, 0),
    four_days_975 = ifelse(above_pct_975 == 1 & lag(above_pct_975, 3) == 1 & lag(above_pct_975, 2) == 0 & lag(above_pct_975) == 0 & lead(above_pct_975) == 0, 1, 0)
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
fwrite(percentiles, "data/heatwave_duration_all_percentiles.csv")






