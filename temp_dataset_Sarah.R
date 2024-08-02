
##Code to create min temp dataset

library(dplyr)
library(readr)

getwd()
setwd("C:/Users/ulric/OneDrive - Appalachian State University/Documents/RStudio/CEHI Temp")

#Read in temp files and create data frame#######################################

# Path to the directory containing CSV files
folder_path <- "C:/Users/ulric/OneDrive - Appalachian State University/Documents/RStudio/CEHI Temp/data/Final Temp Datasets"  # Replace with your folder path

# List all csv files for NC
file_list <- list.files(path = folder_path, pattern = "NC.*\\.csv", full.names = TRUE)

# Initialize an empty data frame to store appended tmax data
nc_data <- data.frame()

# Read and append data from each file
for (file in file_list) {
  file_data <- read_csv(file)  # Read the CSV file
  
  # Append data to tmax_data
  nc_data <- bind_rows(nc_data, file_data)
  
  # Print progress information
  cat(paste("Appended file", file, "\n"))
}

# Display summary or perform further operations with nc_data
summary(nc_data)

#Write off merged file
write.csv(nc_data, "data/Final Temp Datasets/full_nc_temp.csv")

###Calculate heat events with 90 and 95 percentile thresholds###################

library(data.table)

# Assuming 'long_format_data' contains the formatted data as described in the previous code

# Function to calculate the 90th percentile threshold for each day within the window
calculate_95th_percentile <- function(data, target_date) {
  window_start <- target_date - lubridate::days(15)
  window_end <- target_date + lubridate::days(15)
  
  window_data <- data[Date >= window_start & Date <= window_end]
  percentile_95 <- quantile(window_data$Temperature, probs = 0.95, na.rm = TRUE)
  
  return(percentile_95)
}

# Function to identify heatwave events based on the defined criteria
identify_heatwaves <- function(data) {
  data[, Heatwave := 0]  # Initialize Heatwave column
  
  for (i in 1:nrow(data)) {
    
    target_date <- data$Date[i]
    window_start <- target_date - lubridate::days(15)
    window_end <- target_date + lubridate::days(15)
    
    window_data <- data[Date >= window_start & Date <= window_end]
    if (nrow(window_data) < 3) next  # Skip if window size is less than 3
    
    threshold <- calculate_95th_percentile(data, target_date)
    
    consecutive_days_above_threshold <- sum(window_data$Temperature > threshold)
    
    if (consecutive_days_above_threshold >= 3) {
      data[i:(i + 2), Heatwave := 1]  # Mark as heatwave event
    }
  }
  
  return(data)
}

# Identify heatwave events in the data

nc_avg <- nc_data %>% #Filter for avg temp
  filter(Variable=="TAVG")
setDT(nc_avg)

#Sample dataset to test code
set.seed(123)  # Set a seed for reproducibility
nc_sample <- nc_avg[sample(nrow(nc_avg), size = 1000, replace = FALSE), ]

data_with_heatwaves <- identify_heatwaves(long_format_data)

table(data_with_heatwaves$Heatwave)

##THRESHOLDS OVER ENTIRE TIME PERIOD############################################

# View data with heatwave events marked

# Function to identify heatwave events based on the defined criteria
identify_heatwaves_test <- function(data) {
  data[, Heatwave := 0]  # Initialize Heatwave column
  
  for (i in 1:nrow(data)) {
    
    target_date <- data$Date[i]
    window_start <- target_date - lubridate::days(15)
    window_end <- target_date + lubridate::days(15)
    
    window_data <- data[Date >= window_start & Date <= window_end]
    if (nrow(window_data) < 3) next  # Skip if window size is less than 3
    
    threshold <- quantile(data$Temperature, probs = 0.95, na.rm = TRUE)
    
    consecutive_days_above_threshold <- sum(window_data$Temperature > threshold)
    
    if (consecutive_days_above_threshold >= 3) {
      data[i:(i + 2), Heatwave := 1]  # Mark as heatwave event
    }
  }
  
  return(data)
}

table(nc_sample$Heatwave)

