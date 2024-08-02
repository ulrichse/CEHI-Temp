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

df[Temperature == -999.99, Temperature := NA] # Replace missing values with NA

long_format_data <- df

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

#Test code on a sample
set.seed(123) 
nc_sample <- long_format_data[sample(nrow(long_format_data), size = 10000, replace = FALSE), ]

result <- calculate_region_percentile_threshold(long_format_data)



