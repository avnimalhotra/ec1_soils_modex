#######this code creates a merged file using the raw ESS dive data for EC1 soils from this data product https://data.ess-dive.lbl.gov/view/doi%3A10.15485%2F1960313 

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(gridExtra) # For arranging graphs side by side
library(tidyr)


# Specify the folder path
folder_path <- "/Users/malh455/Library/CloudStorage/OneDrive-PNNL/Documents/projects/compass/ec1_soils/aug 2025 update to v3/ec1_soil_v3"

# List all CSV files in the folder
file_list <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

# Initialize counters and an empty list to store the datasets
files_with_kit_id <- 0
files_with_transect_location <- 0
data_list <- list()

# Loop through each file
for (file in file_list) {
  # Read the CSV file
  data <- read.csv(file)
  
  # Check if the "kit_id" column exists
  if ("kit_id" %in% colnames(data)) {
    files_with_kit_id <- files_with_kit_id + 1
    
    # Check if "transect_location" column exists
    if ("transect_location" %in% colnames(data)) {
      files_with_transect_location <- files_with_transect_location + 1
    } else {
      # If no "transect_location" column, create rows for each transect location (wetland, transition, upland)
      data <- data %>%
        tidyr::crossing(transect_location = c("wetland", "transition", "upland")) # Expand rows for each transect_location
    }
    
    # Add the dataset to the list
    data_list[[length(data_list) + 1]] <- data
  }
}

# Print the results of the checks
cat("Number of files with 'kit_id' column:", files_with_kit_id, "\n")
cat("Number of files with 'transect_location' column:", files_with_transect_location, "\n")

# Merge all dataframes by "kit_id" and "transect_location"
merged_data <- Reduce(function(d1, d2) merge(d1, d2, by = c("kit_id", "transect_location"), all = TRUE), data_list)

# Write the merged dataframe to a CSV file
write.csv(merged_data, file = file.path(folder_path, "merged.csv"), row.names = FALSE)

cat("Merged file saved to 'merged.csv' in the folder:", folder_path, "\n")