
###############################################################################
# Title: Total Soil Carbon Predictors by Region and Transect Location
# Author: Avni Malhotra
# Date: January 16, 2026
#
# Purpose:
#   - Use the merged EC1 soils dataset to:
#       * Explore total soil carbon (TC) across transect locations
#         (wetland, transition, upland) for Great Lakes and Mid-Atlantic.
#       * Generate exploratory scatterplots of TC by transect location and region.
#       * Create publication-ready violin plots of TC by transect location and region.
#       * Quantify and export coefficients of variation (CV) for TC:
#           - by region × transect_location
#           - by transect_location (regions combined)
#   - Save figures to:
#       results/figures/exploratory/
#       results/figures/paper/
#     and tables to:
#       results/tables/
###############################################################################

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(gridExtra) # For arranging graphs side by side
library(tidyr)

# Specify the file path for the merged dataset
merged_file_path <- "/Users/malh455/Library/CloudStorage/OneDrive-PNNL/Documents/projects/compass/ec1_soils/ec1_modex_paper/data/processed/merged.csv"

# Load the merged dataset
merged_data <- read.csv(merged_file_path)

# Ensure transect locations are ordered: "wetland", "transition", "upland"
merged_data$transect_location <- factor(
  merged_data$transect_location,
  levels = c("wetland", "transition", "upland")
)

# Filter data for the two specific regions: Great Lakes and Mid-Atlantic
filtered_data <- merged_data %>%
  filter(region %in% c("Great Lakes", "Mid-Atlantic") & 
           !is.na(transect_location) & 
           !is.na(carbon_weight_perc))

# Separate data for each region
great_lakes_data <- filtered_data %>% filter(region == "Great Lakes")
mid_atlantic_data <- filtered_data %>% filter(region == "Mid-Atlantic")

# Create scatter plot for Great Lakes region
great_lakes_plot <- ggplot(great_lakes_data, aes(x = transect_location, y = carbon_weight_perc)) +
  geom_point(alpha = 0.7, color = "blue") +
  labs(
    x = "Transect Location",
    y = "Carbon Weight Percentage",
    title = "Region: Great Lakes"
  ) +
  theme_minimal()

# Create scatter plot for Mid-Atlantic region
mid_atlantic_plot <- ggplot(mid_atlantic_data, aes(x = transect_location, y = carbon_weight_perc)) +
  geom_point(alpha = 0.7, color = "red") +
  labs(
    x = "Transect Location",
    y = "Carbon Weight Percentage",
    title = "Region: Mid-Atlantic"
  ) +
  theme_minimal()

# Arrange plots side by side and assign to an object
tc_scatter <- grid.arrange(great_lakes_plot, mid_atlantic_plot, ncol = 2)

# Save as high-resolution PNG
ggsave(
  filename = "/Users/malh455/Library/CloudStorage/OneDrive-PNNL/Documents/projects/compass/ec1_soils/ec1_modex_paper/results/figures/exploratory/tc_scatter.png",
  plot     = tc_scatter,
  width    = 7,      # 2 panels * ~3.5" each
  height   = 3.5,    # inches
  units    = "in",
  dpi      = 300
)

##### make figure for paper
# Create violin plot for Great Lakes region
great_lakes_violin <- ggplot(great_lakes_data, aes(x = transect_location, y = carbon_weight_perc)) +
  geom_violin(fill = "#E6FAFD", color = NA, alpha = 0.6) +  # Light blue shading (#E6FAFD), no outlines
  geom_point(position = position_jitter(width = 0.1), size = 2, color = "#545454") +  # Dark gray points (#545454)
  labs(
    x = NULL,  # Remove x-axis label
    y = "Total Soil Carbon (%)",
    title = "Great Lakes"
  ) +
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(face = "bold", color = "black"),
    panel.background = element_rect(color = "black", fill = NA, size = 1),  # Black outline for graph panel
    axis.ticks.y = element_line(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Create violin plot for Mid-Atlantic region
mid_atlantic_violin <- ggplot(mid_atlantic_data, aes(x = transect_location, y = carbon_weight_perc)) +
  geom_violin(fill = "#E6FAFD", color = NA, alpha = 0.6) +  # Light blue shading (#E6FAFD), no outlines
  geom_point(position = position_jitter(width = 0.1), size = 2, color = "#545454") +  # Dark gray points (#545454)
  labs(
    x = NULL,  # Remove x-axis label
    y = "",
    title = "Mid-Atlantic"
  ) +
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(face = "bold", color = "black"),
    panel.background = element_rect(color = "black", fill = NA, size = 1),  # Black outline for graph panel
    axis.ticks.y = element_line(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Combine the two plots into one figure, side by side
library(patchwork)  # Use patchwork for arranging multiple plots
final_plot <- great_lakes_violin + mid_atlantic_violin + 
  plot_annotation(title = "") + 
  plot_layout(guides = "collect") & 
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5, color = "black")
  )

# Display the final plot
print(final_plot)

# Save final plot for paper as PDF
ggsave(
  filename = "/Users/malh455/Library/CloudStorage/OneDrive-PNNL/Documents/projects/compass/ec1_soils/ec1_modex_paper/results/figures/paper/tc_violin_by_region.pdf",
  plot     = final_plot,
  width    = 7,      # inches (2 panels side by side)
  height   = 3.5,    # inches
  units    = "in"
)


###table to show cv for each location and region Hypothesis 1 Transitions have the most variability in soil carbon, compared to upland and wetland


# Load necessary libraries
library(dplyr)

# Filter relevant data with no missing values in carbon_weight_perc
filtered_data <- merged_data %>%
  filter(!is.na(carbon_weight_perc) & !is.na(region) & !is.na(transect_location))

# Calculate CV for each region and transect_location
cv_results <- filtered_data %>%
  group_by(region, transect_location) %>%
  summarise(
    mean_tc = mean(carbon_weight_perc, na.rm = TRUE),
    sd_tc = sd(carbon_weight_perc, na.rm = TRUE),
    cv_tc = (sd_tc / mean_tc) * 100
  ) %>%
  arrange(region, transect_location)

# Print the results
print(cv_results)

# Save CV by region and transect_location
write.csv(
  cv_results,
  file = "/Users/malh455/Library/CloudStorage/OneDrive-PNNL/Documents/projects/compass/ec1_soils/ec1_modex_paper/results/tables/cv_region_transect.csv",
  row.names = FALSE
)

####CV for regions combined
# Filter relevant data with no missing values in carbon_weight_perc
combined_data <- merged_data %>%
  filter(!is.na(carbon_weight_perc) & !is.na(transect_location))

# Group by transect_location and calculate CV
cv_results_combined <- combined_data %>%
  group_by(transect_location) %>%
  summarise(
    mean_tc = mean(carbon_weight_perc, na.rm = TRUE),
    sd_tc = sd(carbon_weight_perc, na.rm = TRUE),
    cv_tc = (sd_tc / mean_tc) * 100
  ) %>%
  arrange(transect_location)

# Print the results
print(cv_results_combined)

# Save CV by transect_location (regions combined)
write.csv(
  cv_results_combined,
  file = "/Users/malh455/Library/CloudStorage/OneDrive-PNNL/Documents/projects/compass/ec1_soils/ec1_modex_paper/results/tables/cv_transect.csv",
  row.names = FALSE
)

