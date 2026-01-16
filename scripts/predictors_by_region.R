###############################################################################
# Title: Regional Soil Carbon Regression Analysis (Great Lakes & Mid-Atlantic)
# Author: Avni Malhotra
# Date: January 16, 2026
#
# Purpose:
#   1. Load, clean, and subset soil data for Great Lakes and Mid-Atlantic.
#   2. Fit multiple linear regression models (per region) predicting
#      carbon_weight_perc from:
#        - bulk_density_g_cm3 (standardized)
#        - percent_sand       (standardized)
#        - ph                 (standardized)
#        - K_meq_100g         (standardized)
#        - transect_location  (categorical)
#   3. Extract:
#        - R²
#        - overall model F-statistic and F-test p-value
#        - per-predictor coefficients, standard errors, t-values, p-values
#        - per-predictor VIF
#        - standardized beta coefficients (absolute value of standardized slopes)
#   4. Create visualizations comparing standardized betas across regions.
#   5. Provide clean tables of per-predictor p-values for each region.
###############################################################################

########################
# 0. Load Dependencies #
########################

library(tidyverse)   # Data manipulation (dplyr, ggplot2, etc.)
library(car)         # Variance Inflation Factor (VIF)
library(broom)       # Tidy model outputs (not heavily used but available)
library(ggplot2)     # Visualization (explicit)
library(patchwork)   # Plot composition (loaded for possible future use)

##############################
# 1. Read and Filter Dataset #
##############################

# Path to merged dataset
merged_file_path <- "/Users/malh455/Library/CloudStorage/OneDrive-PNNL/Documents/projects/compass/ec1_soils/aug 2025 update to v3/ec1_soil_v3/merged.csv"

# Read CSV
merged_data <- read.csv(merged_file_path)

# Filter to regions of interest and remove rows with missing values in predictors/response
filtered_data <- merged_data %>%
  filter(region %in% c("Great Lakes", "Mid-Atlantic")) %>%     # Keep only these regions
  filter(
    !is.na(bulk_density_g_cm3),
    !is.na(transect_location),
    !is.na(percent_sand),
    !is.na(ph),
    !is.na(K_meq_100g),
    !is.na(carbon_weight_perc)                                  # Response variable
  )

# Split into region-specific datasets
great_lakes_data    <- filtered_data %>% filter(region == "Great Lakes")
mid_atlantic_data   <- filtered_data %>% filter(region == "Mid-Atlantic")

# Optional: keep a vector of predictor names for reference (not used directly below)
predictors <- c("bulk_density_g_cm3",
                "transect_location",
                "percent_sand",
                "ph",
                "K_meq_100g")

#############################################
# 2. Function to Build and Summarize Models #
#############################################

# This function:
#   - Standardizes numeric predictors (z-scores)
#   - Fits a linear model:
#       carbon_weight_perc ~ std_bulk_density + std_percent_sand +
#                            std_ph + std_K_meq_100g + transect_location
#   - Extracts:
#       * R²
#       * F-statistic and F-test p-value
#       * Coefficients, SE, t, p
#       * VIF for each predictor
#       * Standardized beta coefficients (absolute value of standardized slopes)
#   - Returns a tidy data frame with one row per predictor (no intercept).

build_model <- function(data, region_name) {
  
  # Standardize numeric predictors: z-scores (mean = 0, sd = 1)
  standardized_data <- data %>%
    mutate(
      across(
        all_of(c("bulk_density_g_cm3", "percent_sand", "ph", "K_meq_100g")),
        ~ scale(.),
        .names = "std_{col}"
      )
    )
  
  # Construct model formula:
  # carbon_weight_perc ~ std_bulk_density_g_cm3 + std_percent_sand +
  #                      std_ph + std_K_meq_100g + transect_location
  formula <- as.formula(
    paste(
      "carbon_weight_perc ~",
      paste(
        names(standardized_data)[grepl("^std_", names(standardized_data))],
        collapse = " + "
      ),
      "+ transect_location"
    )
  )
  
  # Fit linear model
  model <- lm(formula, data = standardized_data)
  
  # Extract R²
  model_r2 <- summary(model)$r.squared
  
  # Extract overall F-statistic (for the full model) and compute p-value
  f_statistic <- summary(model)$fstatistic   # Named vector: value, df1, df2
  f_value     <- unname(f_statistic[1])
  f_df1       <- unname(f_statistic[2])
  f_df2       <- unname(f_statistic[3])
  f_p_value   <- pf(f_value, f_df1, f_df2, lower.tail = FALSE)
  
  # Calculate Variance Inflation Factors (VIF) for each predictor
  vif_values <- car::vif(model)
  
  # Extract coefficient-level details
  model_summary <- summary(model)$coefficients %>%
    as.data.frame() %>%
    rownames_to_column(var = "Predictor") %>%
    rename(
      Estimate  = Estimate,
      Std.Error = `Std. Error`,
      t.value   = `t value`,
      P.Value   = `Pr(>|t|)`
    ) %>%
    filter(Predictor != "(Intercept)") %>%   # Exclude intercept
    mutate(
      # Standardized beta is effectively the coefficient for standardized variables;
      # we take absolute value for easier comparison of magnitude.
      Std.Beta = abs(Estimate)
    )
  
  # Attach VIF to each predictor row (NA for terms not in VIF, e.g., factor levels if not named)
  model_summary <- model_summary %>%
    mutate(
      VIF = ifelse(Predictor %in% names(vif_values),
                   vif_values[Predictor],
                   NA)
    )
  
  # Add region-level statistics (same values repeated for each predictor row)
  model_summary <- model_summary %>%
    mutate(
      R2        = model_r2,
      F_value   = f_value,
      F_p_value = f_p_value,
      Region    = region_name
    )
  
  # Return both the lm object and the tidy summary for downstream use
  return(list(
    model        = model,
    model_summary = model_summary
  ))
}

###################################
# 3. Fit Models for Both Regions  #
###################################

# Great Lakes model
gl_results <- build_model(great_lakes_data, "Great Lakes")
great_lakes_model   <- gl_results$model          # lm object
great_lakes_summary <- gl_results$model_summary  # tidy data frame

# Mid-Atlantic model
ma_results <- build_model(mid_atlantic_data, "Mid-Atlantic")
mid_atlantic_model   <- ma_results$model         # lm object
mid_atlantic_summary <- ma_results$model_summary # tidy data frame

########################################
# 4. Combine and Inspect Model Results #
########################################

# Combine the two regions into one data frame for comparison
combined_summary <- bind_rows(great_lakes_summary, mid_atlantic_summary)

# Print model summaries (per-predictor metrics) to console
cat("\n--- Great Lakes Model Summary (Per-Predictor) ---\n")
print(great_lakes_summary)

cat("\n--- Mid-Atlantic Model Summary (Per-Predictor) ---\n")
print(mid_atlantic_summary)

###########################################################
# 5. Visualization: Standardized Beta Coefficients (Plot) #
###########################################################

# First basic plot (raw predictor names) - optional but kept for completeness
ggplot(
  data = combined_summary,
  aes(x = Std.Beta,
      y = reorder(Predictor, Std.Beta),
      fill = Region)
) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Standardized Beta Coefficients Across Regions (Raw Labels)",
    x     = "Standardized Beta Coefficients (Std.Beta)",
    y     = "Predictors",
    fill  = "Region"
  ) +
  theme_minimal()

############################################################
# 6. Relabel Predictors for Nicer Plot Labels (Poster Use) #
############################################################

# Create user-friendly labels for predictors
combined_summary_labeled <- combined_summary %>%
  mutate(
    Predictor = case_when(
      Predictor == "std_nitrogen_weight_perc"      ~ "Nitrogen",
      Predictor == "std_bulk_density_g_cm3"        ~ "Bulk Density",
      Predictor == "std_percent_sand"              ~ "Sand",
      Predictor == "std_ph"                        ~ "pH",
      Predictor == "std_K_meq_100g"                ~ "Potassium",
      Predictor == "transect_locationupland"       ~ "Transect location (upland)",
      Predictor == "transect_locationtransition"   ~ "Transect location (transition)",
      TRUE                                         ~ Predictor  # Leave unchanged if no mapping
    )
  )

# Plot with nicer labels and larger font sizes for poster readability
ggplot(
  data = combined_summary_labeled,
  aes(x = Std.Beta,
      y = reorder(Predictor, Std.Beta),
      fill = Region)
) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Standardized Beta Coefficients Across Regions",
    x     = "Standardized Beta Coefficients (Std.Beta)",
    y     = "Predictors",
    fill  = "Region"
  ) +
  theme_minimal(base_size = 20) +
  theme(
    axis.title.x = element_text(size = 24, face = "bold"),
    axis.title.y = element_text(size = 24, face = "bold"),
    axis.text.x  = element_text(size = 18),
    axis.text.y  = element_text(size = 18),
    plot.title   = element_text(size = 28, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 20, face = "bold"),
    legend.text  = element_text(size = 16)
  )

###########################################################
# 7. Extract and Print Per-Predictor p-values by Region   #
###########################################################

# Great Lakes: extract predictor-level p-values from the lm object
great_lakes_pvalues <- as.data.frame(summary(great_lakes_model)$coefficients) %>%
  rownames_to_column(var = "Predictor") %>%
  select(Predictor, `Pr(>|t|)`)

cat("\nP-values for Great Lakes model (per predictor):\n")
print(great_lakes_pvalues)

# Mid-Atlantic: extract predictor-level p-values from the lm object
mid_atlantic_pvalues <- as.data.frame(summary(mid_atlantic_model)$coefficients) %>%
  rownames_to_column(var = "Predictor") %>%
  select(Predictor, `Pr(>|t|)`)

cat("\nP-values for Mid-Atlantic model (per predictor):\n")
print(mid_atlantic_pvalues)

####################################################
# 8. Optional: Inspect Overall F-test Information  #
####################################################

# Note: F-statistic and F-test p-values are already included per row in
# great_lakes_summary and mid_atlantic_summary as F_value and F_p_value.
# Here we just print one line per region for quick reference.

gl_F <- unique(great_lakes_summary[, c("R2", "F_value", "F_p_value")])
ma_F <- unique(mid_atlantic_summary[, c("R2", "F_value", "F_p_value")])

cat("\n--- Great Lakes Overall Model Statistics ---\n")
print(gl_F)

cat("\n--- Mid-Atlantic Overall Model Statistics ---\n")
print(ma_F)

###############################################################################
# End of script
###############################################################################
