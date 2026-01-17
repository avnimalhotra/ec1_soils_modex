###############################################################################
# Title: Total Soil Carbon Predictors by Region and Transect Location
# Author: Avni Malhotra
# Date: January 16, 2026
#
# Purpose:
#   1. Load and clean soil data from EXCHANGE-derived merged dataset.
#   2. PART A: Region-focused models (Great Lakes vs Mid-Atlantic)
#      - Predict carbon_weight_perc using:
#          bulk_density_g_cm3, percent_sand, ph, K_meq_100g, transect_location
#      - Fit separate models by region.
#      - Extract R², F-statistic, per-predictor coefficients, p-values, VIF,
#        and standardized beta coefficients.
#      - Plot standardized betas by predictor and region.
#      - Save summary tables and figures for paper.
#   3. PART B: Transect-focused models (upland, transition, wetland) where regions are merged
#      - Predict carbon_weight_perc using:
#          bulk_density_g_cm3, Ca_meq_100g, ph, percent_clay,
#          specific_conductance_us_cm
#      - Fit separate models by transect_location.
#      - Extract model metrics (R², F-test p), standardized betas.
#      - Plot standardized betas and model metrics by transect.
#      - Save summary tables and figures for paper.
###############################################################################

########################
# 0. Load Dependencies #
########################
library(tidyverse)   # dplyr, ggplot2, etc.
library(car)         # VIF
library(broom)       # Tidy model outputs (optional)
library(patchwork)   # Arrange multiple ggplots

################################
# 1. Paths and Data Import     #
################################

# Path to merged dataset (updated)
merged_file_path <- "/Users/malh455/Library/CloudStorage/OneDrive-PNNL/Documents/projects/compass/ec1_soils/ec1_modex_paper/data/processed/merged.csv"

# Output directories for paper-ready results
fig_paper_dir  <- "/Users/malh455/Library/CloudStorage/OneDrive-PNNL/Documents/projects/compass/ec1_soils/ec1_modex_paper/results/figures/paper"
tab_paper_dir  <- "/Users/malh455/Library/CloudStorage/OneDrive-PNNL/Documents/projects/compass/ec1_soils/ec1_modex_paper/results/tables"

# Load data
merged_data <- read.csv(merged_file_path)

###############################################################################
# PART A: REGION-FOCUSED MODELS (Great Lakes vs Mid-Atlantic)
###############################################################################

############################################
# 2A. Filter Data for Region-Focused Models
############################################

region_filtered_data <- merged_data %>%
  filter(region %in% c("Great Lakes", "Mid-Atlantic")) %>%
  filter(
    !is.na(bulk_density_g_cm3),
    !is.na(transect_location),
    !is.na(percent_sand),
    !is.na(ph),
    !is.na(K_meq_100g),
    !is.na(carbon_weight_perc)
  )

# Split by region
great_lakes_data  <- region_filtered_data %>% filter(region == "Great Lakes")
mid_atlantic_data <- region_filtered_data %>% filter(region == "Mid-Atlantic")

######################################################
# 3A. Function to Build Region-Specific Linear Models
######################################################

build_region_model <- function(data, region_name) {
  # Standardize numeric predictors (z-scores)
  standardized_data <- data %>%
    mutate(
      across(
        all_of(c("bulk_density_g_cm3", "percent_sand", "ph", "K_meq_100g")),
        ~ scale(.),
        .names = "std_{col}"
      )
    )
  
  # Model formula:
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
  
  model <- lm(formula, data = standardized_data)
  
  # Basic model stats
  model_r2 <- summary(model)$r.squared
  f_stats  <- summary(model)$fstatistic
  f_value  <- unname(f_stats[1])
  f_df1    <- unname(f_stats[2])
  f_df2    <- unname(f_stats[3])
  f_p      <- pf(f_value, f_df1, f_df2, lower.tail = FALSE)
  
  # VIF (for non-aliased predictors)
  vif_values <- car::vif(model)
  
  # Per-predictor coefficient summary
  model_summary <- summary(model)$coefficients %>%
    as.data.frame() %>%
    rownames_to_column(var = "Predictor") %>%
    rename(
      Estimate  = Estimate,
      Std.Error = `Std. Error`,
      t.value   = `t value`,
      P.Value   = `Pr(>|t|)`
    ) %>%
    filter(Predictor != "(Intercept)") %>%
    mutate(
      Std.Beta = abs(Estimate),
      VIF      = ifelse(Predictor %in% names(vif_values),
                        vif_values[Predictor], NA_real_),
      R2        = model_r2,
      F_value   = f_value,
      F_p_value = f_p,
      Region    = region_name
    )
  
  list(
    model         = model,
    summary_table = model_summary
  )
}

####################################
# 4A. Fit Models for Both Regions  #
####################################

gl_res <- build_region_model(great_lakes_data,  "Great Lakes")
ma_res <- build_region_model(mid_atlantic_data, "Mid-Atlantic")

great_lakes_model    <- gl_res$model
mid_atlantic_model   <- ma_res$model
great_lakes_summary  <- gl_res$summary_table
mid_atlantic_summary <- ma_res$summary_table

# Combined summary for plotting/tables
region_combined_summary <- bind_rows(great_lakes_summary, mid_atlantic_summary)

# Print to console (optional)
cat("\n--- Great Lakes Region Model Summary ---\n")
print(great_lakes_summary)
cat("\n--- Mid-Atlantic Region Model Summary ---\n")
print(mid_atlantic_summary)

#########################################
# 5A. Region Models: P-value Tables etc.
#########################################

# Per-predictor p-values (already in summaries, but provide a simpler table)
region_pvalues <- region_combined_summary %>%
  select(Region, Predictor, P.Value) %>%
  arrange(Region, Predictor)

# Overall model stats (R², F, p) by region (one row per region)
region_model_stats <- region_combined_summary %>%
  select(Region, R2, F_value, F_p_value) %>%
  distinct()

# Save region-level tables
write.csv(
  region_combined_summary,
  file = file.path(tab_paper_dir, "tc_predictors_region_full_summary.csv"),
  row.names = FALSE
)

write.csv(
  region_pvalues,
  file = file.path(tab_paper_dir, "tc_predictors_region_pvalues.csv"),
  row.names = FALSE
)

write.csv(
  region_model_stats,
  file = file.path(tab_paper_dir, "tc_predictors_region_model_stats.csv"),
  row.names = FALSE
)

##########################################################
# 6A. Plot: Standardized Betas by Predictor and Region   #
##########################################################

# Relabel predictors for nicer plotting
region_combined_labeled <- region_combined_summary %>%
  mutate(
    Predictor_label = case_when(
      Predictor == "std_bulk_density_g_cm3"      ~ "Bulk Density",
      Predictor == "std_percent_sand"            ~ "Sand",
      Predictor == "std_ph"                      ~ "pH",
      Predictor == "std_K_meq_100g"              ~ "Potassium",
      Predictor == "transect_locationupland"     ~ "Transect location (upland)",
      Predictor == "transect_locationtransition" ~ "Transect location (transition)",
      Predictor == "transect_locationwetland"    ~ "Transect location (wetland)",
      TRUE                                       ~ Predictor
    )
  )

# Bar plot of standardized betas by region (no title)
beta_region_plot <- ggplot(
  data = region_combined_labeled,
  aes(x = Std.Beta,
      y = reorder(Predictor_label, Std.Beta),
      fill = Region)
) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(
    x = "Standardized beta coefficient (|Std.Beta|)",
    y = "Predictor"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.x = element_text(face = "bold", size = 14),
    axis.title.y = element_text(face = "bold", size = 14)
  )

# Save region beta plot as paper-ready PDF
ggsave(
  filename = file.path(fig_paper_dir, "tc_predictors_standardized_betas_by_region.pdf"),
  plot     = beta_region_plot,
  width    = 7,
  height   = 4,
  units    = "in"
)

###############################################################################
# PART B: TRANSECT-FOCUSED MODELS (Upland, Transition, Wetland)
###############################################################################

###############################################
# 7B. Filter Data for Transect-Focused Models
###############################################

transect_filtered_data <- merged_data %>%
  filter(
    !is.na(bulk_density_g_cm3),
    !is.na(Ca_meq_100g),
    !is.na(ph),
    !is.na(percent_clay),
    !is.na(specific_conductance_us_cm),
    !is.na(carbon_weight_perc),
    !is.na(transect_location)
  )

# Split by transect location
upland_data     <- transect_filtered_data %>% filter(transect_location == "upland")
transition_data <- transect_filtered_data %>% filter(transect_location == "transition")
wetland_data    <- transect_filtered_data %>% filter(transect_location == "wetland")

# Predictors used in transect-focused models
transect_predictors <- c(
  "bulk_density_g_cm3",
  "Ca_meq_100g",
  "ph",
  "percent_clay",
  "specific_conductance_us_cm"
)

# Optional: correlation matrix for these predictors (overall)
correlation_matrix <- transect_filtered_data %>%
  select(all_of(transect_predictors)) %>%
  cor(use = "pairwise.complete.obs")

cat("\nCorrelation matrix among transect predictors:\n")
print(correlation_matrix)

#########################################################
# 8B. Function to Build Transect-Specific Linear Models #
#########################################################

build_transect_model <- function(data, transect_name) {
  # Standardize numeric predictors
  standardized_data <- data %>%
    mutate(across(all_of(transect_predictors), ~ scale(.), .names = "std_{col}"))
  
  # Build formula:
  # carbon_weight_perc ~ std_bulk_density_g_cm3 + std_Ca_meq_100g + ...
  formula <- as.formula(
    paste(
      "carbon_weight_perc ~",
      paste(names(standardized_data)[grepl("^std_", names(standardized_data))],
            collapse = " + ")
    )
  )
  
  model <- lm(formula, data = standardized_data)
  
  # Model stats
  model_r2 <- summary(model)$r.squared
  f_stats  <- summary(model)$fstatistic
  f_value  <- f_stats[1]
  f_df1    <- f_stats[2]
  f_df2    <- f_stats[3]
  f_p      <- pf(f_value, f_df1, f_df2, lower.tail = FALSE)
  
  # VIF (handle possible aliasing errors)
  vif_values <- tryCatch(
    car::vif(model),
    error = function(e) {
      warning("VIF calculation failed due to aliased coefficients.")
      rep(NA_real_, length(transect_predictors))
    }
  )
  
  # Per-predictor coefficients
  model_summary <- summary(model)$coefficients %>%
    as.data.frame() %>%
    rownames_to_column(var = "Predictor") %>%
    rename(
      Estimate  = Estimate,
      Std.Error = `Std. Error`,
      t.value   = `t value`,
      P.Value   = `Pr(>|t|)`
    ) %>%
    filter(Predictor != "(Intercept)") %>%
    mutate(
      Std.Beta = abs(Estimate),
      VIF      = ifelse(Predictor %in% names(vif_values),
                        vif_values[Predictor], NA_real_),
      R2        = model_r2,
      F_value   = f_value,
      F_p_value = f_p,
      Transect  = transect_name
    )
  
  metrics <- data.frame(
    Transect  = transect_name,
    R2        = model_r2,
    F_p_value = f_p
  )
  
  list(
    model         = model,
    summary_table = model_summary,
    metrics       = metrics
  )
}

############################################
# 9B. Fit Models for Upland/Transition/etc #
############################################

upland_res     <- build_transect_model(upland_data,     "Upland")
transition_res <- build_transect_model(transition_data, "Transition")

# Wetland model may fail; protect with tryCatch
wetland_res <- tryCatch(
  build_transect_model(wetland_data, "Wetland"),
  error = function(e) {
    warning("Wetland model failed to build: ", conditionMessage(e))
    NULL
  }
)

# Combine predictor-level results
transect_combined_summary <- bind_rows(
  upland_res$summary_table,
  transition_res$summary_table,
  if (!is.null(wetland_res)) wetland_res$summary_table else NULL
)

# Combine metrics (R², F p)
transect_model_metrics <- bind_rows(
  upland_res$metrics,
  transition_res$metrics,
  if (!is.null(wetland_res)) wetland_res$metrics else NULL
)

# Print to console (optional)
cat("\n--- Upland model summary ---\n")
print(upland_res$summary_table)
cat("\n--- Transition model summary ---\n")
print(transition_res$summary_table)
if (!is.null(wetland_res)) {
  cat("\n--- Wetland model summary ---\n")
  print(wetland_res$summary_table)
} else {
  cat("\nWetland model failed to build.\n")
}

cat("\n--- Transect-level model metrics (R² and F-test p-value) ---\n")
print(transect_model_metrics)

###########################################
# 10B. Save Transect Tables for the Paper #
###########################################

# Save full predictor-level summary by transect
write.csv(
  transect_combined_summary,
  file = file.path(tab_paper_dir, "tc_predictors_transect_full_summary.csv"),
  row.names = FALSE
)

# Save only model metrics (R² and F p) by transect
write.csv(
  transect_model_metrics,
  file = file.path(tab_paper_dir, "tc_predictors_transect_model_metrics.csv"),
  row.names = FALSE
)

###############################################
# 11B. Plots: Standardized Betas & Model Stats
###############################################

# Relabel predictors for nicer plotting
transect_combined_labeled <- transect_combined_summary %>%
  mutate(
    Predictor_label = case_when(
      Predictor == "std_bulk_density_g_cm3"         ~ "Bulk Density",
      Predictor == "std_Ca_meq_100g"                ~ "Calcium",
      Predictor == "std_ph"                         ~ "pH",
      Predictor == "std_percent_clay"               ~ "Clay",
      Predictor == "std_specific_conductance_us_cm" ~ "Spc. conductance",
      TRUE                                          ~ Predictor
    )
  )

# Plot: standardized betas by transect (no title)
beta_transect_plot <- ggplot(
  transect_combined_labeled,
  aes(x = Std.Beta,
      y = reorder(Predictor_label, Std.Beta),
      fill = Transect)
) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(
    x = "Standardized beta coefficient (|Std.Beta|)",
    y = "Predictor"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.x = element_text(face = "bold", size = 14),
    axis.title.y = element_text(face = "bold", size = 14)
  )

# Plot: R² by transect (no title)
r2_transect_plot <- ggplot(
  transect_model_metrics,
  aes(x = Transect, y = R2, fill = Transect)
) +
  geom_bar(stat = "identity", width = 0.6) +
  labs(
    x = "Transect location",
    y = "R²"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.x = element_text(face = "bold", size = 14),
    axis.title.y = element_text(face = "bold", size = 14)
  )

# Plot: F-test p-value by transect (no title)
f_transect_plot <- ggplot(
  transect_model_metrics,
  aes(x = Transect, y = F_p_value, fill = Transect)
) +
  geom_bar(stat = "identity", width = 0.6) +
  labs(
    x = "Transect location",
    y = "p-value (F-test)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.x = element_text(face = "bold", size = 14),
    axis.title.y = element_text(face = "bold", size = 14)
  )

# Combine R² and F-test plots side-by-side (no overall title)
transect_metrics_plot <- r2_transect_plot + f_transect_plot +
  plot_layout(ncol = 2)

# Save plots as paper-ready PDFs
ggsave(
  filename = file.path(fig_paper_dir, "tc_predictors_standardized_betas_by_transect.pdf"),
  plot     = beta_transect_plot,
  width    = 7,
  height   = 4,
  units    = "in"
)

ggsave(
  filename = file.path(fig_paper_dir, "tc_predictors_transect_model_performance.pdf"),
  plot     = transect_metrics_plot,
  width    = 7,
  height   = 4,
  units    = "in"
)

###############################################################################
# End of combined script
###############################################################################