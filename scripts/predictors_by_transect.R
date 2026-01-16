# Load necessary libraries
library(tidyverse)
library(car)
library(broom)
library(ggplot2)
library(patchwork)

# Step 1: Read the dataset
merged_file_path <- "/Users/malh455/Library/CloudStorage/OneDrive-PNNL/Documents/projects/compass/ec1_soils/aug 2025 update to v3/ec1_soil_v3/merged.csv"
merged_data <- read.csv(merged_file_path)

# Step 2: Filter dataset for relevant predictors and remove rows with NA values
filtered_data <- merged_data %>%
  filter(!is.na(bulk_density_g_cm3) & !is.na(Ca_meq_100g) & !is.na(ph) &
           !is.na(percent_clay) & !is.na(specific_conductance_us_cm) & !is.na(carbon_weight_perc))

# Step 3: Separate data by transect location
upland_data     <- filtered_data %>% filter(transect_location == "upland")
transition_data <- filtered_data %>% filter(transect_location == "transition")
wetland_data    <- filtered_data %>% filter(transect_location == "wetland")

# Define predictors
predictors <- c("bulk_density_g_cm3", "Ca_meq_100g", "ph", "percent_clay", "specific_conductance_us_cm")

# Correlation matrix check
correlation_matrix <- filtered_data %>%
  select(all_of(predictors)) %>%
  cor(use = "pairwise.complete.obs")
print("Correlation Matrix:")
print(correlation_matrix)

# Function to build regression model and calculate metrics
build_model <- function(data, transect_name) {
  standardized_data <- data %>%
    mutate(across(all_of(predictors), ~ scale(.), .names = "std_{col}"))
  
  formula <- as.formula(
    paste("carbon_weight_perc ~",
          paste(names(standardized_data)[grepl("^std_", names(standardized_data))],
                collapse = "+"))
  )
  
  model <- lm(formula, data = standardized_data)
  
  # R²
  model_r2 <- summary(model)$r.squared
  
  # F-statistic and p-value
  f_statistic <- summary(model)$fstatistic
  f_value <- f_statistic[1]
  f_df1   <- f_statistic[2]
  f_df2   <- f_statistic[3]
  f_p_value <- pf(f_value, f_df1, f_df2, lower.tail = FALSE)
  
  # VIF
  vif_values <- tryCatch({
    car::vif(model)
  }, error = function(e) {
    warning("VIF calculation failed due to aliased coefficients.")
    return(rep(NA, length(predictors)))
  })
  
  # Coefficient summary
  model_summary <- summary(model)$coefficients %>%
    as.data.frame() %>%
    rownames_to_column(var = "Predictor") %>%
    rename(Estimate = Estimate, Std.Error = `Std. Error`,
           t.value = `t value`, P.Value = `Pr(>|t|)`) %>%
    filter(Predictor != "(Intercept)") %>%
    mutate(Std.Beta = abs(Estimate))
  
  model_summary <- model_summary %>%
    mutate(VIF = ifelse(Predictor %in% names(vif_values), vif_values[Predictor], NA),
           R2 = model_r2,
           F_value = f_value,
           F_p_value = f_p_value,
           Transect = transect_name)
  
  # Return both: full summary and scalar metrics for plotting
  list(
    summary_table = model_summary,
    metrics = data.frame(
      Transect   = transect_name,
      R2         = model_r2,
      F_p_value  = f_p_value
    ),
    model = model
  )
}

# Build models for upland, transition, wetland
upland_res     <- build_model(upland_data,     "Upland")
transition_res <- build_model(transition_data, "Transition")
wetland_res    <- tryCatch(build_model(wetland_data, "Wetland"), error = function(e) NULL)

# Combine summaries for predictor-level analysis
combined_summary <- bind_rows(
  upland_res$summary_table,
  transition_res$summary_table,
  if (!is.null(wetland_res)) wetland_res$summary_table else NULL
)

# Print summaries
cat("\n--- Upland Model Summary ---\n");     print(upland_res$summary_table)
cat("\n--- Transition Model Summary ---\n"); print(transition_res$summary_table)
if (!is.null(wetland_res)) {
  cat("\n--- Wetland Model Summary ---\n"); print(wetland_res$summary_table)
} else {
  cat("\nWetland model failed to build.\n")
}

# ========= Plot 1: Standardized beta coefficients =========
beta_plot <- ggplot(combined_summary,
                    aes(x = Std.Beta,
                        y = reorder(Predictor, Std.Beta),
                        fill = Transect)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Standardized Beta Coefficients Across Transects",
       x = "Standardized Beta Coefficients (Std.Beta)",
       y = "Predictors",
       fill = "Transect Location") +
  theme_minimal(base_size = 20) +
  theme(
    axis.title = element_text(size = 24, face = "bold"),
    axis.text  = element_text(size = 18),
    plot.title = element_text(size = 28, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 20, face = "bold"),
    legend.text  = element_text(size = 16)
  )

# ========= Build model_metrics automatically (for R² and F p-value) =========
model_metrics <- bind_rows(
  upland_res$metrics,
  transition_res$metrics,
  if (!is.null(wetland_res)) wetland_res$metrics else NULL
)

print("\n--- Model Metrics (R² and F-test p-value) ---")
print(model_metrics)

# ========= Plot 2: R² and F-test p-values =========
r2_plot <- ggplot(model_metrics,
                  aes(x = Transect, y = R2, fill = Transect)) +
  geom_bar(stat = "identity", width = 0.6) +
  labs(title = "R² Across Transect Locations",
       x = "Transect Location",
       y = "R²",
       fill = "Transect Location") +
  theme_minimal(base_size = 15) +
  theme(
    axis.title.x = element_text(size = 18, face = "bold"),
    axis.title.y = element_text(size = 18, face = "bold"),
    axis.text.x  = element_text(size = 14),
    axis.text.y  = element_text(size = 14),
    plot.title   = element_text(size = 20, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 14),
    legend.text  = element_text(size = 12)
  )

f_pvalue_plot <- ggplot(model_metrics,
                        aes(x = Transect, y = F_p_value, fill = Transect)) +
  geom_bar(stat = "identity", width = 0.6) +
  labs(title = "F-Test p-values Across Transect Locations",
       x = "Transect Location",
       y = "p-value of F Statistic",
       fill = "Transect Location") +
  theme_minimal(base_size = 15) +
  theme(
    axis.title.x = element_text(size = 18, face = "bold"),
    axis.title.y = element_text(size = 18, face = "bold"),
    axis.text.x  = element_text(size = 14),
    axis.text.y  = element_text(size = 14),
    plot.title   = element_text(size = 20, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 14),
    legend.text  = element_text(size = 12)
  )

# Show R² / p-value plots side by side
combined_metrics_plot <- r2_plot + f_pvalue_plot +
  plot_annotation(title = "R² and F-Test p-value Metrics Across Transects") +
  plot_layout(ncol = 2)

print(beta_plot)
print(combined_metrics_plot)
