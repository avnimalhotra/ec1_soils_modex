###### This code makes exploratory figures for all the continuous variables in the merged EC soils file


## Load packages --------------------------------------------------------------
library(tidyverse)

## Read data ------------------------------------------------------------------
dat <- read_csv(
  "/Users/malh455/Library/CloudStorage/OneDrive-PNNL/Documents/projects/compass/ec1_soils/ec1_modex_paper/data/processed/merged.csv"
)

## Basic cleaning / filtering -------------------------------------------------
# Standardize transect_location to lower case (if needed)
dat <- dat %>%
  mutate(
    transect_location = tolower(transect_location),
    transect_location = factor(
      transect_location,
      levels = c("upland", "transition", "wetland", "water", "sediment")
    )
  )

# Keep only rows with region + transect_location and exclude sediment
soil_dat <- dat %>%
  filter(
    !is.na(region),
    !is.na(transect_location),
    transect_location != "sediment"
  )

## Choose continuous variables automatically ----------------------------------
# You'll likely want to drop ID and clearly non-numeric columns before plotting.
# Here we:
#  - keep only numeric columns
#  - then drop obviously uninteresting numeric columns by name.

# Identify all numeric columns
numeric_vars <- soil_dat %>%
  select(where(is.numeric)) %>%
  names()

# Columns to EXCLUDE from plotting even if numeric
exclude_vars <- c(
  "latitude", "longitude", "elevation_m",
  "n", "alpha", "th_r", "th_s",
  "water_dissolved_o2_mg_l", "water_orp_mv", "water_ph",
  "upland_soil_temperature_degC", "transition_soil_temperature_degC",
  "wetland_soil_temperature_degC", "upland_soil_moisture",
  "transition_soil_moisture", "wetland_soil_moisture",
  "air_temperature_degC", "barometric_pressure_inhg",
  "water_color_forel.ule_scale", "volume_ml"
)

cont_vars <- setdiff(numeric_vars, exclude_vars)

# If there are specific soil vars you definitely want, you could intersect
# this with a "whitelist", e.g.:
# keep_vars <- c("bulk_density_g_cm3", "ph", "carbon_weight_perc", 
#                "nitrogen_weight_perc", "percent_clay", "percent_sand",
#                "percent_silt", "CEC_meq_100g", "specific_conductance_us_cm")
# cont_vars <- intersect(cont_vars, keep_vars)

## Long-format for plotting ---------------------------------------------------
plot_dat <- soil_dat %>%
  select(region, transect_location, all_of(cont_vars)) %>%
  pivot_longer(
    cols = all_of(cont_vars),
    names_to = "variable",
    values_to = "value"
  ) %>%
  # Drop NAs per variable
  filter(!is.na(value)) %>%
  # Drop any weird transect locations that slipped in
  filter(transect_location %in% c("upland", "transition", "wetland", "water")) %>%
  droplevels()

## A general violin plotting function ----------------------------------------
plot_violin_region_transect <- function(df, var_name) {
  df_var <- df %>% filter(variable == var_name)
  
  if (nrow(df_var) == 0) {
    warning(glue::glue("No non-NA data for variable `{var_name}`; skipping."))
    return(NULL)
  }
  
  ggplot(df_var, aes(x = transect_location, y = value, fill = region)) +
    geom_violin(
      alpha = 0.6,
      color = "gray30",
      trim  = FALSE
    ) +
    # Optional: add median point for each group
    stat_summary(
      fun = median,
      geom = "point",
      position = position_dodge(width = 0.9),
      size = 1.5,
      color = "black"
    ) +
    facet_wrap(~ region, scales = "free_y") +
    labs(
      x = "Transect position",
      y = var_name,
      title = paste("Distribution of", var_name, "by region and transect location")
    ) +
    theme_bw() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 30, hjust = 1)
    )
}

## Example: make a single plot for one variable -------------------------------
# Replace "bulk_density_g_cm3" with any name in `cont_vars`
p_bd <- plot_violin_region_transect(plot_dat, "bulk_density_g_cm3")
print(p_bd)

## Batch-generate supplementary figures for many variables --------------------
# Choose a subset of cont_vars you actually care about, or just use cont_vars directly.
vars_to_plot <- cont_vars

# Directory to save figs
out_dir <- "~/Library/CloudStorage/OneDrive-PNNL/Documents/projects/compass/ec1_soils/ec1_modex_paper/results/figures/exploratory/violins"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

for (v in vars_to_plot) {
  p <- plot_violin_region_transect(plot_dat, v)
  if (!is.null(p)) {
    ggsave(
      filename = file.path(out_dir, paste0("violin_", v, ".png")),
      plot     = p,
      width    = 7,
      height   = 5,
      dpi      = 300
    )
  }
}
11
