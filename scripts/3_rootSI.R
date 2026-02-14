### this code explores the relationship between root presence and TC. Hypothesis that TC and root presence are related is supported.
## good for SI

## Load packages --------------------------------------------------------------
library(tidyverse)
library(broom)    # for tidy model outputs (not strictly needed here, but handy)
library(emmeans)  # for estimated marginal means and pairwise comparisons

## Read data ------------------------------------------------------------------
dat <- read_csv(
  "/Users/malh455/Library/CloudStorage/OneDrive-PNNL/Documents/projects/compass/ec1_soils/ec1_modex_paper/data/processed/merged.csv"
)

## Prepare data: keep relevant rows and order factor levels -------------------
tc_root <- dat %>%
  filter(
    !is.na(carbon_weight_perc),   # keep only rows with TC
    !is.na(root_presence),        # and with root_presence recorded
    transect_location %in% c("upland", "transition", "wetland")  # exclude water/sediment
  ) %>%
  mutate(
    root_presence = factor(
      root_presence,
      levels = c("none", "few", "moderate", "many")  # impose an ordered gradient
    )
  )

## Quick data check: counts per root_presence category ------------------------
tc_root %>%
  count(root_presence)

## One-way ANOVA: total C as a function of root_presence ----------------------
mod_aov <- aov(carbon_weight_perc ~ root_presence, data = tc_root)

# Standard ANOVA table
summary(mod_aov)

## Estimated marginal means and pairwise contrasts ----------------------------
# Model-based means (± SE, CI) for each root_presence class
emm <- emmeans(mod_aov, specs = "root_presence")
emm

# Tukey-adjusted pairwise comparisons among root_presence levels
pairs(emm, adjust = "tukey")

# Quick diagnostic plot of emmeans with confidence intervals
plot(emm)

## Visualization: TC distributions by root presence ---------------------------
ggplot(tc_root, aes(x = root_presence, y = carbon_weight_perc)) +
  geom_violin(fill = "grey85", color = "grey30") +       # distribution per category
  stat_summary(fun = median, geom = "point",             # add median point
               size = 1.5, color = "black") +
  labs(
    x = "Root presence",
    y = "Total C (%)",
    title = "Total carbon vs root presence (all regions, all transect locations)"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1)    # tilt labels for readability
  )



tc_root %>% 
  group_by(root_presence) %>% 
  summarise(
    n = n(),
    mean_TC = mean(carbon_weight_perc),
    sd_TC   = sd(carbon_weight_perc)
  )


#####running diagnostics to check whether i need to do non parametric stats

par(mfrow = c(1, 2))
plot(mod_aov, which = 1)  # residuals vs fitted
plot(mod_aov, which = 2)  # normal Q-Q
par(mfrow = c(1, 1))

###looks ok but check Kruskal anyway

##kruskal check
kruskal.test(carbon_weight_perc ~ root_presence, data = tc_root)

## still fine, stick to anova

###############now by region and transect


#### Does the TC–root relationship differ by region? NOPE

tc_root <- dat %>%
  filter(
    !is.na(carbon_weight_perc),
    !is.na(root_presence),
    !is.na(region),
    !is.na(transect_location),
    transect_location %in% c("upland", "transition", "wetland")
  ) %>%
  mutate(
    root_presence = factor(root_presence,
                           levels = c("none", "few", "moderate", "many")),
    transect_location = factor(
      tolower(transect_location),
      levels = c("upland", "transition", "wetland")
    ),
    region = factor(region)
  )




mod_region <- lm(
  carbon_weight_perc ~ root_presence * region,
  data = tc_root
)

anova(mod_region)   # Type I ANOVA table
summary(mod_region)


### what about by transect location YEP
mod_transect <- lm(
  carbon_weight_perc ~ root_presence * transect_location,
  data = tc_root
)

anova(mod_transect)
summary(mod_transect)

#### optional 3-way model 
mod_full <- lm(
  carbon_weight_perc ~ root_presence * region * transect_location,
  data = tc_root
)

anova(mod_full)










