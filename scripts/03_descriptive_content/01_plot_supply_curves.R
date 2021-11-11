#####################################################
# This script reads in the mc-benefit data generated
# in the processing folder, and creates some figures
# to explore the supply curves for each country,
# realm, province, and ecoregion, as well as globaly
####################################################

# SET UP ################################################################################################
# Load packages
library(startR)
library(cowplot)
library(tidyverse)

# Load data
## Global market
### By country
eez_cb <- readRDS(
  file = file.path(project_path, "processed_data", "eez_costs_and_benefits.rds")
)
### Horizontally summed
eez_h_sum <- readRDS(
  file = file.path(project_path,"processed_data","eez_h_sum_costs_and_benefits.rds")
)

## Hemisphere market
### By country-hemisphere
hem_cb <- readRDS(
  file = file.path(project_path, "processed_data", "hem_eez_costs_and_benefits.rds")
)

### Horizontally summed for each hemishpere
hem_h_sum <- readRDS(
  file = file.path(project_path,"processed_data","hem_h_sum_costs_and_benefits.rds")
)

## Realm eez
### For each realm and country
rlm_eez_cb <- readRDS(
  file = file.path( project_path, "processed_data", "rlm_eez_costs_and_benefits.rds")
) 
### Horizontally summed for each realm
rlm_h_sum <- readRDS(
  file = file.path( project_path, "processed_data", "rlm_h_sum_costs_and_benefits.rds")
) 

## Province eez
### For each province, realm and country
pro_eez_cb <- readRDS(
  file = file.path( project_path, "processed_data", "pro_eez_costs_and_benefits.rds")
)
### Horizontaly summed for each province
pro_h_sum <- readRDS(
  file = file.path( project_path, "processed_data", "pro_h_sum_costs_and_benefits.rds")
)


# PROCESSING ############################################################################################

# Global curve
global <- ggplot(data = eez_h_sum,
                 mapping = aes(x = tb / 1e3, y = mc)) +
  geom_line(size = 1) +
  ggtheme_plot() +
  labs(x = bquote("Surface area (HS weighted; thousand"~Km^2~")"),
       y = bquote("Fisheries revenue ($"~Km^-2~")"))

# Country-level supply curves
eez <- ggplot(data = eez_cb,
              mapping = aes(x = tb / 1e3, y = mc, group = iso3)) +
  geom_line() +
  guides(color = "none") +
  ggtheme_plot() +
  labs(x = bquote("Surface area (HS weighted; Thousand "~Km^2~")"),
       y = bquote("Fisheries revenue ($"~Km^-2~")"))

eez_supply_curve <- plot_grid(eez, global, ncol = 2, labels = "AUTO")

# Hemisphere

eez_hem <- 
  ggplot(data = hem_cb,
         mapping = aes(x = tb, y = mc, group = iso3)) +
  geom_line() +
  guides(color = "none") +
  scale_color_viridis_d() +
  facet_wrap( ~ hemisphere, scales = "free") +
  ggtheme_plot() +
  labs(x = "Biodiversity",
       y = "Marginal Costs")

# Hem summed
hem <- ggplot(data = hem_h_sum,
              mapping = aes(x = tb, y = mc, color = hemisphere)) +
  geom_line() +
  scale_color_viridis_d() +
  ggtheme_plot() +
  labs(x = "Biodiversity",
       y = "Marginal Costs")

eez_hem_supply_curve <- plot_grid(eez_hem, hem, ncol = 1, labels = "AUTO")


# Country realm
eez_rlm <-
  ggplot(data = rlm_eez_cb,
         mapping = aes(x = tb, y = mc, group = iso3)) +
  geom_line() +
  guides(color = "none") +
  scale_color_viridis_d() +
  facet_wrap( ~ realm, scales = "free") +
  ggtheme_plot() +
  labs(x = bquote("Surface area (HS weighted; Thousand"~Km^2~")"),
       y = bquote("Fisheries revenue ($"~Km^-2~")"))

# realm summed
rlm <- ggplot(data = rlm_h_sum,
       mapping = aes(x = tb, y = mc, color = realm)) +
  geom_line() +
  scale_color_viridis_d() +
  ggtheme_plot() +
  labs(x = bquote("Surface area (HS weighted;"~Km^2~")"),
       y = bquote("Fisheries revenue ($"~Km^-2~")"))

eez_rlm_supply_curve <- plot_grid(eez_rlm, rlm, ncol = 1, labels = "AUTO")

# Country province, for each Realm
eez_pro <-
  ggplot(data = pro_h_sum,
         mapping = aes(x = tb, y = mc, color = iso3, group = paste(province, iso3))) +
  geom_line() +
  guides(color = "none") +
  scale_color_viridis_d() +
  facet_wrap( ~ realm, scales = "free") +
  ggtheme_plot() +
  labs(x = bquote("Surface area (HS weighted;"~Km^2~")"),
       y = bquote("Fisheries revenue ($"~Km^-2~")"))


# EXPORT PLOTS ##############################################################################################
lazy_ggsave(plot = eez_supply_curve,
            filename = "supply_curves/eez_supply_curve",
            width = 18, height = 7)

lazy_ggsave(plot = eez_hem_supply_curve ,
            filename = "supply_curves/eez_hem_supply_curve",
            width = 20,
            height = 20)

lazy_ggsave(plot = eez_rlm_supply_curve ,
            filename = "supply_curves/eez_rlm_supply_curve",
            width = 20,
            height = 20)

lazy_ggsave(plot = eez_pro,
            filename = "supply_curves/eez_pro_supply_curve",
            width = 20,
            height = 20)

# END OF SCRIPT
