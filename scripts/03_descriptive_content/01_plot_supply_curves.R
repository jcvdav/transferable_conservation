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
## Global data
global_cb <- readRDS(
  file = file.path(project_path,"processed_data","global_costs_and_benefits.rds")
)

## National data
eez_cb <- readRDS(
  file = file.path(project_path, "processed_data", "eez_costs_and_benefits.rds")
)

## Realm eez
rlm_eez_cb <- readRDS(
  file = file.path( project_path, "processed_data", "rlm_eez_costs_and_benefits.rds")
) 

## Province eez
pro_eez_cb <- readRDS(
  file = file.path( project_path, "processed_data", "pro_eez_costs_and_benefits.rds")
)

# Ecoregion eez
eco_eez_cb <- readRDS(
  file = file.path( project_path, "processed_data", "eco_eez_costs_and_benefits.rds")
)

# PROCESSING ############################################################################################

# Global curve
global <- ggplot(data = global_cb,
                 mapping = aes(x = tb, y = mc)) +
  geom_line() +
  ggtheme_plot() +
  labs(x = "Conservation",
       y = "Marginal costs")

# Country-level supply curves
eez <- ggplot(data = eez_cb,
              mapping = aes(x = tb, y = mc, color = iso3)) +
  geom_line() +
  guides(color = F) +
  scale_color_viridis_d() +
  ggtheme_plot() +
  labs(x = "Conservation",
       y = "Marginal costs")

# Country realm
eez_rlm <-
  ggplot(data = rlm_eez_cb,
         mapping = aes(x = tb, y = mc, color = iso3)) +
  geom_line() +
  guides(color = F) +
  scale_color_viridis_d() +
  facet_wrap( ~ realm, scales = "free") +
  ggtheme_plot() +
  labs(x = "Conservation",
       y = "Marginal costs")

# Country province, for each Realm
eez_pro <-
  ggplot(data = pro_eez_cb,
         mapping = aes( x = tb, y = mc, color = iso3, group = paste(province, iso3))) +
  geom_line() +
  guides(color = F) +
  scale_color_viridis_d() +
  facet_wrap( ~ realm, scales = "free") +
  ggtheme_plot() +
  labs(x = "Conservation",
       y = "Marginal costs")


eez_eco <-
  ggplot(data = eco_eez_cb,
         mapping = aes(x = tb,y = mc,color = iso3,group = paste(ecoregion, province, iso3))) +
  geom_line() +
  guides(color = F) +
  scale_color_viridis_d() +
  facet_wrap( ~ realm, scales = "free") +
  ggtheme_plot() +
  labs(x = "Conservation",
       y = "Marginal costs")

# EXPORT PLOTS ##############################################################################################
lazy_ggsave(plot = global,
            filename = "global_aggrregate_supply_curve")

lazy_ggsave(plot = eez,
            filename = "eez_supply_curve")

lazy_ggsave(plot = eez_rlm,
            filename = "eez_rlm_supply_curve")

lazy_ggsave(plot = eez_pro,
            filename = "eez_pro_supply_curve")

lazy_ggsave(plot = eez_eco,
            filename = "eez_eco_supply_curve")

# END OF SCRIPT
