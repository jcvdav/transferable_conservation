# Load packages
library(startR)
library(cowplot)
library(tidyverse)

# Load data

global_cb <- readRDS(file = file.path(project_path, "processed_data", "global_costs_and_benefits.rds"))
eez_cb <- readRDS(file = file.path(project_path, "processed_data", "eez_costs_and_benefits.rds"))
rlm_eez_cb <- readRDS(file = file.path(project_path, "processed_data", "rlm_eez_costs_and_benefits.rds"))
pro_eez_cb <- readRDS(file = file.path(project_path, "processed_data", "pro_eez_costs_and_benefits.rds"))
eco_eez_cb <- readRDS(file = file.path(project_path, "processed_data", "eco_eez_costs_and_benefits.rds"))

# FIGURES

# Global curve
global <- ggplot(data = global_cb,
       mapping = aes(x = tb, y = cost)) +
  geom_line() +
  ggtheme_plot() +
  labs(x = "Conservation",
       y = "Marginal Costs")

# Country-level supply curves
eez <- ggplot(data = eez_cb,
       mapping = aes(x = tb, y = cost, color = iso3)) +
  geom_line() +
  guides(color = F) +
  scale_color_viridis_d() +
  ggtheme_plot() +
  labs(x = "Conservation",
       y = "Marginal Costs")

# Country realm
eez_rlm <- ggplot(data = rlm_eez_cb, mapping = aes(x = tb, y = cost, color = iso3)) +
  geom_line() +
  guides(color = F) +
  scale_color_viridis_d() +
  facet_wrap(~realm, scales = "free") +
  ggtheme_plot() +
  labs(x = "Conservation",
       y = "Marginal Costs")

# Country province, for each Realm
eez_pro <- ggplot(data = pro_eez_cb, mapping = aes(x = tb, y = cost, color = iso3, group = paste(province, iso3))) +
  geom_line() +
  guides(color = F) +
  scale_color_viridis_d() +
  facet_wrap(~realm, scales = "free") +
  ggtheme_plot() +
  labs(x = "Conservation",
       y = "Marginal Costs")


eez_eco <- ggplot(data = eco_eez_cb, mapping = aes(x = tb, y = cost, color = iso3, group = paste(ecoregion, province, iso3))) +
  geom_line() +
  guides(color = F) +
  scale_color_viridis_d() +
  facet_wrap(~realm, scales = "free") +
  ggtheme_plot() +
  labs(x = "Conservation",
       y = "Marginal Costs")

# Export plots
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














