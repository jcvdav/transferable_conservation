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
  file = file.path(project_path,
                   "processed_data", 
                   "supply_curves",
                   "with_mpas",
                   "global_eez_supply_curves_with_mpas.rds")
)
### Horizontally summed
eez_h_sum <- readRDS(
  file = file.path(project_path,
                   "processed_data",
                   "supply_curves",
                   "with_mpas",
                   "global_supply_curve_with_mpas.rds")
)

## Hemisphere market
### By country-hemisphere
hem_cb <- readRDS(
  file = file.path(project_path,
                   "processed_data",
                   "supply_curves",
                   "with_mpas",
                   "hemisphere_eez_supply_curves_with_mpas.rds")
)

### Horizontally summed for each hemishpere
hem_h_sum <- readRDS(
  file = file.path(project_path,
                   "processed_data",
                   "supply_curves",
                   "with_mpas",
                   "hemisphere_supply_curves_with_mpas.rds")
)

## Realm eez
### For each realm and country
rlm_eez_cb <- readRDS(
  file = file.path( project_path,
                    "processed_data",
                    "supply_curves",
                    "with_mpas",
                    "realm_eez_supply_curves_with_mpas.rds")
) 

### Horizontally summed for each realm
rlm_h_sum <- readRDS(
  file = file.path(project_path,
                   "processed_data",
                   "supply_curves",
                   "with_mpas",
                   "realm_supply_curves_with_mpas.rds")
) 

## Province eez
### For each province, realm and country
pro_eez_cb <- readRDS(
  file = file.path(project_path,
                   "processed_data",
                   "supply_curves",
                   "with_mpas",
                   "province_eez_supply_curves_with_mpas.rds")
)
### Horizontaly summed for each province
pro_h_sum <- readRDS(
  file = file.path(project_path,
                   "processed_data",
                   "supply_curves",
                   "with_mpas",
                   "province_supply_curves_with_mpas.rds")
)


# PROCESSING ############################################################################################

# Global curve
global <- ggplot(data = eez_h_sum,
                 mapping = aes(x = tb / 1e3, y = mc)) +
  geom_line(size = 1) +
  ggtheme_plot() +
  labs(x = bquote("Conservation benefit (HS weighted; Thousand "~Km^2~")"),
       y = bquote("Fisheries revenue ($/Q)"))

# Country-level supply curves
eez <- ggplot(data = eez_cb,
              mapping = aes(x = tb / 1e3, y = mc, group = iso3)) +
  geom_line() +
  guides(color = "none") +
  ggtheme_plot() +
  labs(x = bquote("Conservation benefit (HS weighted; Thousand "~Km^2~")"),
       y = bquote("Fisheries revenue ($/Q)"))

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
  labs(x = bquote("Conservation benefit (HS weighted; Thousand "~Km^2~")"),
       y = bquote("Fisheries revenue ($/Q)"))

# Hem summed
hem <- ggplot(data = hem_h_sum,
              mapping = aes(x = tb, y = mc, color = hemisphere)) +
  geom_line() +
  scale_color_viridis_d() +
  ggtheme_plot() +
  labs(x = bquote("Conservation benefit (HS weighted; Thousand "~Km^2~")"),
       y = bquote("Fisheries revenue ($/Q)"))

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
  labs(x = bquote("Conservation benefit (HS weighted; Thousand "~Km^2~")"),
       y = bquote("Fisheries revenue ($/Q)"))

# realm summed
rlm <- ggplot(data = rlm_h_sum,
       mapping = aes(x = tb, y = mc, color = realm)) +
  geom_line() +
  scale_color_viridis_d() +
  ggtheme_plot() +
  labs(x = bquote("Conservation benefit (HS weighted; Thousand "~Km^2~")"),
       y = bquote("Fisheries revenue ($/Q)"))

eez_rlm_supply_curve <- plot_grid(eez_rlm, rlm, ncol = 1, labels = "AUTO")

# EXPORT PLOTS ##############################################################################################
lazy_ggsave(plot = eez_supply_curve,
            filename = "supply_curves/eez_supply_curve_with_mpas",
            width = 20, height = 7)

lazy_ggsave(plot = eez_hem_supply_curve ,
            filename = "supply_curves/eez_hem_supply_curve_with_mpas",
            width = 20,
            height = 20)

lazy_ggsave(plot = eez_rlm_supply_curve ,
            filename = "supply_curves/eez_rlm_supply_curve_with_mpas",
            width = 20,
            height = 20)

# Presentation figures

labs <- eez_cb %>%
  filter(iso3 %in% c("MEX", "PER"),
         pct == 1)

eez_all <- ggplot(
  data = eez_cb,
  mapping = aes(x = tb / 1e3, y = mc, group = iso3)) +
  geom_line(color = "transparent") +
  guides(color = "none") +
  ggtheme_plot() +
  labs(x = bquote("Conservation benefit (HS weighted; Thousand "~Km^2~")"),
       y = bquote("Fisheries revenue ($/Q)"))

eez_supply_curve_PER <- eez_all +
  geom_line(data = filter(eez_cb, iso3 == "PER"),
            mapping = aes(x = tb / 1e3, y = mc, group = iso3),
            size = 1) +
  guides(color = "none") +
  ggtheme_plot() +
  labs(x = bquote("Conservation benefit (HS weighted; Thousand "~Km^2~")"),
       y = bquote("Fisheries revenue ($/Q)")) +
  geom_text(data = labs %>% filter(iso3 == "PER"), aes(label = iso3))

eez_supply_curve_PER_MEX <- eez_all +
  geom_line(data = filter(eez_cb, iso3 %in% c("MEX", "PER")), size = 1) +
  geom_text(data = labs, aes(label = iso3))

eez_all <- eez_all +
  geom_line() +
  geom_text(data = labs, aes(label = iso3))


lazy_ggsave(plot = eez_supply_curve_PER,
            filename = "supply_curves/eez_supply_curve_no_hsum_with_mpas_PER",
            width = 10, height = 7)

lazy_ggsave(plot = eez_supply_curve_PER_MEX,
            filename = "supply_curves/eez_supply_curve_no_hsum_with_mpas_PER_MEX",
            width = 10, height = 7)

lazy_ggsave(plot = eez_all,
            filename = "supply_curves/eez_supply_curve_no_hsum_with_mpas",
            width = 10, height = 7)

# END OF SCRIPT
