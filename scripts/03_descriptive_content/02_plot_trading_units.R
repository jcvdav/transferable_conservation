######################################################
#title#
######################################################
# 
# Purpose
#
######################################################

## SET UP ######################################################################
# Load packages
library(startR)
library(cowplot)
library(rmapshaper)
library(rnaturalearth)
library(sf)
library(tidyverse)


# Load data

# eez_h_sum_cb <- readRDS(file = file.path(project_path, "processed_data", "eez_h_sum_costs_and_benefits.rds"))
# eez_cb <- readRDS(file = file.path(project_path, "processed_data", "eez_costs_and_benefits.rds"))
# rlm_eez_cb <- readRDS(file = file.path(project_path, "processed_data", "rlm_eez_costs_and_benefits.rds"))
# pro_eez_cb <- readRDS(file = file.path(project_path, "processed_data", "pro_eez_costs_and_benefits.rds"))

eez_meow <- st_read(file.path(project_path, "processed_data", "intersected_eez_and_meow.gpkg")) %>% 
  ms_simplify(keep_shapes = T)

# Load a coastline
coast <- ne_countries(returnclass = "sf")


## FIGURES #####################################################################

# Global bubble
ggplot() +
  geom_sf(data = eez_meow, fill = "steelblue", color = "transparent") +
  geom_sf(data = coast, color = "black", size = 0.1) +
  ggtheme_map() +
  theme(legend.position = "bottom")


# Hemisphere bubble

hemisphere_map <- ggplot() +
  geom_sf(data = coast, color = "black", size = 0.1) +
  geom_sf(data = eez_meow, aes(fill = hemisphere), color = "black", size = 0.1) +
  ggtheme_map() +
  scale_fill_viridis_d() +
  labs(fill = "Hemisphere") +
  theme(legend.position = "bottom")




lazy_ggsave(plot = hemisphere_map,
            filename = "hemisphere_map",
            width = 20, 
            height = 10)


# Map marine ecoregions
realm_map <- ggplot() +
  geom_sf(data = coast, color = "black", size = 0.1) +
  geom_sf(data = eez_meow, aes(fill = realm), color = "black", size = 0.1) +
  ggtheme_map() +
  scale_fill_viridis_d() +
  labs(fill = "Realm") +
  theme(legend.position = "bottom")


lazy_ggsave(plot = realm_map,
            filename = "realm_map",
            width = 20, 
            height = 10)

# Provinces map

provinces_map <- ggplot() +
  geom_sf(data = coast, color = "black", size = 0.1) +
  geom_sf(data = eez_meow, aes(fill = province), color = "black", size = 0.1) +
  ggtheme_map() +
  scale_fill_viridis_d() +
  # labs(fill = "Realm") +
  theme(legend.position = "None")

lazy_ggsave(plot = provinces_map,
            filename = "provinces_map",
            width = 20, 
            height = 10)

# How many countries per MEOW?
eezs_per_realm <- eez_meow %>% 
  st_drop_geometry() %>%
  group_by(realm) %>% 
  summarize(n_eez = n_distinct(iso3)) %>% 
  ungroup() %>% 
  mutate(realm = fct_reorder(realm, n_eez))

eezs_per_province <- eez_meow %>% 
  st_drop_geometry() %>% 
  group_by(realm, province) %>% 
  summarize(n_eez = n_distinct(iso3)) %>% 
  ungroup() %>% 
  mutate(province = fct_reorder(province, n_eez))


# Barplots

eezs_per_realm_plot <-
  ggplot(eezs_per_realm, aes(x = realm, y = n_eez)) +
  geom_col() +
  coord_flip() +
  labs(x = "Realm", y = "Number of countries") +
  guides(fill = F) +
  ggtheme_plot() +
  scale_y_continuous(expand = c(0, 0)) +
  geom_hline(yintercept = 2, linetype = "dashed", color = "black")

eezs_per_province_plot <-
  ggplot(eezs_per_province, aes(x = province, y = n_eez)) +
  geom_col(color = "black") +
  coord_flip() +
  labs(x = "Province", y = "Number of countries") +
  guides(fill = F) +
  ggtheme_plot() +
  scale_y_continuous(expand = c(0, 0)) +
  geom_hline(yintercept = 2, linetype = "dashed", color = "black")

# Export plots
lazy_ggsave(plot = eezs_per_realm_plot,
            filename = "eezs_per_realm",
            width = 15, height = 8)

# Export plots
lazy_ggsave(plot = eezs_per_province_plot,
            filename = "eezs_per_province",
            width = 15, height = 20)

# END OF SCRIPT #






















