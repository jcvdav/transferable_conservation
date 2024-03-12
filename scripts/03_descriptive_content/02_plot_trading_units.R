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

sf::sf_use_s2(FALSE)

# Load data

eez_meow <- st_read(file.path(project_path, "processed_data", "intersected_eez_and_meow.gpkg")) %>% 
  ms_simplify(keep_shapes = T)

# Load a coastline
coast <- ne_countries(returnclass = "sf")

# Load hemisphere shapefile and intersect with EEZ
hemisphere <- st_read(file.path(project_path, "processed_data", "hemispheres.gpkg")) %>% 
  st_make_valid() %>% 
  st_intersection(eez_meow)


## FIGURES #####################################################################

# Global bubble
global <- eez_meow %>% 
  group_by(iso3) %>% 
  summarize(a = 1) %>% 
  ungroup() %>% 
  ggplot() +
  geom_sf(fill = "steelblue", color = "black", size = 0.1) +
  geom_sf(data = coast, color = "transparent") +
  ggtheme_map() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_sf(crs = "ESRI:54009")

lazy_ggsave(plot = global, filename = "trading_units/global",
            width = 10, height = 5)


# Hemisphere bubble

hemisphere_data <- hemisphere %>% 
  group_by(hemisphere, iso3) %>% 
  summarize(a = 1) %>% 
  ungroup() %>% 
  group_by(hemisphere) %>% 
  mutate(n_eez = n_distinct(iso3)) %>% 
  ungroup() %>% 
  mutate(hemisphere = fct_reorder(hemisphere, n_eez))

hemisphere_map <- 
  ggplot() +
  geom_sf(data = hemisphere_data, aes(fill = hemisphere), color = "black", size = 0.1) +
  geom_sf(data = coast, color = "transparent") +
  ggtheme_map() +
  scale_fill_viridis_d(option = "A") +
  labs(fill = "Hemisphere") +
  theme(legend.position = "none") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_sf(crs = "ESRI:54009")

hemisphere_bars <- hemisphere_data %>% 
  st_drop_geometry() %>% 
  select(hemisphere, n_eez) %>% 
  distinct() %>% 
  ggplot(aes(x = hemisphere, y = n_eez, fill = hemisphere)) +
  geom_col() +
  coord_flip() +
  scale_fill_viridis_d(option = "A") +
  labs(x = "Hemisphere", y = "Number of nations") +
  ggtheme_plot() +
  theme(legend.position = "none") +
  geom_hline(yintercept = 2, linetype = "dashed", color = "black")

hemisphere_segments <- plot_grid(hemisphere_map, hemisphere_bars,
                                 ncol = 1,
                                 rel_heights = c(5, 2.5))


lazy_ggsave(plot = hemisphere_map,
            filename = "trading_units/hemisphere_map",
            width = 16,
            height = 8)

lazy_ggsave(plot = hemisphere_segments,
            filename = "trading_units/hemisphere",
            width = 10,
            height = 8)


# Realm bubble

realm_data <- eez_meow %>% 
  group_by(iso3, realm) %>% 
  summarize(a = 1) %>% 
  ungroup() %>% 
  group_by(realm) %>% 
  mutate(n_eez = n_distinct(iso3)) %>% 
  ungroup() %>% 
  mutate(realm = fct_reorder(realm, n_eez))

realm_map <- 
  ggplot() +
  geom_sf(data = realm_data, aes(fill = realm), color = "black", size = 0.1) +
  geom_sf(data = coast, color = "transparent", size = 0.1) +
  ggtheme_map() +
  scale_fill_viridis_d(option = "B") +
  labs(fill = "Realm") +
  theme(legend.position = "none") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_sf(crs = "ESRI:54009")

realm_bars <- realm_data %>% 
  st_drop_geometry() %>% 
  select(realm, n_eez) %>% 
  distinct() %>% 
  ggplot(aes(x = realm, y = n_eez, fill = realm)) +
  geom_col() +
  coord_flip() +
  labs(x = "Realm", y = "Number of nations") +
  ggtheme_plot() +
  theme(legend.position = "none") +
  scale_fill_viridis_d(option = "B") +
  geom_hline(yintercept = 2, linetype = "dashed", color = "black")

realm_segments <- plot_grid(realm_map, realm_bars,
                            ncol = 1,
                            rel_heights = c(5, 6.5))

realm_segments_h <- plot_grid(realm_map, realm_bars,
                              ncol = 2,
                              rel_widths = c(1, 1))

lazy_ggsave(plot = realm_map,
            filename = "trading_units/realm_map",
            width = 16,
            height = 8)

lazy_ggsave(plot = realm_segments,
            filename = "trading_units/realm",
            width = 10,
            height = 12)


lazy_ggsave(plot = realm_segments_h,
            filename = "trading_units/realm_h",
            width = 16,
            height = 9)

# Provinces map

provinces_data <- eez_meow %>% 
  group_by(iso3, province) %>% 
  summarize(a = 1) %>% 
  ungroup() %>% 
  group_by(province) %>% 
  mutate(n_eez = n_distinct(iso3)) %>% 
  ungroup() %>% 
  mutate(province = fct_reorder(province, n_eez))

province_map <- ggplot() +
  geom_sf(data = provinces_data, aes(fill = province), color = "black", size = 0.1) +
  geom_sf(data = coast, color = "transparent", size = 0.1) +
  ggtheme_map() +
  scale_fill_viridis_d(option = "C") +
  theme(legend.position = "none") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))  +
  coord_sf(crs = "ESRI:54009")

province_bars <- provinces_data %>% 
  st_drop_geometry() %>% 
  select(province, n_eez) %>% 
  distinct() %>% 
  ggplot(aes(x = province, y = n_eez, fill = province)) +
  geom_col() +
  coord_flip() +
  labs(x = "Province", y = "Number of nations") +
  ggtheme_plot() +
  theme(legend.position = "none") +
  scale_fill_viridis_d(option = "C") +
  geom_hline(yintercept = 2, linetype = "dashed", color = "black")

province_segments <- plot_grid(province_map, province_bars,
                               ncol = 1,
                               rel_heights = c(5, 31.5))


lazy_ggsave(plot = province_segments,
            filename = "trading_units/province",
            width = 10,
            height = 32)

lazy_ggsave(plot = province_map,
            filename = "trading_units/province_map",
            width = 16,
            height = 8)


ecoregion_data <- eez_meow %>% 
  group_by(iso3, ecoregion) %>% 
  summarize(a = 1) %>% 
  ungroup() %>% 
  group_by(ecoregion) %>% 
  mutate(n_eez = n_distinct(iso3)) %>% 
  ungroup() %>% 
  mutate(ecoregion = fct_reorder(ecoregion, n_eez))

ecoregion_map <- ggplot() +
  geom_sf(data = ecoregion_data, aes(fill = ecoregion), color = "black", size = 0.1) +
  geom_sf(data = coast, color = "transparent", size = 0.1) +
  ggtheme_map() +
  scale_fill_viridis_d(option = "E") +
  theme(legend.position = "none") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_sf(crs = "ESRI:54009")

ecoregion_bars <- ecoregion_data %>% 
  st_drop_geometry() %>% 
  select(ecoregion, n_eez) %>% 
  distinct() %>% 
  ggplot(aes(x = ecoregion, y = n_eez, fill = ecoregion)) +
  geom_col() +
  coord_flip() +
  labs(x = "ecoregion", y = "Number of nations") +
  ggtheme_plot() +
  theme(legend.position = "none") +
  scale_fill_viridis_d(option = "D") +
  geom_hline(yintercept = 2, linetype = "dashed", color = "black")



# Panel figure

panel <- plot_grid(hemisphere_map, realm_map, province_map, ecoregion_map,
                   ncol = 1,
                   labels = "AUTO")

lazy_ggsave(plot = panel,
            filename = "trading_units/trading_units_panel",
            width = 10,
            height = 20)


# END OF SCRIPT #






















