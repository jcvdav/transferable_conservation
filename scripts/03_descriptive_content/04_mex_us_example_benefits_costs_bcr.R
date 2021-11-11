####################################################
# Now that we have the targets and prices, we can 
# move forward and identify where biodiversity is
# protected, and the associated costs of doing so.
# 
# The benefits should be equivalent, since we are
# using the same target.
####################################################

# SET UP ################################################################################################
# Load packages
library(startR)
library(raster)
library(rnaturalearth)
library(cowplot)
library(sf)
library(tidyverse)

# Load data

## master data
master_cb <- readRDS(file = file.path(project_path, "processed_data", "master_costs_and_benefits.rds")) %>% 
  filter(iso3 %in% c("MEX", "USA")) %>% 
  filter(!realm %in% c("Arctic", "Eastern Indo-Pacific"))

eez_cb <- readRDS(
  file = file.path(project_path, "processed_data", "eez_costs_and_benefits.rds")) %>% 
  filter(iso3 %in% c("MEX", "USA")) %>% 
  filter(!realm %in% c("Arctic", "Eastern Indo-Pacific"))

coastline <- ne_countries(returnclass = "sf",scale = "large") %>% 
  filter(iso_a3 %in% c("MEX", "USA"))

## PLOT IT

# Benefits

benefit_map <- ggplot() +
  geom_tile(data = master_cb, aes(x = lon, y = lat, fill = benefit / 2500)) +
  geom_sf(data = coastline, color = "black", size = 0.5) +
  # coord_sf(crs = proj_longlat) +
  ggtheme_map() +
  labs(fill = bquote("Habitat\nsuitability")) +
  scale_fill_viridis_c() +
  guides(fill = guide_colorbar(frame.colour = "black",
                               ticks.colour = "black")) +
  # theme(legend.position = "bottom") +
  # lims(y = c(1500000, 5900000),
       # x = c(-11900000, -5500000)) +
  ggtitle(label = "Habitat suitability is heterogeneously\ndistributed between and within nations",
          subtitle = "All pixels are 50 X 50 km")



cost_map <- ggplot() +
  geom_tile(data = master_cb, aes(x = lon, y = lat, fill = cost / 1e6)) +
  geom_sf(data = coastline, color = "black", size = 0.5) +
  # coord_sf(crs = proj_longlat) +
  ggtheme_map() +
  labs(fill = bquote("Mean annual\nfisheries revenue\n(Million USD)")) +
  scale_fill_viridis_c() +
  guides(fill = guide_colorbar(frame.colour = "black",
                               ticks.colour = "black")) +
  # theme(legend.position = "bottom") +
  lims(y = c(1500000, 5900000),
       x = c(-11900000, -5500000)) +
  ggtitle(label = "Costs are heterogeneously\ndistributed between and within nations",
          subtitle = "All pixels are 50 X 50 km")

# BCR
bcr_map <- ggplot() +
  geom_tile(data = master_cb, aes(x = lon, y = lat, fill = bcr)) +
  geom_sf(data = coastline, color = "black", size = 0.5) +
  # coord_sf(crs = proj_longlat) +
  ggtheme_map() +
  labs(fill = bquote("Mean annual\nfisheries revenue\n(Million USD)")) +
  scale_fill_viridis_c(trans = "log10") +
  guides(fill = guide_colorbar(frame.colour = "black",
                               ticks.colour = "black")) +
  # theme(legend.position = "bottom") +
  lims(y = c(1500000, 5900000),
       x = c(-11900000, -5500000)) +
  ggtitle(label = "Benefit-to-cost ratio is heterogeneously\ndistributed between and within nations",
          subtitle = "All pixels are 50 X 50 km")

ggplot(eez_cb, aes(x = tb, y = mc, color = iso3)) +
  geom_line(size = 1) +
  ggtheme_plot() +
  labs(x = "HS-weighted ")

# Export figures
lazy_ggsave(benefit_map,
            "us_mex_benefit_map",
            width = 15,
            height = 7)

lazy_ggsave(cost_map,
            "us_mex_cost_map",
            width = 15,
            height = 7)

lazy_ggsave(bcr_map,
            "us_mex_bcr_map",
            width = 15,
            height = 7)
