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
master_cb <- readRDS(
  file = file.path(project_path, "processed_data", "master_costs_and_benefits.rds")
)

suitability_raster <-
  raster(file.path(project_path,
                   "processed_data",
                   "suitability.tif"))
costs_raster <-
  raster(
    x = file.path(project_path, "processed_data", "revenue_raster.tif")
  )

mpa_raster_raw <-
  raster(file.path(project_path, "processed_data", "mpa_raster.tif"))
mpa_raster <- mpa_raster_raw
mpa_raster[mpa_raster == 1] <- 0
mpa_raster[is.na(mpa_raster)] <- 1

iso3n <- raster(file.path(project_path, "processed_data", "eez_raster.tif"))

area_raster <- raster::area(suitability_raster)
benefits_raster <- suitability_raster * area_raster * (iso3n > 0) * (mpa_raster)
costs_raster <- costs_raster * (iso3n > 0) * (mpa_raster)
costs_raster[costs_raster == 0] <- 1

mc <- costs_raster / benefits_raster



coastline <- ne_countries(returnclass = "sf")

## PLOT IT

# Benefits

benefit_map <- 
  benefits_raster %>% 
  as.data.frame(xy = T) %>% 
  drop_na(layer) %>% 
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = layer)) +
  geom_sf(data = coastline, color = "black") +
  ggtheme_map() +
  labs(fill = expression(Q[i])) +
  scale_fill_viridis_c() +
  guides(fill = guide_colorbar(frame.colour = "black",
                               ticks.colour = "black")) +
  theme(legend.position = "bottom")

# Costs

cost_map <- 
  costs_raster %>% 
  as.data.frame(xy = T) %>% 
  drop_na(layer) %>% 
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = layer)) +
  geom_sf(data = coastline, color = "black") +
  ggtheme_map() +
  labs(fill = "log10(Fisheries revenue)") +
  scale_fill_viridis_c(trans = "log10") +
  guides(fill = guide_colorbar(frame.colour = "black",
                               ticks.colour = "black")) +
  theme(legend.position = "bottom")

# BCR
bcr_map <- 
  mc %>% 
  as.data.frame(xy = T) %>% 
  drop_na(layer) %>% 
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = layer)) +
  geom_tile(data = mpas, aes(x = lon, y = lat, fill = 0)) +
  geom_sf(data = coastline, fill = "gray", color = "black") +
  ggtheme_map() +
  scale_fill_viridis_c(trans = "log10") +
  guides(fill = guide_colorbar(title = "log10(Fisheries revenue/Qi)",
                               frame.colour = "black",
                               ticks.colour = "black")) +
  theme(legend.position = "bottom")

pannel <- plot_grid(benefit_map, cost_map, bcr_map,
                    ncol = 3,
                    labels = "AUTO")

# Export figures
lazy_ggsave(benefit_map,
            "benefit_and_cost_maps/benefit_map",
            width = 15,
            height = 7)

lazy_ggsave(cost_map,
            "benefit_and_cost_maps/cost_map",
            width = 15,
            height = 7)

lazy_ggsave(bcr_map,
            "benefit_and_cost_maps/bcr_map",
            width = 15,
            height = 7)

lazy_ggsave(pannel,
            "benefit_and_cost_maps/input_data_maps_panel",
            width = 15,
            height = 5)
