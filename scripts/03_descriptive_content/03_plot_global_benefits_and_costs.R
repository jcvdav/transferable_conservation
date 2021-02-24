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

# Raw data
benefits <- raster(file.path(project_path, "processed_data", "normalized_suitability.tif")) %>% 
  as.data.frame(xy = T) %>% 
  drop_na(suitability)

costs <- raster(file.path(project_path, "processed_data", "revenue_raster.tif")) %>% 
  as.data.frame(xy = T) %>% 
  drop_na(revenue_raster)

## master data
master_cb <- readRDS(
  file = file.path(project_path, "processed_data", "master_costs_and_benefits.rds")
)

coastline <- ne_countries(returnclass = "sf") %>% 
  st_transform(epsg_moll)

## PLOT IT

# Benefits

benefit_map <- ggplot() +
  geom_sf(data = coastline, color = "transparent") +
  geom_raster(data = master_cb, aes(x = lon, y = lat, fill = benefit)) +
  ggtheme_map() +
  labs(fill = expression(Q_i)) +
  scale_fill_viridis_c() +
  guides(fill = guide_colorbar(frame.colour = "black",
                               ticks.colour = "black"))

# Costs

cost_map <- ggplot(trans = "log10") +
  geom_sf(data = coastline, color = "transparent") +
  geom_raster(data = master_cb, aes(x = lon, y = lat, fill = cost)) +
  ggtheme_map() +
  labs(fill = "Fisheries\nrevenue") +
  scale_fill_viridis_c(trans = "log10") +
  guides(fill = guide_colorbar(frame.colour = "black",
                               ticks.colour = "black"))

# BCR
bcr_map <- master_cb %>% 
  ggplot() +
  geom_raster(aes(x = lon, y = lat, fill = bcr)) +
  geom_sf(data = coastline, fill = "gray") +
  ggtheme_map() +
  scale_fill_viridis_c(trans = "log10") +
  guides(fill = guide_colorbar(title = "log10(BCR)",
                               frame.colour = "black",
                               ticks.colour = "black"))

pannel <- plot_grid(benefit_map, cost_map, bcr_map,
                    ncol = 1,
                    labels = "AUTO")

# Export figures
lazy_ggsave(benefit_map,
            "benefit_map",
            width = 15,
            height = 7)

lazy_ggsave(cost_map,
            "cost_map",
            width = 15,
            height = 7)

lazy_ggsave(bcr_map,
            "bcr_map",
            width = 15,
            height = 7)

lazy_ggsave(pannel,
            "input_data_maps_panel",
            width = 10,
            height = 15)
