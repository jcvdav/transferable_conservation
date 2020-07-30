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
library(sf)
library(tidyverse)

# Load data

# Raw data
benefits <- raster("~/Google Drive File Stream/Shared drives/emlab/projects/current-projects/ocean-conservation-priorities/data/03_output/05_spp_wts_smts_provs_MPAs/delta_v_raster.tif") %>% 
  as.data.frame(xy = T) %>% 
  mutate(delta_v_raster = ifelse(delta_v_raster <= 1e-6, 0, delta_v_raster)) %>%
  drop_na(delta_v_raster)



costs <- raster("~/Google Drive File Stream/Shared drives/emlab/projects/current-projects/transferable-conservation/processed_data/costs_raster.tif") %>% 
  as.data.frame(xy = T) %>% 
  drop_na(costs_raster)

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
  geom_raster(data = benefits, aes(x = x, y = y, fill = delta_v_raster)) +
  ggtheme_map() +
  labs(fill = "Biodiversity\nranking") +
  scale_fill_viridis_c(trans = "log10") +
  guides(fill = guide_colorbar(frame.colour = "black",
                               ticks.colour = "black")) +
  labs(caption = "Note: Fill values have been log10-transformded")

# Costs

cost_map <- ggplot() +
  geom_sf(data = coastline, color = "transparent") +
  geom_raster(data = costs, aes(x = x, y = y, fill = costs_raster)) +
  ggtheme_map() +
  labs(fill = "Losses in\nlandings") +
  scale_fill_viridis_c(trans = "log10") +
  guides(fill = guide_colorbar(frame.colour = "black",
                               ticks.colour = "black")) +
  labs(caption = "Note: Fill values have been log10-transformded")

# BCR
bcr_map <- master_cb %>% 
  ggplot() +
  geom_raster(aes(x = lon, y = lat, fill = bcr)) +
  geom_sf(data = coastline, fill = "gray") +
  ggtheme_map() +
  scale_fill_viridis_c(trans = "log10") +
  guides(fill = guide_colorbar(title = "BCR",
                               frame.colour = "black",
                               ticks.colour = "black")) +
  labs(caption = "Note: Fill values have been log10-transformded")


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
