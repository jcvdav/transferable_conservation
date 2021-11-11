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

coastline <- ne_countries(returnclass = "sf")

## PLOT IT

# Benefits

benefit_map <- ggplot() +
  geom_tile(data = master_cb, aes(x = lon, y = lat, fill = benefit)) +
  geom_sf(data = coastline, color = "black") +
  ggtheme_map() +
  labs(fill = expression(S[i])) +
  scale_fill_viridis_c() +
  guides(fill = guide_colorbar(frame.colour = "black",
                               ticks.colour = "black")) +
  theme(legend.position = "bottom")

# Costs

cost_map <- ggplot(trans = "log10") +
  geom_tile(data = master_cb, aes(x = lon, y = lat, fill = cost)) +
  geom_sf(data = coastline, color = "black") +
  ggtheme_map() +
  labs(fill = "Fisheries\nrevenue") +
  scale_fill_viridis_c(trans = "log10") +
  guides(fill = guide_colorbar(frame.colour = "black",
                               ticks.colour = "black")) +
  theme(legend.position = "bottom")

# BCR
bcr_map <- master_cb %>% 
  ggplot() +
  geom_tile(aes(x = lon, y = lat, fill = bcr)) +
  geom_sf(data = coastline, fill = "gray", color = "black") +
  ggtheme_map() +
  scale_fill_viridis_c(trans = "log10") +
  guides(fill = guide_colorbar(title = "log10(BCR)",
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
