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
library(rnaturalearth)
library(sf)
library(tidyverse)

# Load data
## master data
master_cb <- readRDS(
  file = file.path(project_path, "processed_data", "master_costs_and_benefits.rds")
)

coastline <- ne_countries(returnclass = "sf") %>% 
  st_transform(epsg_moll)

## PLOT IT

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

lazy_ggsave(bcr_map,
            "bcr_map",
            width = 15,
            height = 11.5)
