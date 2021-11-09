################################################################
################### 03_intersect_eez_and_meow ##################
################################################################
# 
# This script reads the clean EEZ and MEOW data and intersects 
# the spatial features in them. I then calculate the area
# of each polygon (eez-realm-province-ecoregion), so that
# I can know how much of each habitat is available each grouping
# 
################################################################

## Set up ##########################################################################################################
# Load packages
library(lwgeom)
library(sf)
library(tidyverse)


# Load shapefiles
eez <- st_read(file.path(project_path, "processed_data", "clean_world_eez_v11.gpkg"))      # Load clean EEZ
meow <- st_read(dsn = file.path(project_path, "processed_data", "clean_meow.gpkg"))        # Load clean MEOW

## Process #########################################################################################################
# Intersect two shapefiles and generate some summary statistics ----------------------------------------------------
eez_meow <- eez %>% 
  # st_crop(xmin = -180, ymin = -90, xmax = 180, ymax = 90) %>%
  st_intersection(meow) %>% 
  select(iso3, iso3n,                                                 # Keep only relevant columns
         province, pro_code,
         realm, rlm_code)

## Export -----------------------------------------------------------------------------------------------------------
intersected_eez_and_meow_fn <- file.path(project_path, "processed_data", "intersected_eez_meow_hem.gpkg") # File name
file.remove(intersected_eez_and_meow_fn)                                     # Remove any existing files
st_write(obj = eez_meow, dsn = intersected_eez_and_meow_fn)                  # Write geopackage to disk

# END OF SCRIPT ######################################################################################################










