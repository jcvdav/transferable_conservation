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
eez <- st_read(file.path(project_path, "processed_data", "clean_world_eez_v11.gpkg")) %>% 
  st_set_crs(epsg_moll)      
meow <- st_read(dsn = file.path(project_path, "processed_data", "clean_meow.gpkg")) %>% 
  st_set_crs(epsg_moll)      
hem <- st_read(dsn = file.path(project_path, "processed_data", "hemispheres.gpkg")) %>% 
  st_set_crs(epsg_moll)

## Process #########################################################################################################
# Intersect two shapefiles and generate some summary statistics ----------------------------------------------------
eez_meow <- st_intersection(eez, meow) %>%                    # Intersect features
  st_intersection(hem) %>% 
  # mutate(area_eco = st_area(.)) %>%                           # Calculate the area of each ecorgion within a country
  # group_by(iso3, iso3n, province, pro_code) %>%                                # Calculate area of each province within a country
  # mutate(area_pro = sum(area_eco, na.rm = T)) %>% 
  # ungroup() %>% 
  # group_by(iso3, iso3n, realm, rlm_code) %>%                                   # Calculate area of each realm within a country
  # mutate(area_rea = sum(area_eco, na.rm = T)) %>% 
  # ungroup() %>% 
  # group_by(iso3, iso3n) %>%                                          # Calculate EEZ area
  # mutate(area_eez = sum(area_eco, na.rm = T)) %>% 
  # ungroup() %>% 
  # mutate(eco_div_eez = area_eco / area_eez,                   # Calculate ratio of polygon area to EEZ area
         # pro_div_eez = area_pro / area_eez,
         # rea_div_eez = area_rea / area_eez) %>% 
  select(iso3, iso3n,                                     # Keep only rlevant columns
         province, pro_code,
         realm, rlm_code, 
         hemisphere, hem_code = hem_id)

## Export -----------------------------------------------------------------------------------------------------------
intersected_eez_and_meow_fn <- file.path(project_path, "processed_data", "intersected_eez_meow_hem.gpkg") # File name
file.remove(intersected_eez_and_meow_fn)                                     # Remove any existing files
st_write(obj = eez_meow, dsn = intersected_eez_and_meow_fn)                  # Write geopackage to disk

# END OF SCRIPT ######################################################################################################










