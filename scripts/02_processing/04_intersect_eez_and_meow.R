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
pacman::p_load(
  sf,
  tidyverse
)

sf_use_s2(F)
# Load shapefiles
eez <- st_read(here("clean_data", "clean_world_eez_v11.gpkg"))      # Load clean EEZ
meow <- st_read(dsn = here("clean_data", "clean_meow.gpkg"))        # Load clean MEOW

## Process #########################################################################################################
# Intersect two shapefiles and generate some summary statistics ----------------------------------------------------
eez_meow <- eez %>% 
  st_intersection(meow) %>% 
  select(iso3, iso3n,                                                 # Keep only relevant columns
         ecoregion, eco_code,
         province, pro_code,
         realm, rlm_code) 

eez_meow_tbl <- eez_meow %>%
  st_drop_geometry() %>%
  as_tibble()

## Export -----------------------------------------------------------------------------------------------------------
intersected_eez_and_meow_fn <- here("clean_data", "intersected_eez_and_meow.gpkg") # File name
file.remove(intersected_eez_and_meow_fn)                                     # Remove any existing files
st_write(obj = eez_meow, dsn = intersected_eez_and_meow_fn)                  # Write geopackage to disk

write_csv(eez_meow_tbl, here("clean_data", "intersected_eez_and_meow_feature_info.csv"))
# END OF SCRIPT ######################################################################################################










