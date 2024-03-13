################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Description
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  raster,
  sf,
  tidyverse
)

sf_use_s2(F)

# Load data --------------------------------------------------------------------
hemispheres_raster <- raster(here("clean_data", "hemispheres.tif"))      # Define filename

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
hemispheres_sf <- raster::rasterToPolygons(x = hemispheres_raster, dissolve = T) %>% 
  st_as_sf() %>% 
  mutate(hemisphere = case_when(hemispheres == 1 ~ "NE",
                                hemispheres == 2 ~ "NW",
                                hemispheres == 3 ~ "SW",
                                hemispheres == 4 ~ "SE")) %>% 
  rename(hem_id = hemispheres) %>% 
  select(hemisphere, hem_id, geometry)

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
hemispheres_gpkg_fn <- here("clean_data", "hemispheres.gpkg")      # Define filename

file.remove(hemispheres_gpkg_fn)                            # Remove them if file exists
st_write(obj = hemispheres_sf, dsn = hemispheres_gpkg_fn)             # Save file to disk

# END OF SCRIPT ####################################################################