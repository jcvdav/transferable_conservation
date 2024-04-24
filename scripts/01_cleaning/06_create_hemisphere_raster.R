################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Builds a raster that divides the world into four hemispheres: NE, SE, SW, NW
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  raster,
  tidyverse
)
# Load data --------------------------------------------------------------------
# Base raster
base_raster <- raster(here("clean_data", "base_raster.tif"))

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
hemispheres_raster <- as.data.frame(base_raster, xy = T) %>% 
  mutate(
    r = case_when(
      x > 0 & y > 0 ~ 1,
      x < 0 & y > 0 ~ 2,
      x < 0 & y < 0 ~ 3,
      x > 0 & y < 0 ~ 4
    )) %>% 
  select(x, y, r) %>% 
  rasterFromXYZ(crs = proj_longlat)

## EXPORT ######################################################################
hemispheres_raster_fn <- here("clean_data", "hemispheres.tif")      # Define filename

file.remove(hemispheres_raster_fn)                            # Remove them if file exists
writeRaster(x = hemispheres_raster,
            filename = hemispheres_raster_fn)

# END OF SCRIPT ####################################################################