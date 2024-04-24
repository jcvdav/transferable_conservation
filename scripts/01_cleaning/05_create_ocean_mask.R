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
  fasterize,
  sf,
  tidyverse
)


# Load data --------------------------------------------------------------------
world_seas <-
  st_read(here("clean_data", "clean_world_seas.gpkg"))   # Read world seas

base_raster <-
  raster(here("clean_data", "base_raster.tif"))          # Read the base raster

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
ocean_mask <- fasterize(world_seas, raster = base_raster)                       # Generate an ocean mask

## EXPORT ######################################################################

# Export
writeRaster(
  ocean_mask,
  filename = here("clean_data", "ocean_mask.tif"),
  overwrite = TRUE
)
