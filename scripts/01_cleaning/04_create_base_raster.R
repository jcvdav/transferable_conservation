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
  here,
  raster,
  tidyverse
)

# Load data --------------------------------------------------------------------

## PROCESSING ##################################################################

# Create a raster object with the World's extent
base_raster <- raster(
  xmn = -180,
  xmx = 180,
  ymn = -90,
  ymx = 90,
  resolution = 0.5,                                                             # Set resolution to 0.5 of a degree (~55 km at the equator)
  vals = 1L,                                                                    # Assign everything a value of 1L
  crs = proj_longlat                                                            # Specify LongLat projection (so, not projected)
)

## EXPORT ######################################################################

# Export the base raster
writeRaster(
  base_raster,
  filename = here("clean_data", "base_raster.tif"),
  overwrite = TRUE
)
