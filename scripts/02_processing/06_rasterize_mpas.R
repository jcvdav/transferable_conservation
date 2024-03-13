######################################################
#title#
######################################################
# 
# Purpose
#
######################################################

# Load packages
pacman::p_load(
  here,
  raster,
  fasterize,
  sf,
  tidyverse
)

# Load reference raster
base_raster <- raster(here("clean_data","base_raster.tif"))

# Load the vector data
mpas <- st_read(here("clean_data", "clean_mpa_atlas.gpkg"))

# Rasterize
mpa_raster <- fasterize(mpas, base_raster)

# Export rasters
writeRaster(x = mpa_raster,
            filename = here("clean_data", "mpa_raster.tif"),
            overwrite = TRUE)

# END OF SCRIPT #