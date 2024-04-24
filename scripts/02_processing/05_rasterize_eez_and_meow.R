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
  fasterize,
  sf,
  tidyverse
)

sf_use_s2(F)

# Load data --------------------------------------------------------------------
# Load reference raster
base_raster <- raster(here("clean_data", "base_raster.tif"))

# Load the vector data
eez_meow <- st_read(here("clean_data", "intersected_eez_and_meow.gpkg")) %>% 
  select(iso3n, contains("code")) 

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
# Rasterize
eez_raster <- fasterize(eez_meow, base_raster, field = "iso3n")
realm_raster <- fasterize(eez_meow, base_raster, field = "rlm_code")
province_raster <- fasterize(eez_meow, base_raster, field = "pro_code")
ecoregion_raster <- fasterize(eez_meow, base_raster, field = "eco_code")

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
# Export rasters
writeRaster(x = eez_raster,
            filename = here("clean_data", "eez_raster.tif"),
            overwrite = TRUE)

writeRaster(x = realm_raster,
            filename = here("clean_data", "rlm_raster.tif"),
            overwrite = TRUE)

writeRaster(x = province_raster,
            filename = here("clean_data", "pro_raster.tif"),
            overwrite = TRUE)

writeRaster(x = ecoregion_raster,
            filename = here("clean_data", "eco_raster.tif"),
            overwrite = TRUE)

# END OF SCRIPT #