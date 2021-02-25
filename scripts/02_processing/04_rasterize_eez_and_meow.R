# Load packages
library(raster)
library(fasterize)
library(sf)
library(tidyverse)

# Load reference raster
base_raster <-
  raster(
    file.path(
      project_path,
      "processed_data",
      "base_raster.tif"
    )
  )

# Load the vector data
eez_meow <- st_read(file.path(project_path, "processed_data", "intersected_eez_and_meow.gpkg")) %>% 
  select(iso3n, contains("code")) 

# Rasterize
eez_raster <- fasterize(eez_meow, base_raster, field = "iso3n")
realm_raster <- fasterize(eez_meow, base_raster, field = "rlm_code")
province_raster <- fasterize(eez_meow, base_raster, field = "pro_code")
ecoregion_raster <- fasterize(eez_meow, base_raster, field = "eco_code")


# Export rasters
writeRaster(x = eez_raster,
            filename = file.path(project_path, "processed_data", "eez_raster.tif"),
            overwrite = TRUE)

writeRaster(x = realm_raster,
            filename = file.path(project_path, "processed_data", "rlm_raster.tif"),
            overwrite = TRUE)

writeRaster(x = province_raster,
            filename = file.path(project_path, "processed_data", "pro_raster.tif"),
            overwrite = TRUE)

writeRaster(x = ecoregion_raster,
            filename = file.path(project_path, "processed_data", "eco_raster.tif"),
            overwrite = TRUE)

# END OF SCRIPT #