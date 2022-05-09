######################################################
#title#
######################################################
# 
# Purpose
#
######################################################

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
mpas <- st_read(file.path(project_path, "processed_data", "clean_mpa_atlas.gpkg"))

# Rasterize
mpa_raster <- fasterize(mpas, base_raster)

# Export rasters
writeRaster(x = mpa_raster,
            filename = file.path(project_path, "processed_data", "mpa_raster.tif"),
            overwrite = TRUE)

# END OF SCRIPT #