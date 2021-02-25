
library(raster)
library(fasterize)
library(rnaturalearth)
library(sf)
library(tidyverse)


world_seas <- st_read(file.path(project_path, "processed_data", "clean_world_seas.gpkg"))

base_raster <- raster(file.path(project_path, "processed_data", "base_raster.tif"))

ocean_mask <- fasterize(world_seas, raster = base_raster)
plot(ocean_mask)

writeRaster(ocean_mask,
            filename = file.path(project_path, "processed_data", "ocean_mask.tif"),
            overwrite = TRUE)
