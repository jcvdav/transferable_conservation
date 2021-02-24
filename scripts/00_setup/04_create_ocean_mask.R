
library(fasterize)
library(rnaturalearth)
library(sf)
library(tidyverse)


world_seas <- st_read(dsn = file.path(data_path, "world-seas-v3", "World_Seas_IHO_v3"), "World_Seas_IHO_v3") %>% 
  st_transform(proj_moll)

base_raster <- raster(file.path(project_path, "processed_data", "base_raster.tif"))

ocean_mask <- fasterize(world_seas, raster = base_raster)
plot(ocean_mask)

writeRaster(ocean_mask,
            filename = file.path(project_path, "processed_data", "ocean_mask.tif"),
            overwrite = TRUE)
