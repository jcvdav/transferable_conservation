library(raster)
library(fasterize)
library(tidyverse)

# Load reference raster
benefits <- raster(file.path(ng_data_path, "/03_output/14_upgrade_weak_MPAs/ranking_raster.tif"))

eez_meow <- st_read(file.path(project_path, "data", "intersected_eez_and_meow.gpkg")) %>% 
  select(iso3n, contains("code")) 

eez_meow_raster <- fasterize(eez_meow, benefits, field = "iso3n", fun = "min")
names(eez_meow_raster) <- "iso3n"
mapview(eez_meow_raster)

# Export raster
writeRaster(x = eez_meow_raster,
            filename = file.path(project_path, "data", "eez_meow_raster.tif"),
            overwrite = TRUE)
