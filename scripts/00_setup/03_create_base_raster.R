# create base raster
library(raster)
library(tidyverse)

# Create a raster object with the World's extent
base_raster <- raster(xmn = -180, xmx = 180,
                      ymn = -90, ymx = 90,
                      resolution = 0.1,                # Set resolution to 0.1 of a degree (~10 km at the equator)
                      vals = 1L,                       # Assign everything a value of 1L
                      crs = proj_longlat) %>%          # Specify LongLat projection
  projectRaster(res = 50000,                           # Specify desired end resolution (50 km grid-cells)
                crs = proj_moll,                       # Specify Mollweide projection
                method = "ngb",                        # Use NGB to ensure that they are all 1L in the end
                over = T)                              # Allow to wrap around dateline
                
# Export the base raster
writeRaster(base_raster,
            filename = file.path(project_path, "processed_data", "base_raster.tif"),
            overwrite = TRUE)
