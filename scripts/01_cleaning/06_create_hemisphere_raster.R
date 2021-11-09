################################################################
######################## 02_clean_MEOWs ########################
################################################################
#
# This is a cleaning script for the MEOW shapefile.
# The purpose is to read the file in, and keep only relevant
# columns, as well as only relevan polygons.
# 
# Data will be exported as a geopackage in the data folder.
# 
# This dataset represents the boundaries of the major oceans and
# seas of the world. The source for the boundaries is the publication
# 'Limits of Oceans & Seas, Special Publication No. 23' published
# by the IHO in 1953. The dataset was composed by the Flanders
# Marine Data and Information Centre.
# 
# 
# Data available at:
# https://www.marineregions.org/sources.php#iho
# 
# For more info:
# Flanders Marine Institute (2018). IHO Sea Areas, version 3. 
# Available online at https://www.marineregions.org/. https://doi.org/10.14284/323.
################################################################

## Set up ##########################################################################
# Load packages
library(raster)
library(sf)
library(tidyverse)

## Process #########################################################################
# Load ocean raster ----------------------------------------------------------------
base_raster <- raster(file.path(project_path, "processed_data", "base_raster.tif"))

hemispheres <- as.data.frame(base_raster, xy = T) %>% 
  mutate(r = case_when(
    x > 0 & y > 0 ~ 1,
    x < 0 & y > 0 ~ 2,
    x < 0 & y < 0 ~ 3,
    x > 0 & y < 0 ~ 4
  )) %>% 
  select(x, y, r) %>% 
  rasterFromXYZ(crs = proj_longlat)

hemispheres_raster_fn <- file.path(project_path, "processed_data", "hemispheres.tif")      # Define filename

file.remove(hemispheres_raster_fn)                            # Remove them if file exists

writeRaster(x = hemispheres_raster, filename = hemispheres_raster_fn)

# END OF SCRIPT ####################################################################