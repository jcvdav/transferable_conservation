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
# Load hemisphere raster ----------------------------------------------------------------
hemispheres_raster <- raster(file.path(project_path, "processed_data", "hemispheres.tif"))      # Define filename

hemispheres_sf <- raster::rasterToPolygons(x = hemispheres_raster, dissolve = T) %>% 
  st_as_sf() %>% 
  mutate(hemisphere = case_when(hemispheres == 1 ~ "NE",
                                hemispheres == 2 ~ "NW",
                                hemispheres == 3 ~ "SW",
                                hemispheres == 4 ~ "SE")) %>% 
  rename(hem_id = hemispheres) %>% 
  select(hemisphere, hem_id, geometry)

hemispheres_gpkg_fn <- file.path(project_path, "processed_data", "hemispheres.gpkg")      # Define filename

file.remove(hemispheres_gpkg_fn)                            # Remove them if file exists

st_write(obj = hemispheres_sf, dsn = hemispheres_gpkg_fn)             # Save file to disk

# END OF SCRIPT ####################################################################