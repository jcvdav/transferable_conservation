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
pacman::p_load(
  here,
  janitor,
  sf,
  tidyverse
)

sf_use_s2(F)

## Process #########################################################################
# Load shapefile -------------------------------------------------------------------
world_seas <- st_read(dsn = here("raw_data",
                                "World_Seas_IHO_v3"),
                layer = "World_Seas_IHO_v3") %>% 
  clean_names() %>%                            # Clean column names
  select(name, mrgid) %>%                      # Select relevant columns
  st_collection_extract()                      # Make sure we don't have geometry collections

## Export ###########################################################################
world_seas_fn <- here("clean_data", "clean_world_seas.gpkg")      # Define filename
file.remove(world_seas_fn)                            # Remove them if file exists
st_write(obj = world_seas, dsn = world_seas_fn)             # Save file to disk

# END OF SCRIPT ####################################################################