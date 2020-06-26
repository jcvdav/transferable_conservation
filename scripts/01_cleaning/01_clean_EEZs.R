################################################################
######################### 01_clean_EEZs ########################
################################################################
#
# This is a cleaning script for the EEZ shapefile.
# The purpose is to read the V11 file in, and keep
# only relevant columns, as well as only relevan polygons.
# 
# Data will be exported as a geopackage in the data folder.
# 
##### About the data ###########################################
#
# The source data for the World EEZ V11 shapefile come from 
# Flanders Marine Institute (2019).
# Maritime Boundaries Geodatabase:
# Maritime Boundaries and Exclusive Economic Zones (200NM), version 11.
# Available online at https://doi.org/10.14284/386
# 
################################################################

## Set up ################################################################################################################################################################
# Load packages
library(janitor)
library(countrycode)
library(rmapshaper)
library(sf)
library(tidyverse)

## Process ###############################################################################################################################################################
# Read in the shapefiles -------------------------------------------------------------------------------------------------------------------------------------------------
eez <- st_read(dsn = file.path(project_path,
                               "raw_data",
                          "World_EEZ_v11_20191118"),
               layer = "eez_v11",
               stringsAsFactors = F) %>%              # Make sure strings are not read in as factors
  clean_names() %>%                                   # Use janitor to clean the names
  filter(str_detect(geoname, "Exclusive")) %>%        # Keep only polygons that make reference to EEZs (no joint regimes). This assings EEZs to Western Sahara and Ukraine
  st_make_valid() %>%                                 # Make all polygons valid
  mutate(iso3 = ifelse(is.na(iso_ter1),               # Replace missing territory ids with sovereign ids
                        iso_sov1,
                        iso_ter1)) %>% 
  select(iso3) %>%                                    # Select relevant columns
  st_wrap_dateline() %>% 
  st_make_valid() %>%                                 # Make all polygons valid
  group_by(iso3) %>%                                  # Union geometries
  summarize(a = 1) %>%
  ungroup() %>%
  select(-a) %>% 
  st_make_valid() %>%
  ms_simplify(keep_shapes = T, sys = T) %>% 
  st_make_valid() %>% 
  st_transform(crs = proj_moll) %>%                 # Reproject to moll
  st_make_valid() %>% 
  mutate(iso3n = countrycode(iso3, "iso3c", "iso3n")) %>% 
  select(iso3, iso3n)

## Export ----------------------------------------------------------------------------------------------------------------------------------------------------------------
eez_fn <- file.path(project_path,
                    "data",
                    "clean_world_eez_v11.gpkg")       # Create a filename
file.remove(eez_fn)                                   # Remove any preexisting data to avoid binding
st_write(obj = eez, dsn = eez_fn)                     # Save file to disk

# END OF SCRIPT ##########################################################################################################################################################