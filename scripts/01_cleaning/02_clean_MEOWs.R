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
# Marine Ecoregions of the World (MEOW) is a biogeographic
# classification of the world's coasts and shelves. It is the
# first ever comprehensive marine classification system with
# clearly defined boundaries and definitions and was developed
# to closely link to existing regional systems. The ecoregions nest
# within the broader biogeographic tiers of Realms and Provinces.
# 
# MEOW represents broad-scale patterns of species and communities
# in the ocean, and was designed as a tool for planning conservation
# across a range of scales and assessing conservation efforts and
# gaps worldwide. The current system focuses on coast and shelf
# areas and does not consider realms in pelagic or deep benthic
# environment. It is hoped that parallel but distinct systems for
# pelagic and deep benthic biotas will be devised in the near future.
# 
# Data available at:
# https://www.worldwildlife.org/publications/marine-ecoregions-of-the-world-a-bioregionalization-of-coastal-and-shelf-areas
# 
# For more info:
# Spalding, Mark D., et al.
# "Marine ecoregions of the world: a bioregionalization of coastal and shelf areas."
# BioScience 57.7 (2007): 573-583.
################################################################

## Set up ##########################################################################
# Load packages
library(janitor)
library(sf)
library(tidyverse)

## Process #########################################################################
# Load shapefile -------------------------------------------------------------------
meow <- st_read(dsn = file.path(project_path,
                                "raw_data", "MEOW"),
                layer = "meow_ecos") %>% 
  clean_names() %>%                             # Clean column names
  select(ecoregion, eco_code,
         province, pro_code = prov_code,
         realm, rlm_code) %>% 
  group_by(ecoregion, eco_code,
           province, pro_code,
           realm, rlm_code) %>% 
  summarize(a = 1) %>% 
  ungroup()

## Export ###########################################################################
meow_fn <- file.path(project_path, "processed_data", "clean_meow.gpkg")      # Define filename
file.remove(meow_fn)                            # Remove them if file exists
st_write(obj = meow, dsn = meow_fn)             # Save file to disk

# END OF SCRIPT ####################################################################