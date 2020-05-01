################################################################
##################     05_chi_extraction      ##################
################################################################
# 
# This script takes a raster with information on cumulative
# human impacts (CHI) on the ocean (Halpern et al) and extracts
# the values of the rastetr for each polygon of interest. We 
# will generate mean CHI values at three levels:
# 
# L1: eez-realm-province-ecoregion (smallest)
# L2: eez-real-province (medium)
# L3: eez-realm (broadest)
# 
# The raster is in mollewiede at ~1 km resolution. We'll have to 
# aggregate it by a factor of 10 to get it in ~ 10 km.
# 
################################################################

## Set up ############################################################################################
# Load packages
library(startR)
library(here)
library(raster)
library(sf)
library(tidyverse)

# Redefine to make sure we use the correct function
extract <- raster::extract

# Load data
chi_raster <- 
  raster(here("data", "cumulative_impact_2013.tif")) %>%           # Cumulative Human Impacts raster
  raster::aggregate(fact = 10)                                     # Aggregate by a factor of 10

names(chi_raster) <- "chi_2013"

polygons <- 
  st_read(here("data", "intersected_eez_and_meow.gpkg"))           # Shapefiles intersected before

# Parallel processing parameters
n_cores <- parallel::detectCores() - 1                             # Leave one free core for processes

## Processing ########################################################################################
# Define target polygons -----------------------------------------------------------------------------
# For ecoregions, use all
eez_ecoregion <- polygons %>% 
  select(iso3, realm, province, ecoregion) %>% 
  as("Spatial")

# Union of eez and provinces
eez_provinces <- polygons %>% 
  group_by(iso3, province) %>% 
  summarize(geom = st_union(geom)) %>% 
  ungroup() %>% 
  as("Spatial")

# Union of eezs and realms
eez_realm <- polygons %>% 
  group_by(iso3, realm) %>% 
  summarize(geom = st_union(geom)) %>% 
  ungroup()

# Extract values from rasters -----------------------------------------------------------------------
# beginCluster(n = n_cores)                    # Fire up a cluster for parallel extraction

# Ecoregion
eez_ecoregion_chi <- 
  extract(x = chi_raster,
          y = eez_ecoregion,
          fun = mean,                        # Calculate the mean CHI
          sp = TRUE,                         # Return data a spatial object
          small = TRUE,                      # Safguard for small polygons
          na.rm = TRUE) %>%                  # Ignore NAs
  st_as_sf() %>% 
  st_drop_geometry() %>% 
  rename(chi_eco = chi_2013)

# Province
eez_province_chi <- 
  extract(x = chi_raster,
          y = eez_provinces,
          fun = mean,                        # Calculate the mean CHI
          sp = TRUE,                         # Retrn a spatial object
          small = TRUE,                      # Safguard for small polygons
          na.rm = TRUE)  %>%                 # Ignore NAs
  st_as_sf() %>% 
  st_drop_geometry() %>% 
  rename(chi_pro = chi_2013)

# Realms
eez_realm_chi <- 
  extract(x = chi_raster,
          y = eez_realm,
          fun = mean,                        # Calculate the mean CHI
          sp = TRUE,                         # Return a spatial object
          small = TRUE,                      # Safguard for small polygons
          na.rm = TRUE) %>%                  # Ignore NAs
  st_as_sf() %>% 
  st_drop_geometry() %>% 
  rename(chi_rea = chi_2013)

# endCluster()                                 # Close the cluster

# Put data together --------------------------------------------------------------------------------
chi_by_region <- eez_ecoregion_chi %>% 
  left_join(eez_province_chi,
            by = c("iso3", "province")) %>% 
  left_join(eez_realm_chi,
            by = c("iso3", "realm"))

# Export -------------------------------------------------------------------------------------------
write.csv(x = chi_by_region,
          file = here("data", "chi_by_eez_ecoregion.csv"),
          row.names = F)

# END OF SCRIPT ####################################################################################

















