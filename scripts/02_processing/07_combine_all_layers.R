###################################################
# This script loads rasters for costs and benefits
#
# It then adds metadata (country and ecoregion) to
# each cell, and proceeds to create a master dataset
# that contains the marginal benefits.
#
# This master dataset is then sorted by descending
# order in marginal benefits (highest first), and
# calculates the horizontally summed suply curve for
# conservation.
#
# The process is then repeated at different levels
# of spatial hierarchy:
# - eez
# - eez and realm
# - eez and province
# - eez and ecoregion
#
# The data are then exported to the processed_data
# folder under the project folder
###################################################

## SETUP #############################################################################
# Load packages
library(raster)
library(sf)
library(tidyverse)

# Load data
# Benefits raster
benefits_raster <-
  raster(file.path(project_path,
                   "processed_data",
                   "suitability.tif"))

# Costs raster
costs_raster <-
  raster(file.path(project_path,
                   "processed_data",
                   "revenue_raster.tif")) %>%
  crop(benefits_raster) %>%
  extend(benefits_raster)

# Load spatial metadata rasters
iso3n <-
  raster(file.path(project_path, "processed_data", "eez_raster.tif")) %>%
  crop(benefits_raster)

rlm_code <-
  raster(file.path(project_path, "processed_data", "rlm_raster.tif")) %>%
  crop(benefits_raster)

pro_code <-
  raster(file.path(project_path, "processed_data", "pro_raster.tif")) %>%
  crop(benefits_raster)

hem_code <-
  raster(file.path(project_path, "processed_data", "hemispheres.tif")) %>%
  crop(benefits_raster)

mpa_raster <-
  raster(file.path(project_path, "processed_data", "mpa_raster.tif")) %>%
  crop(benefits_raster)

area_raster <- raster::area(benefits_raster)

# Load the EEZ vector data
eez_meow <-
  st_read(file.path(
    project_path,
    "processed_data",
    "intersected_eez_and_meow.gpkg"
  )) %>%
  st_drop_geometry() %>%
  as_tibble()

## PROCESSING ##################################################################


cb <-
  stack(                                                                        # Start by creating a raster stack of all features
    iso3n,
    rlm_code,
    pro_code,
    hem_code,
    benefits_raster,
    costs_raster,
    mpa_raster,
    area_raster
  ) %>%
  as.data.frame(xy = T) %>%                                                     # Convert to data.frame, but keep coordinates
  rename(                                                                       # Select and rename columns
    lon = x,
    lat = y,
    iso3n = eez_raster,
    hem_code = hemispheres,
    rlm_code = rlm_raster,
    pro_code = pro_raster,
    suitability = suitability,
    cost = revenue_raster,
    mpa = mpa_raster,
    area = layer
  ) %>%
  mutate(
    hemisphere = case_when(
      lon > 0 & lat > 0 ~ "NE",
      lon < 0 & lat > 0 ~ "NW",
      lon > 0 & lat <= 0 ~ "SE",
      lon < 0 & lat <= 0 ~ "SW"
    ),
    benefit = area * suitability                                                # Calculate conservation benefit
  )

# Create a master dataset with all the metadata for each pixel
master_data <- eez_meow %>%
  select(iso3, province, realm, iso3n, contains("code")) %>%
  left_join(cb, by = c("iso3n", "rlm_code", "pro_code")) %>%                    # Join to the data.frame from rasters
  drop_na(iso3n, cost, benefit) %>%                                             # Drop areas beyond national jurisdiction and areas with no cost / benefit data
  filter(benefit > 0) %>%
  mutate(bcr = benefit / cost,                                                  # Calculate marginal benefit
         mc = cost / benefit) %>%                                               
  drop_na(lat, lon, benefit, cost) %>% 
  select(hemisphere, realm, province, iso3, everything())


## DATA EXPORT ############################################################################
# Export master data
saveRDS(
  master_data,
  file = file.path(
    project_path,
    "processed_data",
    "master_costs_and_benefits.rds"
  )
)

## END OF SCRIPT ##
