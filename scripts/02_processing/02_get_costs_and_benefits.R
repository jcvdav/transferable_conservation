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
library(startR)
library(raster)
library(sf)
library(tidyverse)

# Load data
# Benefits raster
benefits_raster <-
  raster(
    file.path(
      ng_data_path,
      "03_output/05_spp_wts_smts_provs_MPAs/ranking_raster.tif"
    )
  )

# Costs raster
costs_raster <-
  readRDS(file = file.path(project_path, "raw_data", "Fig1_PixelLeveDH_raw.rds")) %>% 
  rasterFromXYZ(crs = proj_moll) %>% 
  projectRaster(benefits_raster)

# Load spatial metadata rasters
iso3n <-
  raster(file.path(project_path, "processed_data", "eez_raster.tif"))
rlm_code <-
  raster(file.path(project_path, "processed_data", "rlm_raster.tif"))
pro_code <-
  raster(file.path(project_path, "processed_data", "pro_raster.tif"))
eco_code <-
  raster(file.path(project_path, "processed_data", "eco_raster.tif"))

# Load the EEZ vector data
eez_meow <-
  st_read(file.path(
    project_path,
    "processed_data",
    "intersected_eez_and_meow.gpkg"
  ))

## PROCESSING ########################################################################
# Extract codes for each pixel
cb <-
  stack(iso3n,
        # Start by creating a raster stack of all features
        rlm_code,
        pro_code,
        eco_code,
        benefits_raster,
        costs_raster) %>%
  as.data.frame(xy = T) %>%       # Convert to data.frame, but keep coordinates
  rename(                         # Select and rename columns
    lon = x,
    lat = y,
    iso3n = eez_raster,
    rlm_code = rlm_raster,
    pro_code = pro_raster,
    eco_code = eco_raster,
    benefit = ranking_raster,
    cost = deltaH
  ) %>%
  drop_na(iso3n)                  # Drop areas beyond national jurisdiction

# Create a master dataset with all the metadata for eahc pixel
master_data <- eez_meow %>%
  st_drop_geometry() %>%
  select(iso3, ecoregion, province, realm, iso3n, contains("code")) %>%
  left_join(cb, by = c("iso3n", "rlm_code", "pro_code", "eco_code")) %>%            # Join to the data.frame from rasters
  select(lon, lat, iso3, ecoregion, province, realm, benefit, cost) %>%             # Select columns
  mutate(mb = benefit / cost,                                                       # Calculate marginal benefit
         mc = cost / benefit,
         neg = mb >= 0) %>%                                                         # Create dummy variable for negative costs
  drop_na(lat, lon, benefit, cost)                                                 # !!!!!!!!  There are some slivers to be addressed   !!!!!!!!!

# Calculate globla supply curve
global_data <- master_data %>%
  arrange(neg, desc(mb)) %>%
  mutate(
    tb = cumsum(benefit),
    tc = cumsum(cost),
    pct = (1:nrow(.)) / nrow(.)
  )

# Calculate country-level supply curve
eez_data <- master_data %>%
  group_by(iso3) %>%
  arrange(neg, desc(mb)) %>%
  mutate(
    tb = cumsum(benefit),
    tc = cumsum(cost),
    pct = (1:length(tc)) / length(tc)
  ) %>%
  ungroup()

# Calculate realm and country level supply curve
rlm_eez <- master_data %>%
  group_by(iso3, realm) %>%
  arrange(neg, desc(mb)) %>%
  mutate(
    tb = cumsum(benefit),
    tc = cumsum(cost),
    pct = (1:length(tc)) / length(tc)
  ) %>%
  ungroup()

rlm <- master_data %>% 
  group_by(realm) %>% 
  arrange(neg, desc(mb)) %>%
  mutate(
    tb = cumsum(benefit),
    tc = cumsum(cost),
    pct = (1:length(tc)) / length(tc)
  ) %>%
  ungroup()

# Calculate countyr level and province supply curve
pro_eez <- master_data %>%
  group_by(iso3, province) %>%
  arrange(neg, desc(mb)) %>%
  mutate(
    tb = cumsum(benefit),
    tc = cumsum(cost),
    pct = (1:length(tc)) / length(tc)
  ) %>%
  ungroup()

# Calculate country level and ecoregion supplu curve
eco_eez <- master_data %>%
  group_by(iso3, ecoregion) %>%
  arrange(neg, desc(mb)) %>%
  mutate(
    tb = cumsum(benefit),
    tc = cumsum(cost),
    pct = (1:length(tc)) / length(tc)
  ) %>%
  ungroup()

## DATA EXPORT ############################################################################
# Export master data
saveRDS(
  master_data,
  file = file.path(project_path, "processed_data", "master_costs_and_benefits.rds")
)

# Export global table
saveRDS(
  global_data,
  file = file.path( project_path, "processed_data", "global_costs_and_benefits.rds")
)

# Export country-level data
saveRDS(eez_data,
        file = file.path( project_path, "processed_data", "eez_costs_and_benefits.rds")
)

# Export realm level data
saveRDS(rlm_eez,
        file = file.path( project_path, "processed_data", "rlm_eez_costs_and_benefits.rds")
)

# Export province level data
saveRDS(pro_eez,
        file = file.path( project_path, "processed_data", "pro_eez_costs_and_benefits.rds")
)

# Export ecoregion level data
saveRDS(eco_eez,
        file = file.path( project_path, "processed_data", "eco_eez_costs_and_benefits.rds")
)

## END OF SCRIPT ##
