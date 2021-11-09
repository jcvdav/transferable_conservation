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
      project_path,
      "processed_data",
      "suitability.tif"
    )
  )

# Costs raster
costs_raster <-
  raster(
    file.path(
      project_path,
      "processed_data",
      "revenue_raster.tif"
    )
  ) %>% 
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

# Load the EEZ vector data
eez_meow <-
  st_read(file.path(
    project_path,
    "processed_data",
    "intersected_eez_meow_hem.gpkg"
  )) %>% 
  st_drop_geometry() %>% 
  as_tibble()

# Load the world seas vectotr data
world_seas <- st_read(file.path(
  project_path,
  "processed_data",
  "clean_world_seas.gpkg"
)) %>% 
  st_drop_geometry() %>% 
  rename(sea_name = name) %>% 
  as_tibble()


## PROCESSING ########################################################################
# Extract codes for each pixel
cb <-
  stack(iso3n,                   # Start by creating a raster stack of all features
        rlm_code,
        pro_code,
        benefits_raster,
        costs_raster,
        area_raster) %>%
  as.data.frame(xy = T) %>%       # Convert to data.frame, but keep coordinates
  rename(                         # Select and rename columns
    lon = x,
    lat = y,
    iso3n = eez_raster,
    rlm_code = rlm_raster,
    pro_code = pro_raster,
    benefit = suitability,
    cost = revenue_raster,
    area = layer
  ) %>%
  drop_na(iso3n, cost, benefit) %>%                             # Drop areas beyond national jurisdiction
  mutate(hemisphere = case_when(lon > 0 & lat > 0 ~ "NE",
                                lon < 0 & lat > 0 ~ "NW",
                                lon > 0 & lat <= 0 ~ "SE",
                                lon < 0 & lat <= 0 ~ "SW"),
         benefit = area * benefit)

# Create a master dataset with all the metadata for each pixel
master_data <- eez_meow %>%
  select(iso3, province, realm, iso3n, contains("code")) %>% 
  left_join(cb, by = c("iso3n", "rlm_code", "pro_code")) %>%            # Join to the data.frame from rasters
  select(lon, lat, iso3, province, realm, hemisphere, benefit, cost) %>% # Select columns
  filter(benefit > 0) %>% 
  mutate(bcr = benefit / cost,                                                       # Calculate marginal benefit
         mc = cost / benefit,
         neg = bcr > 0) %>%                                                         # Create dummy variable for negative costs
  drop_na(lat, lon, benefit, cost)

  
## AT THE EEZ LEVEL ####
# Calculate country-level supply curve
eez_data <- master_data %>%
  group_by(iso3) %>%
  arrange(desc(bcr)) %>%
  mutate(
    tb = cumsum(benefit),
    tc = cumsum(cost),
    pct = tb / sum(benefit),
    pct_area = 1:n() / n()
  ) %>%
  ungroup()

# Calculate global supply curve by summing horizontally
eez_h_sum <- master_data %>%
  arrange(desc(bcr)) %>%
  mutate(
    tb = cumsum(benefit),
    tc = cumsum(cost),
    pct = tb / sum(benefit),
    pct_area = 1:n() / n()
  )

## AT THE HEMISPHERE LEVEL
# Calculate hemisphere-eez level supply curve
hem_data <- master_data %>%
  group_by(iso3, hemisphere) %>%
  arrange(desc(bcr)) %>%
  mutate(
    tb = cumsum(benefit),
    tc = cumsum(cost),
    pct = tb / sum(benefit),
    pct_area = 1:n() / n()
  ) %>%
  ungroup()

# Calculate hemisphere aggregate curve by summing horizontally
# Calculate global supply curve by summing horizontally
hem_h_sum <- master_data %>% 
  group_by(hemisphere) %>% 
  arrange(desc(bcr)) %>%
  mutate(
    tb = cumsum(benefit),
    tc = cumsum(cost),
    pct = tb / sum(benefit),
    pct_area = 1:n() / n()
  ) %>%
  ungroup()

## AT THE REALM LEVEL ####
# Calculate realm and country level supply curve
rlm_eez <- master_data %>%
  group_by(iso3, realm) %>%
  arrange(desc(bcr)) %>%
  mutate(
    tb = cumsum(benefit),
    tc = cumsum(cost),
    pct = tb / sum(benefit),
    pct_area = 1:n() / n()
  ) %>%
  ungroup()

# Sum horizontally for each realm
rlm_h_sum <- master_data %>% 
  group_by(realm) %>% 
  arrange(desc(bcr)) %>%
  mutate(
    tb = cumsum(benefit),
    tc = cumsum(cost),
    pct = tb / sum(benefit),
    pct_area = 1:n() / n()
  ) %>%
  ungroup()

## AT THE PROVINCE LEVEL ####
# Calculate countyr level and province supply curve
pro_eez <- master_data %>%
  group_by(iso3, province) %>%
  arrange(desc(bcr)) %>%
  mutate(
    tb = cumsum(benefit),
    tc = cumsum(cost),
    pct = tb / sum(benefit),
    pct_area = 1:n() / n()
  ) %>%
  ungroup()

# Sum horizontally for each province
pro_h_sum <- master_data %>%
  group_by(province) %>%
  arrange(desc(bcr)) %>%
  mutate(
    tb = cumsum(benefit),
    tc = cumsum(cost),
    pct = tb / sum(benefit),
    pct_area = 1:n() / n()
  ) %>%
  ungroup()

## AT THE SEALEVEL ####
# Calculate country level and sea supply curve
sea_eez <- master_data %>%
  group_by(iso3, sea_name) %>%
  arrange(desc(bcr)) %>%
  mutate(
    tb = cumsum(benefit),
    tc = cumsum(cost),
    pct = tb / sum(benefit),
    pct_area = 1:n() / n()
  ) %>%
  ungroup()

# Sum horizzontally for each realm
sea_h_sum <- master_data %>%
  group_by(sea_name) %>%
  arrange(desc(bcr)) %>%
  mutate(
    tb = cumsum(benefit),
    tc = cumsum(cost),
    pct = tb / sum(benefit),
    pct_area = 1:n() / n()
  ) %>%
  ungroup()

## DATA EXPORT ############################################################################
# Export master data
saveRDS(
  master_data,
  file = file.path(project_path, "processed_data", "master_costs_and_benefits.rds")
)

# Export country-level data
saveRDS(eez_data,
        file = file.path( project_path, "processed_data", "eez_costs_and_benefits.rds")
)

# Export horizontally summed
saveRDS(
  eez_h_sum,
  file = file.path(project_path, "processed_data", "eez_h_sum_costs_and_benefits.rds")
)

# Export hemisphere-level data
saveRDS(hem_data,
        file = file.path( project_path, "processed_data", "hem_eez_costs_and_benefits.rds")
)

# Export horizontally summed hemisphere data
saveRDS(
  hem_h_sum,
  file = file.path(project_path, "processed_data", "hem_h_sum_costs_and_benefits.rds")
)

# Export realm level data
saveRDS(rlm_eez,
        file = file.path( project_path, "processed_data", "rlm_eez_costs_and_benefits.rds")
)

# Export horizontally summed realms
saveRDS(rlm_h_sum,
        file = file.path( project_path, "processed_data", "rlm_h_sum_costs_and_benefits.rds")
)


# Export province level data
saveRDS(pro_eez,
        file = file.path( project_path, "processed_data", "pro_eez_costs_and_benefits.rds")
)

# Export horizontally summed province level data
saveRDS(pro_h_sum,
        file = file.path( project_path, "processed_data", "pro_h_sum_costs_and_benefits.rds")
)


## END OF SCRIPT ##

