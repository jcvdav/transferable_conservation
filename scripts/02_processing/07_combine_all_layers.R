################################################################################
# title
################################################################################
# 
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# This script loads rasters for costs, benefits, and mateadata to 
# create a master dataset that contains the marginal benefits.
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  raster,
  sf,
  tidyverse
)

# Load data
# Benefits raster
benefits_raster <-
  raster(here("clean_data",
              "suitability.tif"))

# Costs raster
costs_raster <-
  raster(here("clean_data",
              "revenue_raster.tif")) %>%
  crop(benefits_raster) %>%
  extend(benefits_raster)

# Load spatial metadata rasters
iso3n <-
  raster(here("clean_data", "eez_raster.tif")) %>%
  crop(benefits_raster)

rlm_code <-
  raster(here("clean_data", "rlm_raster.tif")) %>%
  crop(benefits_raster)

pro_code <-
  raster(here("clean_data","pro_raster.tif")) %>%
  crop(benefits_raster)

eco_code <-
  raster(here("clean_data", "eco_raster.tif")) %>%
  crop(benefits_raster)

hem_code <-
  raster(here("clean_data", "hemispheres.tif")) %>%
  crop(benefits_raster)

mpa_raster <-
  raster(here("clean_data", "mpa_raster.tif")) %>%
  crop(benefits_raster)

area_raster <- raster::area(benefits_raster)

# Load the EEZ vector data
eez_meow <- read_csv(here("clean_data", "intersected_eez_and_meow_feature_info.csv"))

## PROCESSING ##################################################################

cb <-
  stack(                                                                        # Start by creating a raster stack of all features
    iso3n,
    rlm_code,
    pro_code,
    eco_code,
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
    eco_code = eco_raster,
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
  select(iso3, ecoregion, province, realm, iso3n, contains("code")) %>%
  left_join(cb, by = c("iso3n", "rlm_code", "pro_code", "eco_code")) %>%        # Join to the data.frame from rasters
  replace_na(replace = list(mpa = 0)) %>%                                       # Make mpa a dummy variable with MPA = 1 and no MPA = 0
  drop_na(iso3n, cost, benefit) %>%                                             # Drop areas beyond national jurisdiction and areas with no cost / benefit data
  filter(benefit > 0) %>%
  mutate(bcr = benefit / cost,                                                  # Calculate marginal benefit
         mc = cost / benefit) %>%                                               
  drop_na(lat, lon, benefit, cost) %>% 
  mutate(global = "Global") %>% 
  select(global, hemisphere, realm, province, ecoregion, iso3, everything())


## DATA EXPORT ############################################################################
# Export master data
saveRDS(
  master_data,
  file = here(
    "results",
    "processed_data",
    "master_costs_and_benefits.rds"
  )
)

## END OF SCRIPT ##
