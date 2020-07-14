# Load packages
library(startR)
library(raster)
library(rmapshaper)
library(sf)
library(tidyverse)

# Load data
# Benefits raster
benefits_raster <- raster(file.path(ng_data_path, "03_output/05_spp_wts_smts_provs_MPAs/ranking_raster.tif"))

# Costs raster
costs_raster <- -1 * raster(file.path(ng_data_path, "03_output/07_food/delta_v_raster_a2.tif"))

# EEZ_MEOW raster
eez_meow_raster <- raster(file.path(project_path, "data", "eez_meow_raster.tif"))

eez_meow <- st_read(file.path(project_path, "data", "intersected_eez_and_meow.gpkg"))

# Extract codes for each pixel
cb <- stack(eez_meow_raster, benefits_raster, costs_raster) %>% 
  as.data.frame(xy = T) %>% 
  rename(lon = x, lat = y, iso3n = eez_meow_raster) %>% 
  drop_na()

# Create a master dataset with all the metadata for eahc pixel
master_data <- eez_meow %>% 
  # filter(iso3 %in% c("CAN")) %>%
  st_drop_geometry() %>% 
  select(iso3, iso3n, ecoregion, province, realm) %>% 
  distinct() %>% 
  left_join(cb, by = "iso3n") %>% 
  select(lon, lat, iso3, ecoregion, province, realm, benefit = ranking_raster, cost = delta_v_raster_a2) %>% 
  mutate(mb = benefit / cost,
         neg = mb >= 0)

global_data <- master_data %>% 
  arrange(neg, desc(mb)) %>%
  mutate(tb = cumsum(benefit),
         tc = cumsum(cost),
         pct = (1:nrow(.)) / nrow(.))
  
country_data <- master_data %>% 
  group_by(iso3) %>% 
  arrange(neg, desc(mb)) %>%
  mutate(tb = cumsum(benefit),
         tc = cumsum(cost),
         pct = (1:length(tc)) / length(tc)) %>% 
  ungroup()

country_realm <- master_data %>% 
  group_by(iso3, realm) %>% 
  arrange(neg, desc(mb)) %>%
  mutate(tb = cumsum(benefit),
         tc = cumsum(cost),
         pct = (1:length(tc)) / length(tc)) %>% 
  ungroup()

country_province <- master_data %>% 
  group_by(iso3, province) %>% 
  arrange(neg, desc(mb)) %>%
  mutate(tb = cumsum(benefit),
         tc = cumsum(cost),
         pct = (1:length(tc)) / length(tc)) %>% 
  ungroup()

country_ecoregion <- master_data %>% 
  group_by(iso3, ecoregion) %>% 
  arrange(neg, desc(mb)) %>%
  mutate(tb = cumsum(benefit),
         tc = cumsum(cost),
         pct = (1:length(tc)) / length(tc)) %>% 
  ungroup()


























