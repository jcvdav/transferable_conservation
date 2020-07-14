# Load packages
library(startR)
library(raster)
library(sf)
library(tidyverse)

# Load data
# Benefits raster
benefits_raster <- raster(file.path(ng_data_path, "03_output/05_spp_wts_smts_provs_MPAs/ranking_raster.tif"))

# Costs raster
costs_raster <- -1 * raster(file.path(ng_data_path, "03_output/07_food/delta_v_raster_a2.tif"))

# Load rasters
iso3n <- raster(file.path(project_path, "processed_data", "eez_raster.tif"))
rlm_code <- raster(file.path(project_path, "processed_data", "rlm_raster.tif"))
pro_code <- raster(file.path(project_path, "processed_data", "pro_raster.tif"))
eco_code <- raster(file.path(project_path, "processed_data", "eco_raster.tif"))

eez_meow <- st_read(file.path(project_path, "processed_data", "intersected_eez_and_meow.gpkg"))

# Extract codes for each pixel
cb <- stack(iso3n, rlm_code, pro_code, eco_code, benefits_raster, costs_raster) %>% 
  as.data.frame(xy = T) %>% 
  rename(lon = x, lat = y,
         iso3n = eez_raster, rlm_code = rlm_raster, pro_code = pro_raster, eco_code = eco_raster,
         benefit = ranking_raster,
         cost = delta_v_raster_a2) %>% 
  drop_na()

# Create a master dataset with all the metadata for eahc pixel
master_data <- eez_meow %>% 
  st_drop_geometry() %>% 
  select(iso3, ecoregion, province, realm, iso3n, contains("code")) %>% 
  left_join(cb, by = c("iso3n", "rlm_code", "pro_code", "eco_code")) %>% 
  select(lon, lat, iso3, ecoregion, province, realm, benefit, cost) %>% 
  mutate(mb = benefit / cost,
         neg = mb >= 0) %>% 
  drop_na(lat, lon)

global_data <- master_data %>% 
  arrange(neg, desc(mb)) %>%
  mutate(tb = cumsum(benefit),
         tc = cumsum(cost),
         pct = (1:nrow(.)) / nrow(.))
  
eez_data <- master_data %>% 
  group_by(iso3) %>% 
  arrange(neg, desc(mb)) %>%
  mutate(tb = cumsum(benefit),
         tc = cumsum(cost),
         pct = (1:length(tc)) / length(tc)) %>% 
  ungroup()

rlm_eez <- master_data %>% 
  group_by(iso3, realm) %>% 
  arrange(neg, desc(mb)) %>%
  mutate(tb = cumsum(benefit),
         tc = cumsum(cost),
         pct = (1:length(tc)) / length(tc)) %>% 
  ungroup()

pro_eez <- master_data %>% 
  group_by(iso3, province) %>% 
  arrange(neg, desc(mb)) %>%
  mutate(tb = cumsum(benefit),
         tc = cumsum(cost),
         pct = (1:length(tc)) / length(tc)) %>% 
  ungroup()

eco_eez <- master_data %>% 
  group_by(iso3, ecoregion) %>% 
  arrange(neg, desc(mb)) %>%
  mutate(tb = cumsum(benefit),
         tc = cumsum(cost),
         pct = (1:length(tc)) / length(tc)) %>% 
  ungroup()

# Export the data
saveRDS(master_data,
        file = file.path(project_path, "processed_data", "master_costs_and_benefits.rds"))
saveRDS(global_data,
        file = file.path(project_path, "processed_data", "global_costs_and_benefits.rds"))
saveRDS(eez_data,
        file = file.path(project_path, "processed_data", "eez_costs_and_benefits.rds"))

saveRDS(rlm_eez,
        file = file.path(project_path, "processed_data", "rlm_eez_costs_and_benefits.rds"))
saveRDS(pro_eez,
        file = file.path(project_path, "processed_data", "pro_eez_costs_and_benefits.rds"))
saveRDS(eco_eez,
        file = file.path(project_path, "processed_data", "eco_eez_costs_and_benefits.rds"))


























