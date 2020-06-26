# Load packages
library(startR)
library(raster)
library(rmapshaper)
library(sf)
library(tidyverse)

# Load data
# Benefits raster
benefits_raster <- raster(file.path(ng_data_path, "/03_output/14_upgrade_weak_MPAs/ranking_raster.tif"))

# Costs raster
costs_raster <- raster(file.path(project_path, "data", "fishing_days_as_costs.tif"))

# EEZ_MEOW raster
eez_meow_raster <- raster(file.path(project_path, "data", "eez_meow_raster.tif"))

eez_meow <- st_read(file.path(project_path, "data", "intersected_eez_and_meow.gpkg"))

# Extract codes for each pixel

cb <- stack(eez_meow_raster, benefits_raster, costs_raster) %>% 
  as.data.frame(xy = T) %>% 
  rename(lon = x, lat = y, iso3n = eez_meow_raster) %>% 
  drop_na()

master_data <- eez_meow %>% 
  # filter(iso3 %in% c("CAN")) %>%
  st_drop_geometry() %>% 
  select(iso3, iso3n) %>% 
  distinct() %>% 
  left_join(cb, by = "iso3n") %>% 
  select(lon, lat, iso3, benefit = ranking_raster, cost = fishing_days_as_costs) %>% 
  mutate(mb = benefit / cost)

global_data <- master_data %>% 
  arrange(desc(benefit)) %>%
  mutate(tb = cumsum(benefit),
         tc = cumsum(cost),
         pct = (1:nrow(.)) / nrow(.))
  
country_data <- master_data %>% 
  group_by(iso3) %>% 
  arrange(desc(mb)) %>%
  mutate(tb = cumsum(benefit),
         tc = cumsum(cost)) %>% 
  ungroup()
# FIGURES

# Desired level of conservation
ggplot(global_data, aes(x = pct, y = tb)) +
  geom_line()

# Supply curves
ggplot(mapping = aes(x = tb, y = cost)) +
  geom_line(data = global_data) +
  # geom_line(data = country_data, aes(color = iso3)) +
  guides(color = F)




lggplot(data, aes(benefit, cost)) +
  geom_point()




























