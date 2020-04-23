################################################################
################### 03_intersect_eez_and_meow ##################
################################################################
# 
# This script reads teh clean EEZ and MEOW data and intersects 
# the spatial features in them. I then calculate the area
# of each polygon (eez-realm-province-ecoregion), so that
# I can know how much of each habitat is available each grouping
# 
################################################################

## Set up
# Load packages
library(here)
library(janitor)
library(units)
library(sf)
library(tidyverse)
library(lwgeom)

# Load shapefiles
eez <- st_read(here("data", "clean_world_eez_v11.gpkg"))  # Load clean EEZ
meow <- st_read(dsn = here("data", "clean_meow.gpkg"))    # Load clean MEOW

## Process
# Intersect two shapefiles and generate some summary statistics
eez_meow <- st_intersection(eez, meow) %>%                # Intersect features
  mutate(area_ecoregion = st_area(.)) %>%                 # Calculate the area of each ecorgion within a country
  group_by(iso_3, province) %>%                           # Calculate area of each province within a country
  mutate(area_province = sum(area_ecoregion, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(iso_3, realm) %>%                              # Calculate area of each realm within a country
  mutate(area_realm = sum(area_ecoregion, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(iso_3) %>%                                     # Calculate EEZ area
  mutate(area_eez = sum(area_ecoregion, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(ecoregion_div_eez = area_ecoregion / area_eez,  # Calculate ratio of polygon area to EEZ area
         province_div_eez = area_province / area_eez,
         realm_div_eez = area_realm / area_eez) %>% 
  mutate_at(.vars = vars(ecoregion_div_eez,              # Convert to numeric
                         province_div_eez,
                         realm_div_eez),
            .funs = drop_units) %>% 
  select(mrgid, geoname, iso_3, area_eez,
         ecoregion, eco_code, area_ecoregion, ecoregion_div_eez,
         province, pro_code, area_province, province_div_eez,
         realm, rlm_code, area_realm, realm_div_eez)

mapview::mapview(select(eez_meow, ecoregion_div_eez))

eez_meow %>%
  ggplot(aes(x = realm, y = ecoregion_div_eez, fill = province, color = ecoregion)) + 
  geom_col(size = 1) +
  scale_fill_viridis_d() +
  startR::ggtheme_plot() +
  facet_wrap(~ iso_3, scales = "free_x") +
  theme(legend.position = "none")

