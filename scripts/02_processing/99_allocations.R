
library(mapview)
library(startR)
library(here)
library(sf)
library(tidyverse)

polygons <- 
  st_read(here("data", "intersected_eez_and_meow.gpkg"),
          stringsAsFactors = F) %>% 
  filter(iso3 %in% c("CAN", "USA", "MEX"),
         realm == "Temperate Northern Pacific")

wdi_data <-
  read.csv(here("data", "wdi_indicators_by_country.csv"),
           stringsAsFactors = F)

chi_data <- 
  read.csv(here("data", "chi_by_eez_ecoregion.csv"),
           stringsAsFactors = F)


data <- polygons %>% 
  left_join(wdi_data, by = "iso3") %>%
  left_join(chi_data, by = c("iso3", "realm", "province", "ecoregion")) %>% 
  group_by(iso3, realm, area_rea, chi_rea, per_capita_gdp, population) %>% 
  summarize(geom = st_union(geom)) %>% 
  ungroup() %>% 
  mutate(tot_area_rea = sum(area_rea, na.rm = T),
         gdp_share = per_capita_gdp / sum(per_capita_gdp, na.rm = T),
         pop_share = population / sum(population, na.rm = T),
         chi_share = chi_rea / sum(chi_rea, na.rm = T)) %>% 
  mutate(alloc_bau = 0.3 * area_rea,
         alloc_gdp = 0.3 * gdp_share * tot_area_rea,
         alloc_pop = 0.3 * pop_share * tot_area_rea,
         alloc_chi = 0.3 * chi_share * tot_area_rea) %>% 
  select(iso3, contains("alloc")) %>% 
  rmapshaper::ms_simplify() %>% 
  st_transform(4326) %>% 
  st_rotate()

coast <- rnaturalearth::ne_countries(country = c("United States of America", "Canada", "Mexico"), returnclass = "sf") %>% 
  st_rotate() %>% 
  st_crop(data)

data %>% 
  gather(method, alloc, -c(iso3, geom)) %>% 
  ggplot() +
  geom_sf(aes(fill = alloc / 1e6)) +
  # geom_sf(data = coast) +
  facet_wrap(~method) +
  scale_fill_viridis_c() +
  ggtheme_plot() +
  labs(fill = "Conservation obligation\n(km2)")


