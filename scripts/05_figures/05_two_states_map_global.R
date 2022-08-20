

# Load data
eez_cb <-
  readRDS(
    file = file.path(
      project_path,
      "processed_data",
      "supply_curves",
      "with_mpas",
      "global_eez_supply_curves_with_mpas.rds"
    )) %>% 
  mutate(pixel_fraction = pmin(1 - ((tb - ((tb * 0.3) / pct)) / benefit), 1)) %>% 
  filter(pixel_fraction >= 0) 
  select(lat, lon) %>% 
  mutate(bau = 1)

eez_h_sum_cb <-
  readRDS(file = file.path(
    project_path,
    "processed_data",
    "supply_curves",
    "with_mpas",
    "global_supply_curve_with_mpas.rds"
  )) %>% 
  mutate(pixel_fraction = pmin(1 - ((tb - ((tb * 0.3) / pct)) / benefit), 1)) %>% 
  filter(pixel_fraction >= 0) %>% 
  select(lat, lon) %>% 
  mutate(mkt = 1)

coast <- ne_countries(returnclass = "sf")

when <- full_join(eez_h_sum_cb, eez_cb, by = c("lat", "lon")) %>% 
  replace_na(list("bau" = 0,
                  "mkt" = 0)) %>% 
  mutate(both = mkt + bau,
         protected = case_when(mkt == 1 & bau == 1 ~ "Protected both ways",
                              mkt == 0 & bau == 1 ~ "Protected only under BAU",
                              mkt == 1 & bau == 0 ~ "Protected only under MKT")) %>% 
  ggplot() +
  geom_sf(data = coast, color = "black", fill = "transparent") +
  geom_tile(aes(x = lon, y = lat, fill = protected)) +
  scale_fill_viridis_d() +
  ggtheme_map() +
  theme(legend.position = "bottom")


lazy_ggsave(plot = when,
            filename = "30_by_segment/two_states_map",
            width = 20,
            height = 10)
