

library(startR)
library(rnaturalearth)
library(rmapshaper)
library(sf)
library(ggnewscale)
library(tidyverse)

coast <- ne_countries(returnclass = "sf")

r_target <- "0.30"

file <- list.files(path = file.path(project_path,
                                    "output_data",
                                    "trade_outcomes",
                                    "realm"),
                   pattern = r_target,
                   full.names = T)

data <- read.csv(file) %>% 
  mutate(ratio = savings/ bau_tc)

eez <- st_read(
  file.path(project_path,
            "processed_data",
            "clean_world_eez_v11.gpkg")
) %>% 
  mutate(has_obligations = iso3 %in% unique(data$iso3)) %>% 
  rmapshaper::ms_simplify(keep_shapes = T, sys = T) %>% 
  sf::st_make_valid() %>% 
  filter(iso3 %in% unique(data$iso3))

eez_with_results <- eez %>% 
  left_join(data, by = c("iso3"))

sellers <- eez_with_results %>% 
  filter(transaction == "Sellers") %>% 
  mutate(Sellers = pmin(ratio, 10))

buyers <- eez_with_results %>% 
  filter(transaction == "Buyers") %>% 
  mutate(Buyers = ratio)

dont_participate <- eez_with_results %>%
  filter(transaction == "Doesn't participate")

savings_map <- ggplot() +
  geom_sf(data = coast) +
  geom_sf(data = buyers, aes(fill = Buyers)) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  new_scale_fill() +
  geom_sf(data = sellers, aes(fill = Sellers)) +
  scale_fill_gradient(low = "white", high = "red") +
  geom_sf(data = dont_participate, fill = "gray") +
  ggtheme_map()


lazy_ggsave(plot = savings_map,
            filename = "30_by_segment/realm_savings_map",
            width = 18.7, height = 9)

