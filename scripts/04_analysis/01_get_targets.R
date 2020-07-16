# Load packages
library(startR)
library(rnaturalearth)
library(sf)
library(tidyverse)


# Load data
global_cb <- readRDS(file = file.path(project_path, "processed_data", "global_costs_and_benefits.rds")) %>% 
  mutate(type = "market")

# Load coastline vector
world <- rnaturalearth::ne_countries(returnclass = "sf") %>% 
  select(1)

max_con <- global_cb %>% 
  arrange(desc(benefit)) %>% 
  mutate(tb = cumsum(benefit),
         pct = (1:nrow(.)) / nrow(.)) %>% 
  mutate(type = "maximize conservation")

min_con <- global_cb %>% 
  arrange(benefit) %>% 
  mutate(tb = cumsum(benefit),
         pct = (1:nrow(.)) / nrow(.)) %>% 
  mutate(type = "minimize conservation")

max_cos <- global_cb %>% 
  arrange(desc(cost)) %>% 
  mutate(tb = cumsum(benefit),
         pct = (1:nrow(.)) / nrow(.)) %>% 
  mutate(type = "maximize costs")

min_cos <- global_cb %>% 
  arrange(cost) %>% 
  mutate(tb = cumsum(benefit),
         pct = (1:nrow(.)) / nrow(.)) %>% 
  mutate(type = "minimize costs")

all_curves <- rbind(global_cb,
      max_con,
      min_con,
      max_cos,
      min_cos)

cor_con <- all_curves %>% 
  filter(pct <= 0.3) %>% 
  group_by(type) %>%
  nest() %>%
  mutate(data = map(data, tail, 1)) %>%
  unnest(cols = data) %>% 
  select(pct, tb, type)

ggplot(mapping = aes(x = pct, y = tb, color = type)) +
  geom_line(data = all_curves,
            size = 1) +
  geom_vline(xintercept = 0.3, linetype = "dashed") +
  geom_point(data = cor_con, aes(fill = type), color = "black") +
  geom_segment(data = cor_con, aes(x = 0, xend = 0.3, yend = tb),
               linetype = "dashed") +
  ggtheme_plot() +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  theme(legend.justification = c(0, 1),
        legend.position = c(0, 1)) +
  labs(x = "Percent protected in EEZs",
       y = "Conservation",
       color = "Approach") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  guides(fill = F)

all_curves %>% 
  filter(pct <= 0.3) %>% 
  ggplot() +
  geom_sf(data = world, size = 0.3, fill = "black") +
  geom_raster(aes(x = lon, y = lat, fill = benefit)) +
  facet_wrap(~type, ncol = 2) +
  scale_fill_viridis_c() +
  ggtheme_map() +
  coord_sf(crs = 54009)

desired_cons <- benefit(global_cb, 0.3)




