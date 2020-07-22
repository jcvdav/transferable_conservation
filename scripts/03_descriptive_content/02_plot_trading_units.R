# Load packages
library(startR)
library(cowplot)
library(rmapshaper)
library(sf)
library(tidyverse)


# Load data

eez_h_sum_cb <- readRDS(file = file.path(project_path, "processed_data", "eez_h_sum_costs_and_benefits.rds"))
eez_cb <- readRDS(file = file.path(project_path, "processed_data", "eez_costs_and_benefits.rds"))
rlm_eez_cb <- readRDS(file = file.path(project_path, "processed_data", "rlm_eez_costs_and_benefits.rds"))
pro_eez_cb <- readRDS(file = file.path(project_path, "processed_data", "pro_eez_costs_and_benefits.rds"))
eco_eez_cb <- readRDS(file = file.path(project_path, "processed_data", "eco_eez_costs_and_benefits.rds"))

eez_meow <- st_read(file.path(project_path, "processed_data", "intersected_eez_and_meow.gpkg"))

# How many MEOWS per country?
meows_per_eez <- eez_meow %>% 
  group_by(iso3) %>% 
  summarize(n_rlm = n_distinct(realm),
            n_pro = n_distinct(province),
            n_eco = n_distinct(ecoregion)) %>% 
  ungroup() %>% 
  arrange(desc(n_eco)) %>% 
  ms_simplify(keep_shapes = TRUE)

rlm_per_eez <- ggplot(meows_per_eez, aes(fill = n_rlm)) +
  geom_sf() +
  scale_fill_viridis_c() +
  ggtheme_map() +
  labs(fill = "# of\nRealms") +
  guides(fill = guide_colorbar(frame.colour = "black",
                               ticks.colour = "black"))

pro_per_eez <- ggplot(meows_per_eez, aes(fill = n_pro)) +
  geom_sf() +
  scale_fill_viridis_c() +
  ggtheme_map() +
  labs(fill = "# of\nProvinces") +
  guides(fill = guide_colorbar(frame.colour = "black",
                               ticks.colour = "black"))

eco_per_eez <- ggplot(meows_per_eez, aes(fill = n_eco)) +
  geom_sf() +
  scale_fill_viridis_c() +
  ggtheme_map() +
  labs(fill = "# of\nEcoregions") +
  guides(fill = guide_colorbar(frame.colour = "black",
                               ticks.colour = "black"))

meows_per_eez_plot <- plot_grid(rlm_per_eez,
                           pro_per_eez,
                           eco_per_eez,
                           ncol = 1,
                           labels = "AUTO")

lazy_ggsave(plot = meows_per_eez_plot,
            filename = "meows_per_eez",
            width = 10,
            height = 15)


# How many countries per MEOW?
eezs_per_realm <- eez_meow %>% 
  st_drop_geometry() %>%
  group_by(realm) %>% 
  summarize(n_eez = n_distinct(iso3)) %>% 
  ungroup() %>% 
  mutate(realm = fct_reorder(realm, n_eez))

eezs_per_province <- eez_meow %>% 
  st_drop_geometry() %>% 
  group_by(realm, province) %>% 
  summarize(n_eez = n_distinct(iso3)) %>% 
  ungroup() %>% 
  mutate(province = fct_reorder(province, n_eez))

eezs_per_ecoregion <- eez_meow %>% 
  st_drop_geometry() %>% 
  group_by(realm, province, ecoregion) %>% 
  summarize(n_eez = n_distinct(iso3)) %>% 
  ungroup() %>% 
  arrange(desc(n_eez)) %>% 
  mutate(split = 1:nrow(.) >= 110) %>% 
  mutate(ecoregion = fct_reorder(ecoregion, n_eez)) 


# Barplots

eezs_per_realm_plot <-
  ggplot(eezs_per_realm, aes(x = realm, y = n_eez)) +
  geom_col() +
  coord_flip() +
  labs(x = "Realm", y = "Number of countries") +
  guides(fill = F) +
  ggtheme_plot() +
  scale_y_continuous(expand = c(0, 0)) +
  geom_hline(yintercept = 2, linetype = "dashed", color = "black")

eezs_per_province_plot <-
  ggplot(eezs_per_province, aes(x = province, y = n_eez)) +
  geom_col(color = "black") +
  coord_flip() +
  labs(x = "Province", y = "Number of countries") +
  guides(fill = F) +
  ggtheme_plot() +
  scale_y_continuous(expand = c(0, 0)) +
  geom_hline(yintercept = 2, linetype = "dashed", color = "black")

eezs_per_ecoregion_plot <-
  ggplot(eezs_per_ecoregion, aes(x = ecoregion, y = n_eez)) +
  geom_col(color = "black") +
  coord_flip() +
  labs(x = "Ecoregion", y = "Number of countries") +
  guides(fill = F) +
  ggtheme_plot() +
  scale_y_continuous(expand = c(0, 0)) +
  geom_hline(yintercept = 2, linetype = "dashed", color = "black") +
  facet_wrap(~split, scale = "free_y") + 
  theme(strip.background = element_blank(),
        strip.text.x = element_blank())

# Export plots
lazy_ggsave(plot = eezs_per_realm_plot,
            filename = "eezs_per_realm",
            width = 15, height = 11)

# Export plots
lazy_ggsave(plot = eezs_per_province_plot,
            filename = "eezs_per_province",
            width = 15, height = 20)

# Export plots
lazy_ggsave(plot = eezs_per_ecoregion_plot,
            filename = "eezs_per_ecoregion",
            width = 30, height = 30)

# END OF SCRIPT #






















