



# SET UP ################################################################################################
# Load packages
library(startR)
library(cowplot)
library(tidyverse)

# Load data
## Global data
global_cb <- readRDS(
  file = file.path(project_path,"processed_data","global_costs_and_benefits.rds")
)

## National data
eez_cb <- readRDS(
  file = file.path(project_path, "processed_data", "eez_costs_and_benefits.rds")
)

## Realm eez
rlm_eez_cb <- readRDS(
  file = file.path( project_path, "processed_data", "rlm_eez_costs_and_benefits.rds")
) 


# PROCESSING ############################################################################################


target_cons <- benefit(global_cb, 0.3)
trading_price <- global_cb %>% 
  filter(tb <= target_cons) %>% 
  tail(1) %>% 
  pull(mc)

eez_cb %>% 
  mutate(protected = cost <= trading_price) %>% 
  ggplot(aes(lon, lat, fill = protected)) +
  geom_raster() +
  scale_fill_brewer(palette = "Set1")

protected_global <- global_cb %>% 
  filter(mc <= trading_price)

protected_eez <- eez_cb %>% 
  filter(mc <= trading_price)

protected_rlm_eez <- rlm_eez_cb %>% 
  filter(mc <= trading_price)

ggplot() +
  geom_line(data = protected_rlm_eez,
            mapping = aes(x = tb, y = mc, color = iso3)) +
  # geom_line(data = protected_global, aes(x = tb, y = mc)) +
  guides(color = F) +
  scale_color_viridis_d() +
  ggtheme_plot() +
  labs(x = "Conservation",
       y = "Marginal Costs") +
  facet_wrap(~realm)


protected_eez %>% 
  group_by(iso3) %>% 
  summarize(tb = sum(benefit),
            tc = sum(cost)) %>% 
  arrange(tc)



##### testing maps

a <- group_by(rlm, realm) %>% 
  nest() %>% 
  mutate(target_benefit = map(data, benefit, area = 0.3)) %>% 
  select(-data) %>% 
  unnest(target_benefit)

tps <- rlm %>% 
  group_by(realm) %>% 
  nest() %>% 
  left_join(a, by = "realm") %>% 
  mutate(trading_price = map2(data, target_benefit, trading_price)) %>% 
  select(-data) %>% 
  unnest(trading_price)

ggplot(rlm, aes(x = tb, y = mc, color = realm)) +
  geom_line() +
  geom_point(data = tps, aes(x = target_benefit, y = trading_price, fill = realm)) +
  scale_y_continuous(limits = c(-1000, 1000))








































