####################################################
# Now that we have the targets and prices, we can 
# move forward and identify where biodiversity is
# protected, and the associated costs of doing so.
# 
# The benefits should be equivalent, since we are
# using the same target.
####################################################

# SET UP ################################################################################################
# Load packages
library(startR)
library(cowplot)
library(ggnewscale)
library(rnaturalearth)
library(sf)
library(tidyverse)

# Load data
## Global data
eez_h_sum_cb <- readRDS(
  file = file.path(project_path,"processed_data","eez_h_sum_costs_and_benefits.rds")
)

## National data
eez_cb <- readRDS(
  file = file.path(project_path, "processed_data", "eez_costs_and_benefits.rds")
)

## Read trading prices
trading_price <- eez_h_sum_cb %>% 
  filter(pct <= 0.3) %>% 
  pull(mc) %>% 
  max()

# Load a coastline
coast <- ne_countries(returnclass = "sf") %>% 
  mutate(in_mkt = iso_a3 %in% unique(eez_cb$iso3))

# Load EEZ geopackage
eez <- st_read(
  file.path(project_path,
            "processed_data",
            "clean_world_eez_v11.gpkg")
  ) %>% 
  mutate(has_obligations = iso3 %in% unique(eez_cb$iso3)) %>% 
  rmapshaper::ms_simplify(keep_shapes = T, sys = T) %>% 
  st_make_valid() %>% 
  filter(iso3 %in% unique(eez_cb$iso3))

# PROCESSING ############################################################################################

## Set up a data.frame of nation ISO3s that participate
conserving_nations <- eez_cb %>% 
  select(iso3) %>% 
  distinct()

## Identify conserved patches

# Filter to keep only the protected places
bau <- eez_cb %>% 
  group_by(iso3) %>% 
  mutate(min_pct = min(pct, na.rm = T)) %>% 
  ungroup() %>% 
  filter(pct <= 0.3 | pct <= min_pct) %>%      # Keep the most efficient 30% of each country OR the first patch
  mutate(approach = "bau")
  
mkt <- eez_cb %>% 
  filter(mc <= trading_price) %>%              # Keep all patches in each country with a cost < trading price
  mutate(approach = "mkt")

## Calculate country-level summaries
# For BAU
realized_bau_cb <- bau %>% 
  group_by(approach, iso3) %>% 
  summarize(bau_tb = sum(benefit, na.rm = T),
            bau_tc = sum(cost, na.rm = T),
            bau_mc = max(mc, na.rm = T)) %>% 
  ungroup()

# For a market
realized_mkt_cb <- mkt %>% 
  group_by(approach, iso3) %>% 
  summarize(mkt_tb = sum(benefit, na.rm = T),
            mkt_tc = sum(cost, na.rm = T),
            mkt_mc = max(mc, na.rm = T)) %>% 
  ungroup()

# Create a data.frame with the combined summarized outcomes
combined_outcomes <- conserving_nations %>% 
  left_join(realized_mkt_cb, by = "iso3") %>%
  left_join(realized_bau_cb, by = "iso3") %>% 
  replace_na(replace = list(
    mkt_tb = 0, mkt_tc = 0, mkt_area = 0,
    bau_tb = 0, bau_tc = 0, bau_area = 0)) %>% 
  select(-contains("app")) %>% 
  mutate(transaction = ifelse(mkt_tc < bau_tc, "Buyers", "Sellers"),
         rect = abs(bau_tb - mkt_tb) * trading_price,
         mkt_tc_b = bau_tc - mkt_tc - rect,
         mkt_tc_s = rect - mkt_tc + bau_tc,
         savings = ifelse(mkt_tb < bau_tb, mkt_tc_b, mkt_tc_s),
         ratio = savings / bau_tc) %>% 
  select(iso3, contains("bau"), contains("mkt"), savings, transaction, rect, ratio, everything())

# Find stopping prices
stops <- combined_outcomes %>% 
  filter(bau_tb <= mkt_tb) %>% 
  select(iso3, bau_mc) %>% 
  mutate(approach = "mkt")

eez_with_results <- eez %>% 
  left_join(combined_outcomes, by = "iso3")

## FIGURES #########################################################################

sellers <- eez_with_results %>% 
  filter(transaction == "Sellers") %>% 
  mutate(Sellers = pmin(ratio, 10))

buyers <- eez_with_results %>% 
  filter(!transaction == "Sellers") %>% 
  mutate(Buyers = ratio)

savings_map <- ggplot() +
  geom_sf(data = coast) +
  geom_sf(data = buyers, aes(fill = Buyers)) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  new_scale_fill() +
  geom_sf(data = sellers, aes(fill = Sellers)) +
  scale_fill_gradient(low = "white", high = "red") +
  ggtheme_map()

map_of_trade <- eez_with_results %>% 
  ggplot() +
  geom_sf(aes(fill = transaction), color = "black") +
  geom_sf(data = coast, color = "black") +
  scale_fill_brewer(palette = "Set1", direction = -1, na.value = "gray") +
  ggtheme_map() +
  guides(fill = guide_legend(title = "Transaction")) +
  theme(legend.position = "bottom")

# Plot the two states of the world
two_states_map <- eez_cb %>% 
  select(lon, lat, benefit) %>% 
  left_join(bau %>% select(lon, lat) %>% mutate(a = 1), by = c("lon", "lat")) %>% 
  left_join(mkt %>% select(lon, lat) %>% mutate(b = 1), by = c("lon", "lat")) %>% 
  mutate(status = case_when(a == 1 & b == 1 ~ "Protected anyway",
                            a == 1 & is.na(b) ~ "Protected only under BAU",
                            is.na(a) & b == 1 ~ "Protected only under market",
                            is.na(a) & is.na(b) ~ NA_character_)) %>% 
  drop_na(status) %>% 
  ggplot() +
  geom_sf(data = coast, color = "black", fill = "transparent") +
  geom_tile(aes(x = lon, y = lat, fill = status)) +
  ggtheme_map() +
  scale_fill_viridis_d(na.value = "gray") +
  guides(fill = guide_legend(title = "Status")) +
  theme(legend.position = "bottom")

## EXPORT FIGURES #########################################################################

lazy_ggsave(plot = savings_map,
            filename = "30_global/savings_map",
            width = 20,
            height = 10)

lazy_ggsave(plot = map_of_trade,
            filename = "30_global/map_of_trade",
            width = 20,
            height = 10)

lazy_ggsave(plot = two_states_map,
            filename = "30_global/two_states_map",
            width = 20,
            height = 10)
