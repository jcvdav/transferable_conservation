####################################################
# 
# Provinces
# 
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
library(kableExtra)
library(rnaturalearth)
library(sf)
library(tidyverse)

# Load data
### For each province and country
pro_eez_cb <- readRDS(
  file = file.path( project_path, "processed_data", "pro_eez_costs_and_benefits.rds")
) 
### Horizontally summed for each province
pro_h_sum <- readRDS(
  file = file.path( project_path, "processed_data", "pro_h_sum_costs_and_benefits.rds")
) 

# Load a coastline
coast <- ne_countries(returnclass = "sf") %>% 
  st_transform(crs = epsg_moll) 

# Load EEZ geopackage
eez <- st_read(
  file.path(project_path,
            "processed_data",
            "intersected_eez_meow_hem.gpkg")
) %>% 
  mutate(has_obligations = iso3 %in% unique(pro_eez_cb$iso3)) %>%
  rmapshaper::ms_simplify(keep_shapes = T, sys = T) %>%
  st_make_valid() %>%
  filter(iso3 %in% unique(pro_eez_cb$iso3))

conserving_nations <- pro_eez_cb %>% 
  select(iso3, province) %>% 
  distinct()

# PROCESSING ############################################################################################


pro_trading_prices <- pro_h_sum %>% 
  group_by(province) %>% 
  filter(pct_area <= 0.3) %>% 
  slice_max(mc) %>% 
  ungroup() %>% 
  select(province, trading_price = mc)
## Identify conserved patches
# Get the most efficient trading price

# Filter to keep only the protected places
bau <- pro_eez_cb %>% 
  filter(pct_area <= 0.3) %>%                       # Keep the most efficient 30% of each country - province
  mutate(approach = "bau")

mkt <- pro_eez_cb %>% 
  left_join(pro_trading_prices, by = c("province")) %>%
  filter(mc <= trading_price) %>%              # Keep all patches in each country with a cost < trading price
  mutate(approach = "mkt")

## Calculate country-level summaries
# For BAU
realized_bau_cb <- bau %>% 
  group_by(approach, iso3, province) %>% 
  summarize(bau_tb = sum(benefit, na.rm = T),
            bau_tc = sum(cost, na.rm = T),
            mc_stop = max(mc, na.rm = T),
            bau_area = n()) %>% 
  ungroup()

# For a market
realized_mkt_cb <- mkt %>% 
  group_by(approach, iso3, province, trading_price) %>% 
  summarize(mkt_tb = sum(benefit, na.rm = T),
            mkt_tc = sum(cost, na.rm = T),
            mkt_area = n()) %>% 
  ungroup()

#
combined_outcomes <- conserving_nations %>% 
  left_join(realized_mkt_cb, by = c("iso3", "province")) %>%
  left_join(realized_bau_cb, by = c("iso3", "province")) %>% 
  replace_na(replace = list(
    mkt_tb = 0, mkt_tc = 0, mkt_area = 0,
    bau_tb = 0, bau_tc = 0, bau_area = 0)) %>% 
  select(-contains("app")) %>% 
  mutate(mkt_tc2 = mkt_tc + ((bau_tb - mkt_tb) * trading_price),
         mkt_tc = ifelse(mkt_tc2 <=0, bau_tc, mkt_tc2)) %>% 
  select(-mkt_tc2)

# Find stopping prices
stops <- full_join(realized_mkt_cb, realized_bau_cb, by = c("iso3", "province")) %>% 
  select(-contains("app")) %>% 
  filter(bau_tb <= mkt_tb) %>% 
  select(iso3, mc_stop, province) %>% 
  mutate(approach = "mkt")

gains_from_trade <- full_join(realized_mkt_cb, realized_bau_cb, by = c("iso3", "province"))  %>% 
  select(province, contains("mkt"), contains("bau")) %>% 
  select(-contains("stop")) %>% 
  group_by(province) %>% 
  summarize_all(sum, na.rm = T) %>% 
  gather(variable, value, -province) %>% 
  separate(variable, into = c("approach", "variable")) %>% 
  spread(approach, value) %>% 
  mutate(difference = bau - mkt,
         ratio = mkt / bau)

## FIGURES #########################################################################
got_paid <- combined_outcomes %>% 
  mutate(gets_paid = mc_stop <= trading_price,
         mkt_gains = bau_tc - mkt_tc) %>% 
  select(iso3, province, gets_paid, bau_tc, mkt_tc, mkt_gains) %>% 
  pivot_longer(cols = c(gets_paid, bau_tc, mkt_tc, mkt_gains), names_to = "variable", values_to = "value") %>% 
  mutate(label = case_when(variable == "bau_tc" ~ "A) Business-as-usual",
                           variable == "mkt_tc" ~ "B) Market-based (global)",
                           variable == "mkt_gains" ~ "C) Gains from trade",
                           T ~ NA_character_))

# Plot the two states of the world
two_states_map <-
  pro_eez_cb %>% 
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

lazy_ggsave(plot = two_states_map,
            filename = "two_states_map_pro",
            width = 10,
            height = 10)

percent_of_bau_costs <- 
  gains_from_trade %>% 
  filter(variable == "tc") %>%
  select(bau, mkt) %>% 
  summarize_all(sum, na.rm = T) %>% 
  mutate(ratio = mkt / bau) %>% 
  pull(ratio)

gains_from_trade %>%
  group_by(variable) %>%
  summarize(difference = sum(difference, na.rm = T)) %>% 
  spread(variable, difference) %>% 
  mutate(percent_of_bau = percent_of_bau_costs,
         market = "province",
         segments = 60L) %>% 
  saveRDS(file = file.path(project_path, "output_data", "gains_from_trade_pro.rds"))

gains_from_trade %>% 
  mutate(variable = case_when(variable == "area" ~ "Area",
                              variable == "tb" ~ "Biodiversity",
                              variable == "tc" ~ "Costs")) %>% 
  knitr::kable(format = "latex",
               digits = 2, 
               col.names = c("province", "Variable", "BAU", "Market", "Difference", "Ratio"),
               label = "pro-gains-from-trade",
               caption = "Gains from trade from protecting 73.65 units of biodiversity. Difference shows BAU - Market, ratio shows Market / BAU.") %>% 
  # kableExtra::collapse_rows() %>% 
  cat(file = here::here("results", "tab", "pro_gains_from_trade.tex"))



















