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
  st_transform(crs = epsg_moll) %>% 
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
# Get the most efficient trading price
# trading_price <- trading_prices %>% 
  # filter(type == "efficient") %>% 
  # pull(mc)

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
            bau_mc = max(mc, na.rm = T),
            bau_area = n() * 2500) %>% 
  ungroup()

# For a market
realized_mkt_cb <- mkt %>% 
  group_by(approach, iso3) %>% 
  summarize(mkt_tb = sum(benefit, na.rm = T),
            mkt_tc = sum(cost, na.rm = T),
            mkt_mc = max(mc, na.rm = T),
            mkt_area = n() * 2500) %>% 
  ungroup()

# Create a data.frame with the combined summarized outcomes
combined_outcomes <- conserving_nations %>% 
  left_join(realized_mkt_cb, by = "iso3") %>%
  left_join(realized_bau_cb, by = "iso3") %>% 
  replace_na(replace = list(
    mkt_tb = 0, mkt_tc = 0, mkt_area = 0,
    bau_tb = 0, bau_tc = 0, bau_area = 0)) %>% 
  select(-contains("app")) %>% 
  mutate(transaction = ifelse(mkt_tc < bau_tc, "Pays", "Gets paid"),
         mkt_tc_else = mkt_tc - bau_tc) %>% 
  select(iso3, contains("bau"), contains("mkt"), transaction)

# Find stopping prices
stops <- combined_outcomes %>% 
  filter(bau_tb <= mkt_tb) %>% 
  select(iso3, bau_mc) %>% 
  mutate(approach = "mkt")


gains_from_trade <- combined_outcomes %>% 
  select_if(is.numeric) %>% 
  select(-contains("stop")) %>% 
  summarize_all(sum, na.rm = T) %>% 
  gather(variable, value) %>% 
  separate(variable, into = c("approach", "variable")) %>% 
  spread(approach, value) %>% 
  mutate(difference = bau - mkt,
         ratio = mkt / bau)

got_paid <- combined_outcomes %>% 
  mutate(gets_paid = mc_stop <= trading_price,
         mkt_gains = bau_tc - mkt_tc) %>% 
  select(iso3, gets_paid, bau_tc, mkt_tc, mkt_gains) %>% 
  pivot_longer(cols = c(gets_paid, bau_tc, mkt_tc, mkt_gains), names_to = "variable", values_to = "value") %>% 
  mutate(label = case_when(variable == "bau_tc" ~ "A) Business-as-usual",
                           variable == "mkt_tc" ~ "B) Market-based (global)",
                           variable == "mkt_gains" ~ "C) Gains from trade",
                           T ~ NA_character_))

eez_with_results <- eez %>% 
  left_join(combined_outcomes, by = "iso3")

## FIGURES #########################################################################
## Plot the supply curves where they stop
benefit_supply_curves <- bau %>% 
  select(-min_pct) %>% 
  rbind(mkt) %>%
  left_join(stops, by = c("iso3", "approach"),
            fill = list(mc_stop = 0, mkt_tb = 0, mkt_tc = 0, mkt_area = 0)) %>% 
  mutate(mkt_gain = mc >= mc_stop,
         approach = ifelse(approach == "bau", "BAU", "Market")) %>% 
  replace_na(replace = list(mkt_gain = F)) %>%
  ggplot(aes(x = tb, y = mc, group = iso3, color = mkt_gain)) +
  geom_line(size = 0.2) +
  geom_hline(yintercept = trading_price, linetype = "dashed") +
  facet_wrap(~approach, scales = "free_y") +
  scale_color_brewer(palette = "Set1", direction = -1) +
  # lims(y = c(0, trading_price * 2)) +
  ggtheme_plot() +
  labs(x = "Quality-weighted area protected",
       y = "Marginal Costs ($ USD / km2)",
       caption = "NOTE: Axis have been cropped for visualization purposes") +
  guides(color = FALSE)

map_contrasting_scenarios <- ggplot() +
  geom_sf(data = filter(eez_with_results,!variable == "gets_paid"),
          aes(fill = value), color = "black") +
  geom_sf(data = coast, color = "black") +
  facet_wrap(~label, ncol = 1) +
  scale_fill_viridis_c(na.value = "gray") +
  ggtheme_map() +
  guides(fill = guide_colorbar(title = "Total costs\nmillion USD",
                               frame.colour = "black",
                               ticks.colour = "black"))

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

lazy_ggsave(plot = benefit_supply_curves,
            filename = "equilibrum_supply_curves",
            width = 12,
            height = 6)

lazy_ggsave(plot = two_states_map,
            filename = "two_states_map",
            width = 20,
            height = 10)

lazy_ggsave(plot = map_contrasting_scenarios,
            filename = "map_contrasting_scenarios_global",
            width = 15,
            height = 20)

lazy_ggsave(plot = map_of_trade,
            filename = "map_of_trade",
            width = 20,
            height = 10)

# Tables
percent_of_bau_costs <- gains_from_trade %>% 
  filter(variable == "tc") %>% 
  pull(ratio)

gains_from_trade %>% 
  select(variable, difference) %>% 
  spread(variable, difference) %>% 
  mutate(percent_of_bau = percent_of_bau_costs,
         market = "global",
         segments = 1L) %>% 
  saveRDS(file = file.path(project_path, "output_data", "gains_from_trade_glb.rds"))

gains_from_trade %>% 
  mutate(variable = c("Area", "Biodiversity benefits", "Total costs")) %>% 
  knitr::kable(format = "latex",
               digits = 2, 
               col.names = c("Variable", "BAU", "Market", "Difference", "Ratio"),
               label = "gains-from-trade",
               caption = "Gains from trade from protecting 71.56 units of biodiversity. Difference shows BAU - Market, ratio shows Market / BAU.") %>% 
  cat(file = here::here("results", "tab", "gains_from_trade.tex"))
























