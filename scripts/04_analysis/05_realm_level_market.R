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
library(kableExtra)
library(rnaturalearth)
library(sf)
library(tidyverse)

# Load data
### For each realm and country
rlm_eez_cb <- readRDS(
  file = file.path( project_path, "processed_data", "rlm_eez_costs_and_benefits.rds")
) 
### Horizontally summed for each realm
rlm_h_sum <- readRDS(
  file = file.path( project_path, "processed_data", "rlm_h_sum_costs_and_benefits.rds")
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
  mutate(has_obligations = iso3 %in% unique(rlm_eez_cb$iso3)) %>%
  rmapshaper::ms_simplify(keep_shapes = T, sys = T) %>%
  st_make_valid() %>%
  filter(iso3 %in% unique(rlm_eez_cb$iso3))

conserving_nations <- rlm_eez_cb %>% 
  select(iso3, realm) %>% 
  distinct()

# PROCESSING ############################################################################################


rlm_trading_prices <- rlm_h_sum %>% 
  group_by(realm) %>% 
  filter(pct_area <= 0.3) %>% 
  slice_max(mc) %>% 
  ungroup() %>% 
  select(realm, trading_price = mc)

## Identify conserved patches
# Get the most efficient trading price

# Filter to keep only the protected places
bau <- rlm_eez_cb %>% 
  group_by(iso3, realm) %>% 
  mutate(min_pct = min(pct, na.rm = T)) %>% 
  ungroup() %>% 
  filter(pct <= 0.3 | pct <= min_pct) %>%                       # Keep the most efficient 30% of each country - realm
  mutate(approach = "bau")

mkt <- rlm_eez_cb %>% 
  left_join(rlm_trading_prices, by = c("realm")) %>%
  filter(mc <= trading_price) %>%              # Keep all patches in each country with a cost < trading price
  mutate(approach = "mkt")

## Calculate country-level summaries
# For BAU
realized_bau_cb <- bau %>% 
  group_by(approach, iso3, realm) %>% 
  summarize(bau_tb = sum(benefit, na.rm = T),
            bau_tc = sum(cost, na.rm = T),
            mc_stop = max(mc, na.rm = T),
            bau_area = n() * 2500) %>% 
  ungroup()

# For a market
realized_mkt_cb <- mkt %>% 
  group_by(approach, iso3, realm, trading_price) %>% 
  summarize(mkt_tb = sum(benefit, na.rm = T),
            mkt_tc = sum(cost, na.rm = T),
            mkt_area = n() * 2500) %>% 
  ungroup()

#Something
combined_outcomes <- conserving_nations %>% 
  left_join(realized_mkt_cb, by = c("iso3", "realm")) %>%
  left_join(realized_bau_cb, by = c("iso3", "realm")) %>% 
  replace_na(replace = list(
    mkt_tb = 0, mkt_tc = 0, mkt_area = 0,
    bau_tb = 0, bau_tc = 0, bau_area = 0)) %>% 
  select(-contains("app")) %>% 
  mutate(mkt_tc2 = mkt_tc + ((bau_tb - mkt_tb) * trading_price),
         mkt_tc = ifelse(mkt_tc2 <=0, bau_tc, mkt_tc2)) %>% 
  select(-mkt_tc2)

# Find stopping prices
stops <- combined_outcomes %>% 
  filter(bau_tb <= mkt_tb) %>% 
  select(iso3, realm, mc_stop) %>% 
  mutate(approach = "mkt")

gains_from_trade <- combined_outcomes %>% 
  # select_if(is.numeric) %>% 
  select(-contains("stop"), -c(iso3, trading_price)) %>% 
  group_by(realm) %>% 
  summarize_all(sum, na.rm = T) %>% 
  gather(variable, value, -realm) %>% 
  separate(variable, into = c("approach", "variable")) %>% 
  spread(approach, value) %>% 
  mutate(difference = bau - mkt,
         ratio = mkt / bau)


got_paid <- combined_outcomes %>% 
  mutate(gets_paid = mc_stop <= trading_price,
         mkt_gains = bau_tc - mkt_tc) %>% 
  select(iso3, realm, gets_paid, bau_tc, mkt_tc, mkt_gains) %>% 
  pivot_longer(cols = c(gets_paid, bau_tc, mkt_tc, mkt_gains), names_to = "variable", values_to = "value") %>% 
  mutate(label = case_when(variable == "bau_tc" ~ "A) Business-as-usual",
                           variable == "mkt_tc" ~ "B) Market-based (global)",
                           variable == "mkt_gains" ~ "C) Gains from trade",
                           T ~ NA_character_))

eez_with_results <- eez %>% 
  left_join(got_paid, by = c("iso3", "realm"))

## FIGURES #########################################################################
## Plot the supply curves where they stop
benefit_supply_curves <- bau %>% 
  select(-min_pct) %>% 
  rbind(mkt %>% select(-trading_price)) %>%
  left_join(stops, by = c("iso3", "realm", "approach"), fill = list(mc_stop = 0)) %>% 
  mutate(mkt_gain = mc >= mc_stop,
         approach = ifelse(approach == "bau", "BAU", "Market")) %>% 
  replace_na(replace = list(mkt_gain = F)) %>% 
  ggplot(aes(x = tb, y = mc, group = iso3, color = mkt_gain)) +
  geom_line(size = 0.2) +
  geom_hline(data = rlm_trading_prices, aes(yintercept = trading_price), linetype = "dashed") +
  facet_grid(realm ~ approach, scales = "free_y") +
  scale_color_brewer(palette = "Set1", direction = -1) +
  # lims(y = c(-1000, 100000 * 5), x = c(0, 4)) +
  ggtheme_plot() +
  labs(x = "Biodiversity",
       y = "Costs",
       caption = "NOTE: Axis have been cropped for visualization purposes") +
  guides(color = FALSE)

map_contrasting_scenarios <- ggplot() +
  geom_sf(data = filter(eez_with_results,!variable == "gets_paid", str_detect(realm, "Atlantic")),
          aes(fill = value), color = "black") +
  geom_sf(data = coast, color = "black") +
  facet_grid(label ~ realm) +
  scale_fill_viridis_c(na.value = "gray") +
  ggtheme_map() +
  guides(fill = guide_colorbar(title = "Total costs\nmillion USD",
                               frame.colour = "black",
                               ticks.colour = "black"))

map_of_trade <- eez %>% 
  left_join(got_paid, by = c("iso3", "realm")) %>% 
  filter(variable == "gets_paid",
         str_detect(realm, "Atlantic")) %>% 
  mutate(gets_paid = ifelse(value == 1, "Got paid", "Pays")) %>% 
  ggplot() +
  geom_sf(aes(fill = gets_paid), color = "black") +
  geom_sf(data = coast, color = "black") +
  scale_fill_brewer(palette = "Set1", direction = -1, na.value = "gray") +
  ggtheme_map() +
  facet_wrap(~realm) +
  guides(fill = guide_legend(title = "Transaction")) +
  theme(legend.position = "bottom")

# Plot the two states of the world
two_states_map <-
  rlm_eez_cb %>% 
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
            filename = "equilibrum_supply_curves_rlm",
            width = 12,
            height = 6)

lazy_ggsave(plot = two_states_map,
            filename = "two_states_map_rlm",
            width = 10,
            height = 10)

lazy_ggsave(plot = map_contrasting_scenarios,
            filename = "map_contrasting_scenarios_rlm",
            width = 20,
            height = 20)

lazy_ggsave(plot = map_of_trade,
            filename = "map_of_trade_rlm",
            width = 20,
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
         market = "realm",
         segments = 12L) %>% 
  saveRDS(file = file.path(project_path, "output_data", "gains_from_trade_rlm.rds"))

gains_from_trade %>% 
  mutate(variable = case_when(variable == "area" ~ "Area",
                              variable == "tb" ~ "Biodiversity",
                              variable == "tc" ~ "Costs")) %>% 
  knitr::kable(format = "latex",
               digits = 2, 
               col.names = c("Realm", "Variable", "BAU", "Market", "Difference", "Ratio"),
               label = "rlm-gains-from-trade",
               caption = "Gains from trade from protecting 73.65 units of biodiversity. Difference shows BAU - Market, ratio shows Market / BAU.") %>% 
  cat(file = here::here("results", "tab", "gains_from_trade_rlm.tex"))



















