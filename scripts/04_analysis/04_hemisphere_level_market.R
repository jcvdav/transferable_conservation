####################################################
# Hemisphere segments
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
### For each hemisphere and country
hem_eez_cb <- readRDS(
  file = file.path( project_path, "processed_data", "hem_eez_costs_and_benefits.rds")
) 
### Horizontally summed for each hemisphere
hem_h_sum <- readRDS(
  file = file.path( project_path, "processed_data", "hem_h_sum_costs_and_benefits.rds")
) 

# Load a coastline
coast <- ne_countries(returnclass = "sf") %>% 
  st_transform(crs = epsg_moll) 

# Load EEZ geopackage
eez <- st_read(
  file.path(project_path,
            "processed_data",
            "clean_world_eez_v11.gpkg")
) %>% 
  mutate(has_obligations = iso3 %in% unique(hem_eez_cb$iso3)) %>% 
  rmapshaper::ms_simplify(keep_shapes = T, sys = T) %>% 
  st_make_valid() %>% 
  filter(iso3 %in% unique(hem_eez_cb$iso3))

# PROCESSING ############################################################################################

# Get targets
hem_conservation_target <- hem_eez_cb %>%
  group_by(hemisphere) %>%
  nest() %>%
  mutate(target = map_dbl(data, benefit, 0.3)) %>%
  select(-data)

hem_trading_prices <- hem_h_sum %>% 
  group_by(hemisphere) %>% 
  nest() %>% 
  left_join(hem_conservation_target, by = "hemisphere") %>% 
  mutate(trading_price = map2_dbl(.x = target, .y = data, get_trading_price)) %>% 
  select(-data)

## Set up a data.frame of nation ISO3s that participate
conserving_nations <- hem_eez_cb %>% 
  select(iso3, hemisphere) %>% 
  distinct()

## Identify conserved patches
# Get the most efficient trading price

# Filter to keep only the protected places
bau <- hem_eez_cb %>% 
  group_by(iso3, hemisphere) %>% 
  mutate(min_pct = min(pct, na.rm = T)) %>% 
  ungroup() %>% 
  filter(pct <= 0.3 | pct <= min_pct) %>%                       # Keep the most efficient 30% of each country - hemisphere
  mutate(approach = "bau")

mkt <- hem_eez_cb %>% 
  left_join(hem_trading_prices, by = c("hemisphere")) %>%
  filter(mc <= trading_price) %>%              # Keep all patches in each country with a cost < trading price
  mutate(approach = "mkt")

## Calculate country-level summaries
# For BAU
realized_bau_cb <- bau %>% 
  group_by(approach, iso3, hemisphere) %>% 
  summarize(bau_tb = sum(benefit, na.rm = T),
            bau_tc = sum(cost, na.rm = T),
            mc_stop = max(mc, na.rm = T),
            bau_area = n() * 2500) %>% 
  ungroup()

# For a market
realized_mkt_cb <- mkt %>% 
  group_by(approach, iso3, hemisphere, trading_price) %>% 
  summarize(mkt_tb = sum(benefit, na.rm = T),
            mkt_tc = sum(cost, na.rm = T),
            mkt_area = n() * 2500) %>% 
  ungroup()

# Find stopping prices

# Create a data.frame with the combined summarized outcomes
combined_outcomes <- conserving_nations %>% 
  left_join(realized_mkt_cb, by = c("iso3", "hemisphere")) %>%
  left_join(realized_bau_cb, by = c("iso3", "hemisphere")) %>% 
  replace_na(replace = list(
    mkt_tb = 0, mkt_tc = 0, mkt_area = 0,
    bau_tb = 0, bau_tc = 0, bau_area = 0)) %>% 
  select(-contains("app")) %>% 
  mutate(mkt_tc2 = mkt_tc + ((bau_tb - mkt_tb) * trading_price),
         mkt_tc = ifelse(mkt_tc2 <=0, bau_tc, mkt_tc2)) %>% 
  select(-mkt_tc2)

stops <- full_join(realized_mkt_cb, realized_bau_cb, by = c("iso3", "hemisphere")) %>% 
  select(-contains("app")) %>% 
  filter(bau_tb <= mkt_tb) %>% 
  select(iso3, mc_stop, hemisphere) %>% 
  mutate(approach = "mkt")

gains_from_trade <- combined_outcomes %>% 
  # select_if(is.numeric) %>% 
  select(-contains("stop"), -contains("iso3")) %>% 
  group_by(hemisphere) %>% 
  summarize_all(sum, na.rm = T) %>% 
  gather(variable, value, -hemisphere) %>% 
  separate(variable, into = c("approach", "variable")) %>% 
  spread(approach, value) %>% 
  mutate(difference = bau - mkt,
         ratio = mkt / bau)

## FIGURES #########################################################################
## Plot the supply curves where they stop
benefit_supply_curves <- rbind(bau, mkt %>% select(-target)) %>%
  left_join(stops, by = c("iso3", "hemisphere", "approach"), fill = list(mc_stop = 0)) %>% 
  mutate(mkt_gain = mc >= mc_stop,
         approach = ifelse(approach == "bau", "BAU", "Market")) %>% 
  replace_na(replace = list(mkt_gain = F)) %>% 
  ggplot(aes(x = tb, y = mc, group = iso3, color = mkt_gain)) +
  geom_line(size = 0.2) +
  geom_hline(data = hem_trading_prices, aes(yintercept = trading_price), linetype = "dashed") +
  facet_grid(hemisphere ~ approach, scales = "free_y") +
  scale_color_brewer(palette = "Set1", direction = -1) +
  lims(y = c(-1000, 100000 * 5), x = c(0, 5)) +
  ggtheme_plot() +
  labs(x = "Biodiversity",
       y = "Costs",
       caption = "NOTE: Axis have been cropped for visualization purposes") +
  guides(color = FALSE)

got_paid <- full_join(realized_mkt_cb, realized_bau_cb, by = c("hemisphere", "iso3")) %>% 
  select(-contains("app")) %>% 
  mutate(gets_paid = bau_tc <= mkt_tc)

got_paid <- combined_outcomes %>% 
  mutate(gets_paid = mc_stop <= trading_price,
         mkt_gains = bau_tc - mkt_tc) %>% 
  select(iso3, hemisphere, gets_paid, bau_tc, mkt_tc, mkt_gains) %>% 
  pivot_longer(cols = c(gets_paid, bau_tc, mkt_tc, mkt_gains), names_to = "variable", values_to = "value") %>% 
  mutate(label = case_when(variable == "bau_tc" ~ "A) Business-as-usual",
                           variable == "mkt_tc" ~ "B) Market-based (global)",
                           variable == "mkt_gains" ~ "C) Gains from trade",
                           T ~ NA_character_))

eez_with_results <- eez %>% 
  left_join(got_paid, by = c("iso3", "hemisphere")) #NEEED TO ADD HEMISPHER ESEGMENTATION TO EEZ


# FIGURES

map_of_trade <- coast %>% 
  left_join(got_paid, by = c("iso_a3" = "iso3")) %>% 
  drop_na(hemisphere) %>%
  replace_na(replace = list(gets_paid = FALSE)) %>% 
  ggplot() +
  geom_sf(data = coast) +
  geom_sf(aes(fill = gets_paid), color = "black", size = 0.1) +
  facet_wrap(~hemisphere, ncol = 2) +
  scale_fill_brewer(palette = "Set1", direction = -1) +
  ggtheme_map() +
  guides(fill = FALSE)

# Plot the two states of the world
two_states_map <-
  rbind(bau, mkt %>% select(-target)) %>%
  ggplot() +
  geom_sf(data = coast) +
  geom_raster(aes(x = lon, y = lat, fill = benefit)) +
  facet_wrap(~approach, ncol = 1) +
  ggtheme_map() +
  scale_fill_viridis_c() +
  guides(fill = guide_colorbar(title = "Biodiversity",
                               frame.colour = "black",
                               ticks.colour = "black")) +
  labs(caption = "Both conservation strategies yield the same benefits,\nbut a market approach costs 44% less.")


## EXPORT FIGURES #########################################################################

lazy_ggsave(plot = benefit_supply_curves,
            filename = "equilibrum_supply_curves_hem",
            width = 12,
            height = 6)

lazy_ggsave(plot = two_states_map,
            filename = "two_states_map_hem",
            width = 10,
            height = 10)

lazy_ggsave(plot = map_of_trade,
            filename = "map_of_trade_hem",
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
         market = "hemisphere",
         segments = 4L) %>% 
  saveRDS(file = file.path(project_path, "output_data", "gains_from_trade_hem.rds"))


gains_from_trade %>% 
  mutate(variable = case_when(variable == "area" ~ "Area",
                              variable == "tb" ~ "Biodiversity",
                              variable == "tc" ~ "Costs")) %>% 
  knitr::kable(format = "latex",
               digits = 2, 
               col.names = c("hemisphere", "Variable", "BAU", "Market", "Difference", "Ratio"),
               label = "pro-gains-from-trade",
               caption = "Gains from trade from protecting 73.65 units of biodiversity. Difference shows BAU - Market, ratio shows Market / BAU.") %>% 
  cat(file = here::here("results", "tab", "gains_from_trade_hem.tex"))

# Table of trading prices for each hemisphere
totals <- hem_trading_prices %>% 
  mutate(hemisphere = "Summary") %>% 
  group_by(hemisphere) %>% 
  summarize(trading_price = weighted.mean(trading_price, target),
            target = sum(target)) %>% 
  select(hemisphere, target, trading_price)

hem_trading_prices %>% 
  rbind(totals) %>% 
  knitr::kable(format = "latex",
               digits = 2,
               col.names = c("hemisphere", "Biodiversity", "Trading Price"),
               label = "pro-trading-prices",
               caption = "Biodiversity targets and trading prices for 12 hemispheres. The Last row shows total biodiversity and weighted mean of trading price") %>% 
  cat(file = here::here("results", "tab", "trading_prices_hem.tex"))



















