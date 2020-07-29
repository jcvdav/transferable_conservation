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

### For each realm and country
rlm_eez_cb <- readRDS(
  file = file.path( project_path, "processed_data", "rlm_eez_costs_and_benefits.rds")
) 
### Horizontally summed for each realm
rlm_h_sum <- readRDS(
  file = file.path( project_path, "processed_data", "rlm_h_sum_costs_and_benefits.rds")
) 

## Read trading prices
trading_prices <- readRDS(
  file = file.path(project_path, "output_data", "global_trading_prices.rds")
)

# Load a coastline
coast <- ne_countries(returnclass = "sf") %>% 
  st_transform(crs = epsg_moll) 

# PROCESSING ############################################################################################

# Get targets
rlm_conservation_target <- rlm_eez_cb %>%
  group_by(realm) %>%
  nest() %>%
  mutate(target = map_dbl(data, benefit, 0.3)) %>%
  select(-data)

rlm_trading_prices <- rlm_h_sum %>% 
  group_by(realm) %>% 
  nest() %>% 
  left_join(rlm_conservation_target, by = "realm") %>% 
  mutate(trading_price = map2_dbl(.x = target, .y = data, get_trading_price)) %>% 
  select(-data)
  
## Identify conserved patches
# Get the most efficient trading price

# Filter to keep only the protected places
bau <- rlm_eez_cb %>% 
  filter(pct <= 0.3) %>%                       # Keep the most efficient 30% of each country - realm
  mutate(approach = "bau")

mkt <- rlm_eez_cb %>% 
  left_join(rlm_trading_prices, by = c("realm")) %>%
  filter(mc <= trading_price) %>%              # Keep all patches in each country with a cost < trading price
  mutate(approach = "mkt") %>% 
  select(-c(trading_price,))

## Calculate country-level summaries
# For BAU
realized_bau_cb <- bau %>% 
  group_by(approach, iso3, realm) %>% 
  summarize(bau_tb = sum(benefit, na.rm = T),
            bau_tc = sum(cost, na.rm = T),
            mc_stop = max(mc, na.rm = T),
            bau_area = n()) %>% 
  ungroup()

# For a market
realized_mkt_cb <- mkt %>% 
  group_by(approach, iso3, realm) %>% 
  summarize(mkt_tb = sum(benefit, na.rm = T),
            mkt_tc = sum(cost, na.rm = T),
            mkt_area = n()) %>% 
  ungroup()

# Find stopping prices
stops <- full_join(realized_mkt_cb, realized_bau_cb, by = c("iso3", "realm")) %>% 
  select(-contains("app")) %>% 
  filter(bau_tb <= mkt_tb) %>% 
  select(iso3, mc_stop, realm) %>% 
  mutate(approach = "mkt")

gains_from_trade <- full_join(realized_mkt_cb, realized_bau_cb, by = "iso3")  %>% 
  select_if(is.numeric) %>% 
  select(-contains("stop")) %>% 
  summarize_all(sum, na.rm = T) %>% 
  gather(variable, value) %>% 
  separate(variable, into = c("approach", "variable")) %>% 
  spread(approach, value) %>% 
  mutate(difference = bau - mkt,
         ratio = mkt / bau)

## FIGURES #########################################################################
## Plot the supply curves where they stop
benefit_supply_curves <- rbind(bau, mkt) %>%
  left_join(stops, by = c("iso3", "realm", "approach"), fill = list(mc_stop = 0)) %>% 
  mutate(mkt_gain = mc >= mc_stop,
         approach = ifelse(approach == "bau", "BAU", "Market")) %>% 
  replace_na(replace = list(mkt_gain = F)) %>% 
  ggplot(aes(x = tb, y = mc, group = iso3, color = mkt_gain)) +
  geom_line(size = 0.2) +
  geom_hline(data = rlm_trading_prices, aes(yintercept = trading_price), linetype = "dashed") +
  facet_grid(realm ~ approach, scales = "free_y") +
  scale_color_brewer(palette = "Set1", direction = -1) +
  # lims(y = c(-1000, 1000 * 5), x = c(0, 200)) +
  ggtheme_plot() +
  labs(x = "Biodiversity",
       y = "Costs",
       caption = "NOTE: Axis have been cropped for visualization purposes") +
  guides(color = FALSE)

got_paid <- full_join(realized_mkt_cb, realized_bau_cb, by = "iso3") %>% 
  select(-contains("app")) %>% 
  mutate(gets_paid = bau_tc <= mkt_tc)

map_of_trade <- coast %>% 
  left_join(got_paid, by = c("iso_a3" = "iso3")) %>% 
  replace_na(replace = list(gets_paid = FALSE)) %>% 
  ggplot() +
  geom_sf(aes(fill = gets_paid), color = "black") +
  scale_fill_brewer(palette = "Set1", direction = -1) +
  ggtheme_map() +
  guides(fill = FALSE)

# Plot the two states of the world
two_states_map <- 
  rbind(bau, mkt) %>% 
  ggplot() +
  geom_sf(data = coast) +
  geom_raster(aes(x = lon, y = lat, fill = benefit)) +
  facet_wrap(~approach, ncol = 1) +
  ggtheme_map() +
  scale_fill_viridis_c() +
  guides(fill = guide_colorbar(title = "Biodiversity",
                               frame.colour = "black",
                               ticks.colour = "black")) +
  labs(caption = "Both conservation strategies yield the same benefits,\nbut a market approach results in 1t4% of the costs")


## EXPORT FIGURES #########################################################################

lazy_ggsave(plot = benefit_supply_curves,
            filename = "equilibrum_supply_curves",
            width = 12,
            height = 6)

lazy_ggsave(plot = two_states_map,
            filename = "two_states_map",
            width = 10,
            height = 10)

lazy_ggsave(plot = map_of_trade,
            filename = "map_of_trade",
            width = 10,
            height = 5)

gains_from_trade %>% 
  mutate(variable = c("Area", "Biodiversity benefits", "Total costs")) %>% 
  knitr::kable(format = "latex",
               digits = 2, 
               col.names = c("Variable", "BAU", "Market", "Difference", "Ratio"),
               label = "gains-from-trade",
               caption = "Gains from trade from protecting 9,687 units of biodiversity. Difference shows BAU - Market, ratio shows Market / BAU.") %>% 
  cat(file = here::here("results", "tab", "gains_from_trade.tex"))
























