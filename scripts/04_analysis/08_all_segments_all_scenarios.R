#######################################################
#
#######################################################



# SET UP ################################################################################################
# Load packages
library(startR)
library(cowplot)
library(tidyverse)

# Load data
eez_cb <- readRDS(file = file.path(project_path, "processed_data", "eez_costs_and_benefits.rds")) %>% 
  mutate(type = "efficient") %>% 
  select(iso3, everything())
eez_h_sum_cb <- readRDS(file = file.path(project_path, "processed_data", "eez_h_sum_costs_and_benefits.rds")) %>% 
  mutate(type = "efficient") %>% 
  select(iso3, everything())

hem_eez_cb <- readRDS(
  file = file.path( project_path, "processed_data", "hem_eez_costs_and_benefits.rds")
) 
hem_h_sum <- readRDS(
  file = file.path( project_path, "processed_data", "hem_h_sum_costs_and_benefits.rds")
) 

rlm_eez_cb <- readRDS(
  file = file.path( project_path, "processed_data", "rlm_eez_costs_and_benefits.rds")
) 
rlm_h_sum <- readRDS(
  file = file.path( project_path, "processed_data", "rlm_h_sum_costs_and_benefits.rds")
) 

pro_eez_cb <- readRDS(
  file = file.path( project_path, "processed_data", "pro_eez_costs_and_benefits.rds")
) 
### Horizontally summed for each province
pro_h_sum <- readRDS(
  file = file.path( project_path, "processed_data", "pro_h_sum_costs_and_benefits.rds")
) 


get_market_gains <- function(eez_cb, r, trading_price) {
  bau <- eez_cb %>% 
    filter(pct <= r) %>%                       # Keep the most efficient 30% of each country - hemisphere
    mutate(approach = "bau") %>% 
    group_by(approach, iso3) %>% 
    summarize(bau_tb = sum(benefit, na.rm = T),
              bau_tc = sum(cost, na.rm = T),
              mc_stop = max(mc, na.rm = T),
              bau_area = n(),
              .groups = "drop_last") %>% 
    ungroup()
  
  mkt <- eez_cb %>% 
    filter(mc <= trading_price) %>%              # Keep all patches in each country with a cost < trading price
    mutate(approach = "mkt") %>% 
    group_by(approach, iso3) %>% 
    summarize(mkt_tb = sum(benefit, na.rm = T),
              mkt_tc = sum(cost, na.rm = T),
              mkt_area = n(),
              .groups = "drop_last") %>% 
    ungroup()
  
  
  gains_from_trade <- full_join(mkt, bau, by = c("iso3"))  %>% 
    select(contains("mkt"), contains("bau")) %>% 
    select(-contains("stop")) %>% 
    summarize_all(sum, na.rm = T) %>% 
    ungroup() %>% 
    gather(variable, value) %>% 
    separate(variable, into = c("approach", "variable")) %>% 
    spread(approach, value) %>% 
    filter(variable == "tc") 
  
  return(gains_from_trade)
}

#################
# Gains from trade hemisphere function

get_market_gains_hem <- function(curves, r, trading_prices, group) {
  
  # Filter to keep only the protected places
  bau <- curves %>% 
    filter(pct <= r) %>%                       # Keep the most efficient 30% of each country - hemisphere
    mutate(approach = "bau")
  
  mkt <- curves %>% 
    left_join(trading_prices, by = c(group)) %>%
    filter(mc <= trading_price) %>%              # Keep all patches in each country with a cost < trading price
    mutate(approach = "mkt") %>% 
    select(-c(trading_price))
  
  ## Calculate country-level summaries
  # For BAU
  realized_bau_cb <- bau %>% 
    group_by_at(all_of(c("approach", "iso3", group))) %>% 
    summarize(bau_tb = sum(benefit, na.rm = T),
              bau_tc = sum(cost, na.rm = T),
              mc_stop = max(mc, na.rm = T),
              bau_area = n(),
              .groups = "drop_last") %>% 
    ungroup()
  
  # For a market
  realized_mkt_cb <- mkt %>% 
    group_by_at(all_of(c("approach", "iso3", group))) %>% 
    summarize(mkt_tb = sum(benefit, na.rm = T),
              mkt_tc = sum(cost, na.rm = T),
              mkt_area = n(),
              .groups = "drop_last") %>% 
    ungroup()
  
  gains_from_trade <- full_join(realized_mkt_cb, realized_bau_cb, by = c("iso3", group))  %>% 
    select(contains("mkt"), contains("bau")) %>% 
    select(-contains("stop")) %>% 
    summarize_all(sum, na.rm = T) %>% 
    gather(variable, value) %>% 
    separate(variable, into = c("approach", "variable")) %>% 
    spread(approach, value) %>% 
    filter(variable == "tc")
  
  return(gains_from_trade)
}

benefit_wrapper <- function(r, group, curves) {
  curves %>%
    group_by_at(.vars = all_of(group)) %>%
    nest() %>%
    mutate(target = map_dbl(data, benefit, r)) %>%
    select(-data)
}

trading_price_wrapper <- function(targets, group, agg_curves) {
  agg_curves %>% 
    group_by_at(all_of(group)) %>% 
    nest() %>% 
    left_join(targets, by = group) %>% 
    mutate(trading_price = map2_dbl(.x = target, .y = data, get_trading_price)) %>% 
    select(-data)
}

#################
# Market segmenter

market_segmenter <- function(rs, curves, agg_curves, group){
  tibble(rs = rs) %>% 
    mutate(targets = map(rs, benefit_wrapper, curves = curves, group = group)) %>% 
    mutate(trading_prices = map(targets, trading_price_wrapper, agg_curves = agg_curves, group = group)) %>% 
    mutate(costs = map2(rs, trading_prices, get_market_gains_hem, curves = curves, group = group)) %>% 
    unnest(costs) %>% 
    mutate(difference = bau - mkt,
           ratio = mkt / bau,
           market = group) %>% 
    mutate(targets = map_dbl(targets, function(x){sum(x$target)}))
}

#################

rs <- seq(0.05, 1, by = c(0.01))

gains_from_trade_multiple_scenarios_global <- tibble(rs = rs) %>% 
  mutate(targets = map_dbl(rs, benefit, data = eez_cb)) %>% 
  mutate(trading_prices = map_dbl(targets, get_trading_price, supply_curve = eez_h_sum_cb)) %>% 
  mutate(costs = map2(rs, trading_prices, get_market_gains, eez_cb = eez_cb)) %>% 
  unnest(costs) %>% 
  mutate(difference = bau - mkt,
         ratio = mkt / bau,
         market = "global")

gains_from_trade_multiple_scenarios_hem <- 
  market_segmenter(
    rs = rs,
    curves = hem_eez_cb,
    agg_curves = hem_h_sum,
    group = "hemisphere"
  )

gains_from_trade_multiple_scenarios_rlm <- 
  market_segmenter(
    rs = rs,
    curves = rlm_eez_cb,
    agg_curves = rlm_h_sum,
    group = "realm"
  )

gains_from_trade_multiple_scenarios_pro <- 
  market_segmenter(
    rs = rs,
    curves = pro_eez_cb,
    agg_curves = pro_h_sum,
    group = "province"
  )


gains_from_trade_multiple_scenarios <- rbind(
  gains_from_trade_multiple_scenarios_global,
  gains_from_trade_multiple_scenarios_hem,
  gains_from_trade_multiple_scenarios_rlm,
  gains_from_trade_multiple_scenarios_pro
  )

# Visualize 
abs <- ggplot(gains_from_trade_multiple_scenarios, aes(x = targets, y = difference, color = market)) +
  geom_line(size = 1) +
  ggtheme_plot() +
  labs(x = "Biodiversity", 
       y = "Costs avoided (BAU - MKT)",
       color = "Segment") +
  # scale_x_continuous(labels = scales::percent) +
  scale_color_brewer(palette = "Set1") +
  theme(legend.justification = c(0, 1),
        legend.position = c(0, 1))

rel <- ggplot(gains_from_trade_multiple_scenarios, aes(x = targets, y = 1 - ratio, color = market)) +
  geom_line(size = 1) +
  ggtheme_plot() +
  labs(x = "Biodiversity", 
       y = "Costs avoided (difference / BAU)") +
  # scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position  = "None")

gain_from_trade_segmented_market_plot <- plot_grid(abs, rel, ncol = 2)

lazy_ggsave(plot = gain_from_trade_segmented_market_plot,
            file = "gain_from_trade_segmented_market_plot",
            width = 20,
            height = 7.5)






