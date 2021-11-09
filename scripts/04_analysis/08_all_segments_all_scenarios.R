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

hem_eez_cb <- readRDS(file = file.path( project_path, "processed_data", "hem_eez_costs_and_benefits.rds")) 
hem_h_sum <- readRDS(file = file.path( project_path, "processed_data", "hem_h_sum_costs_and_benefits.rds")) 

rlm_eez_cb <- readRDS(file = file.path( project_path, "processed_data", "rlm_eez_costs_and_benefits.rds")) 
rlm_h_sum <- readRDS(file = file.path( project_path, "processed_data", "rlm_h_sum_costs_and_benefits.rds")) 

pro_eez_cb <- readRDS(file = file.path( project_path, "processed_data", "pro_eez_costs_and_benefits.rds")) 
pro_h_sum <- readRDS(file = file.path( project_path, "processed_data", "pro_h_sum_costs_and_benefits.rds"))


get_global_market_gains <- function(curves, agg_curves, r) {
  
  # browser()
  trading_price <- agg_curves %>% 
    filter(pct <= r) %>% 
    pull(mc) %>% 
    max()
  
  conserving_nations <- curves %>% 
    select(iso3) %>% 
    distinct()
  
  bau <- curves %>% 
    group_by(iso3) %>% 
    mutate(min_pct = min(pct, na.rm = T)) %>% 
    ungroup() %>% 
    filter(pct <= r | pct <= min_pct) %>% # Keep the most efficient r% of each country - hemisphere
    mutate(approach = "bau") %>% 
    group_by(approach, iso3) %>% 
    summarize(bau_tb = sum(benefit, na.rm = T),
              bau_tc = sum(cost, na.rm = T),
              mc_stop = max(mc, na.rm = T),
              bau_area = n() * 2500,
              .groups = "drop_last") %>% 
    ungroup()
  
  mkt <- curves %>% 
    filter(mc <= trading_price) %>%              # Keep all patches in each country with a cost < trading price
    mutate(approach = "mkt") %>% 
    group_by(approach, iso3) %>% 
    summarize(mkt_tb = sum(benefit, na.rm = T),
              mkt_tc = sum(cost, na.rm = T),
              mkt_area = n() * 2500,
              .groups = "drop_last") %>% 
    ungroup()
  
  combined_outcomes <- conserving_nations %>% 
    left_join(mkt, by = "iso3") %>%
    left_join(bau, by = "iso3") %>% 
    replace_na(replace = list(
      mkt_tb = 0, mkt_tc = 0, mkt_area = 0,
      bau_tb = 0, bau_tc = 0, bau_area = 0)) %>% 
    select(-contains("app")) %>% 
    mutate(rect = abs(bau_tb - mkt_tb) * trading_price,
           mkt_tc_b = bau_tc - mkt_tc - rect,
           mkt_tc_s = rect - mkt_tc + bau_tc,
           savings = ifelse(mkt_tb < bau_tb, mkt_tc_b, mkt_tc_s))
  
  gains_from_trade <- combined_outcomes %>% 
    select(bau_tc, difference = savings) %>% 
    summarize_all(sum, na.rm = T) %>% 
    mutate(ratio = difference / bau_tc,
           bubble = "global")
  
  return(gains_from_trade)
}

#################
# Gains from trade hemisphere function

get_segmented_market_gains <- function(r, curves, agg_curves, group) {
  # browser()
  
  # Get conserving nations
  conserving_nations <- curves %>% 
    select(iso3, {{group}}) %>% 
    distinct()
  
  trading_prices <- agg_curves %>% 
    group_by_at(group) %>% 
    filter(pct <= r) %>% 
    slice_max(mc) %>% 
    ungroup() %>% 
    select({{group}}, trading_price = mc)
  
  # Filter to keep only the protected places
  bau <- curves %>% 
    group_by_at(c("iso3", group)) %>% 
    mutate(min_pct = min(pct, na.rm = T)) %>% 
    ungroup() %>% 
    filter(pct <= r | pct <= min_pct) %>%                       # Keep the most efficient 30% of each country - hemisphere
    mutate(approach = "bau")
  
  mkt <- curves %>% 
    left_join(trading_prices, by = group) %>%
    filter(mc <= trading_price) %>%              # Keep all patches in each country with a cost < trading price
    mutate(approach = "mkt")
  
  ## Calculate country-level summaries
  # For BAU
  realized_bau_cb <- bau %>% 
    group_by_at(c("approach", "iso3", group)) %>% 
    summarize(bau_tb = sum(benefit, na.rm = T),
              bau_tc = sum(cost, na.rm = T),
              mc_stop = max(mc, na.rm = T),
              bau_area = n() * 2500,
              .groups = "drop_last") %>% 
    ungroup()
  
  # For a market
  realized_mkt_cb <- mkt %>% 
    group_by_at(c("approach", "iso3", group, "trading_price")) %>% 
    summarize(mkt_tb = sum(benefit, na.rm = T),
              mkt_tc = sum(cost, na.rm = T),
              mkt_area = n() * 2500,
              .groups = "drop_last") %>% 
    ungroup()
  
  # Create a data.frame with the combined summarized outcomes
  combined_outcomes <- conserving_nations %>% 
    left_join(realized_mkt_cb, by = c("iso3", group)) %>%
    left_join(realized_bau_cb, by = c("iso3", group)) %>% 
    replace_na(replace = list(
      mkt_tb = 0, mkt_tc = 0, mkt_area = 0,
      bau_tb = 0, bau_tc = 0, bau_area = 0)) %>% 
    select(-contains("app"), -trading_price) %>% 
    left_join(trading_prices, by = group) %>% 
    mutate(rect = abs(bau_tb - mkt_tb) * trading_price,
           mkt_tc_b = bau_tc - mkt_tc - rect,
           mkt_tc_s = rect - mkt_tc + bau_tc,
           savings = ifelse(mkt_tb < bau_tb, mkt_tc_b, mkt_tc_s))
  
  gains_from_trade <- combined_outcomes %>% 
    select(bau_tc, difference = savings) %>% 
    summarize_all(sum, na.rm = T) %>% 
    mutate(ratio = difference / bau_tc,
           bubble = group)
  
  return(gains_from_trade)
}

#################

rs <- seq(0.01, 1, by = 0.01)

gains_from_trade_multiple_scenarios_global <- tibble(r = rs) %>% 
  mutate(data = map(r,
                    get_global_market_gains,
                    curves = eez_cb,
                    agg_curves = eez_h_sum_cb)) %>% 
  unnest(data)

gains_from_trade_multiple_scenarios_hem <- 
  tibble(r = rs) %>% 
  mutate(data = map(r,
                    get_segmented_market_gains,
                    curves = hem_eez_cb,
                    agg_curves = hem_h_sum,
                    group = "hemisphere")) %>% 
  unnest(data)


gains_from_trade_multiple_scenarios_rlm <- 
  tibble(r = rs) %>% 
  mutate(data = map(r,
                    get_segmented_market_gains,
                    curves = rlm_eez_cb,
                    agg_curves = rlm_h_sum,
                    group = "realm")) %>% 
    unnest(data)

gains_from_trade_multiple_scenarios_pro <- 
  tibble(r = rs) %>% 
  mutate(data = map(r,
                    get_segmented_market_gains,
                    curves = pro_eez_cb,
                    agg_curves = pro_h_sum,
                    group = "province")) %>% 
  unnest(data)

gains_from_trade_multiple_scenarios <- rbind(
  gains_from_trade_multiple_scenarios_global,
  gains_from_trade_multiple_scenarios_hem,
  gains_from_trade_multiple_scenarios_rlm,
  gains_from_trade_multiple_scenarios_pro) %>% 
  mutate(bubble = stringr::str_to_sentence(bubble),
         bubble = case_when(bubble == "Global" ~ "Global (N = 1)",
                            bubble == "Hemisphere" ~ "Hemisphere (N = 4)",
                            bubble == "Realm" ~ "Realm (N = 12)",
                            bubble == "Province" ~ "Province (N = 60)"),
         bubble = fct_relevel(bubble, "Province (N = 60)", after = Inf))

# Visualize 
abs <- ggplot(gains_from_trade_multiple_scenarios, aes(x = r, y = difference, color = bubble)) +
  geom_line(size = 1) +
  ggtheme_plot() +
  labs(x = bquote("% Benefits ("~HS[i]*A[i]~")"), 
       y = "Costs avoided (BAU - MKT)",
       color = "Segment") +
  scale_x_continuous(labels = scales::percent) +
  scale_color_brewer(palette = "Set1") +
  theme(legend.justification = c(0, 1),
        legend.position = c(0, 1)) +
  geom_vline(xintercept = 0.3, linetype = "dashed")

rel <- ggplot(gains_from_trade_multiple_scenarios, aes(x = r, y = ratio, color = bubble)) +
  geom_line(size = 1) +
  ggtheme_plot() +
  labs(x = bquote("% Benefits ("~HS[i]*A[i]~")"), 
       y = "Costs savings (difference / BAU)") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position  = "None") +
  geom_vline(xintercept = 0.3, linetype = "dashed")

gain_from_trade_segmented_market_plot <- plot_grid(abs, rel, ncol = 2, labels = "AUTO")

lazy_ggsave(plot = gain_from_trade_segmented_market_plot,
            file = "gain_from_trade_segmented_market_plot",
            width = 20,
            height = 7.5)

write.csv(x = gains_from_trade_multiple_scenarios,
          file = file.path(project_path, "output_data", "gains_from_trade_bubbles.csv"),
          row.names = F)






