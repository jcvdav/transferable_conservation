################################################################################
# title
################################################################################
# 
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Calculates market gains across a range of protection targets and bubble policies
#
################################################################################

## SET UP ######################################################################
# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  tidyverse
)

# Source directory -------------------------------------------------------------
source_dir <- here("results", "processed_data", "supply_curves", "no_mpas")


# Load data --------------------------------------------------------------------
eez_cb <- readRDS(file = here(source_dir, "global_eez_supply_curves_with_no_mpas.rds"))
eez_h_sum_cb  <- readRDS(file = here(source_dir, "global_supply_curve_with_no_mpas.rds"))

hem_eez_cb <- readRDS(file = here(source_dir, "hemisphere_eez_supply_curves_with_no_mpas.rds"))
hem_h_sum <- readRDS(file = here(source_dir, "hemisphere_supply_curves_with_no_mpas.rds"))

rlm_eez_cb <- readRDS(file = here(source_dir, "realm_eez_supply_curves_with_no_mpas.rds"))
rlm_h_sum <- readRDS(file = here(source_dir, "realm_supply_curves_with_no_mpas.rds"))

pro_eez_cb <- readRDS(file = here(source_dir, "province_eez_supply_curves_with_no_mpas.rds"))
pro_h_sum <- readRDS(file = here(source_dir, "province_supply_curves_with_no_mpas.rds"))

eco_eez_cb <- readRDS(file = here(source_dir, "ecoregion_eez_supply_curves_with_no_mpas.rds"))
eco_h_sum <- readRDS(file = here(source_dir, "ecoregion_supply_curves_with_no_mpas.rds"))


# Define a function that estiamtes gains from trade ----------------------------
get_segmented_market_gains <- 
  function(r, curves, agg_curves, group) {
    # browser()
    
    ## SET UP ##################################################################
    # Get conserving nations
    conserving_nations <- curves %>%
      select(iso3, {{group}}) %>%
      distinct()
    
    # Identify the trading price and fraction of the marginal pixel if needed
    mkt_protected_pixels <- agg_curves %>%
      group_by_at(group) %>%
      mutate(pixel_fraction = pmin(1 - ((tb - ((tb * r) / pct)) / benefit), 1)) %>%
      filter(pixel_fraction >= 0) %>%
      select({{group}}, lon, lat, trading_price = mc, pixel_fraction, pct) %>% 
      ungroup()
    
    trading_prices <- mkt_protected_pixels %>% 
      filter(pixel_fraction < 1 | pct == 1) %>%  # Remove pixels before the nth pixel, whose fraction is 1. Unless it's the last pixel in the world for r = 1, then we keep it
      select({{group}},
             trading_price,
             pixel_fraction)
    
    if(dim(trading_prices)[1] > dim(agg_curves %>% select({{group}}) %>% distinct())[1]) {
      warning("Some segments have more than one trading price")
    }
    
    ## GET MARKET OUTCOMES #####################################################
    
    # Now we calculate the realized outcomes 
    # Note that these estimates of the total benefits do not include the total benefit of what
    # has already been protected by MPAs that are already in place. This is because the benefit of those MPAs
    # is the same for BAU and MKT scenarios. Thus, these are the costs of additional conservation needed
    # to ensure that each nation protects 30% (or more, for some that have already surpassed) under BAU
    # or that 30% of the world is protected.
    
    # BAU realizations
    realized_bau_cb <- curves %>%
      mutate(pixel_fraction = pmin(1 - ((tb - ((tb * r) / pct)) / benefit), 1)) %>% # Calculate the fraction of the pixel that needs to be protected
      filter(pixel_fraction >= 0) %>%
      mutate(adj_benefit = benefit * pixel_fraction,
             adj_cost = cost * pixel_fraction,
             adj_area = area * pixel_fraction) %>%
      group_by_at(c("iso3", group)) %>%
      summarize(
        bau_tb = sum(adj_benefit, na.rm = T),
        bau_tc = sum(adj_cost, na.rm = T),
        bau_area = sum(adj_area, na.rm = T),
        mc_stop = max(mc, na.rm = T),
        .groups = "drop_last"
      ) %>%
      ungroup()
    
    realized_mkt_cb <- curves %>%
      inner_join(mkt_protected_pixels %>%
                   select(-pct),
                 by = c(group, "lat", "lon")) %>%                               # Keep all patches in each country with a marginal cost <= trading price, as identified before
      group_by_at(group) %>%
      mutate(adj_benefit = benefit * pixel_fraction,
             adj_cost = cost * pixel_fraction,
             adj_area = area * pixel_fraction) %>%
      group_by_at(c("iso3", group)) %>%
      summarize(
        mkt_tb = sum(adj_benefit, na.rm = T),
        mkt_tc = sum(adj_cost, na.rm = T),
        mkt_area = sum(adj_area, na.rm = T),
        .groups = "drop_last"
      ) %>%
      ungroup()
    
    # # Market realizations
    # realized_mkt_cb <- curves %>%
    #   left_join(trading_prices, by = group) %>%
    #   filter(mc <= trading_price) %>%                                           # Keep all patches in each country with a marginal cost <= trading price
    #   group_by_at(group) %>%
    #   mutate(pixel_fraction = ifelse(mc < max(mc),                              # If the mc of a patch is below market clearing price then (see line below)
    #                                  1,                                         # the entire patch is protected, otherwise (see line below)
    #                                  pixel_fraction)) %>%                       # a fraciton of it is protected
    #   ungroup() %>%
    #   mutate(adj_benefit = benefit * pixel_fraction,
    #          adj_cost = cost * pixel_fraction,
    #          adj_area = area * pixel_fraction) %>%
    #   group_by_at(c("iso3", group, "trading_price")) %>%
    #   summarize(
    #     mkt_tb = sum(adj_benefit, na.rm = T),
    #     mkt_tc = sum(adj_cost, na.rm = T),
    #     mkt_area = sum(adj_area, na.rm = T),
    #     .groups = "drop_last"
    #   ) %>%
    #   ungroup()
    
    # Create a data.frame with the combined summarized outcomes
    combined_outcomes <- conserving_nations %>%
      left_join(realized_mkt_cb, by = c("iso3", group)) %>%
      left_join(realized_bau_cb, by = c("iso3", group)) %>%
      # Replace na's to account for countries that have already met the target and whose values are not accounted for yet
      replace_na(replace = list(mkt_tb = 0,
                                mkt_tc = 0,
                                bau_tb = 0,
                                bau_tc = 0,
                                mkt_area = 0,
                                bau_area = 0)) %>%
      select(-contains("app")) %>%
      left_join(trading_prices, by = group) %>%
      mutate(rect = abs(bau_tb - mkt_tb) * trading_price,
             mkt_tc_b = bau_tc - mkt_tc - rect,
             mkt_tc_s = rect - mkt_tc + bau_tc,
             savings = ifelse(mkt_tb < bau_tb, mkt_tc_b, mkt_tc_s),
             transaction = case_when(near(savings, 0) ~ "Doesn't participate",
                                     mkt_tc < bau_tc ~ "Buyers",
                                     mkt_tc > bau_tc ~ "Sellers"))
    
    # Build summarized table of outcomes to return
    gains_from_trade <- combined_outcomes %>%
      select(bau_area, mkt_area, bau_tb, mkt_tb, bau_tc, difference = savings) %>%
      summarize_all(sum, na.rm = T) %>%
      mutate(ratio = difference / bau_tc,
             bubble = group)
    
    return(gains_from_trade)
  }

## PROCESSING ##################################################################

# Define range of targets ------------------------------------------------------
rs <- (10:100)/100

# Simulate all global ----------------------------------------------------------
gains_from_trade_multiple_scenarios_global <- tibble(r = rs) %>%
  mutate(
    data = map(
      r,
      get_segmented_market_gains,
      curves = eez_cb,
      agg_curves = eez_h_sum_cb,
      group = "global"
    )
  ) %>%
  unnest(data)

# Simulate all hemisphere ------------------------------------------------------
gains_from_trade_multiple_scenarios_hem <-
  tibble(r = rs) %>%
  mutate(
    data = map(
      r,
      get_segmented_market_gains,
      curves = hem_eez_cb,
      agg_curves = hem_h_sum,
      group = "hemisphere"
    )
  ) %>%
  unnest(data)

# Simulate all realm -----------------------------------------------------------
gains_from_trade_multiple_scenarios_rlm <-
  tibble(r = rs) %>%
  mutate(
    data = map(
      r,
      get_segmented_market_gains,
      curves = rlm_eez_cb,
      agg_curves = rlm_h_sum,
      group = "realm"
    )
  ) %>%
  unnest(data)

# Simulate all province --------------------------------------------------------
gains_from_trade_multiple_scenarios_pro <-
  tibble(r = rs) %>%
  mutate(
    data = map(
      r,
      get_segmented_market_gains,
      curves = pro_eez_cb,
      agg_curves = pro_h_sum,
      group = "province"
    )
  ) %>%
  unnest(data)

# Simulate all ecoregion --------------------------------------------------------
gains_from_trade_multiple_scenarios_eco <-
  tibble(r = rs) %>%
  mutate(
    data = map(
      r,
      get_segmented_market_gains,
      curves = eco_eez_cb,
      agg_curves = eco_h_sum,
      group = "ecoregion"
    )
  ) %>%
  unnest(data)

# Combine results --------------------------------------------------------------
gains_from_trade_multiple_scenarios <- rbind(gains_from_trade_multiple_scenarios_global,
                                             gains_from_trade_multiple_scenarios_hem,
                                             gains_from_trade_multiple_scenarios_rlm,
                                             gains_from_trade_multiple_scenarios_pro,
                                             gains_from_trade_multiple_scenarios_eco) %>%
  group_by(bubble, r) %>%
  summarise_all(sum, na.rm = T) %>%
  ungroup() %>%
  mutate(
    bubble = stringr::str_to_sentence(bubble),
    bubble = case_when(
      bubble == "Global" ~ "Global (N = 1)",
      bubble == "Hemisphere" ~ "Hemisphere (N = 4)",
      bubble == "Realm" ~ "Realm (N = 12)",
      bubble == "Province" ~ "Province (N = 60)",
      bubble == "Ecoregion" ~ "Ecoregion (N = 219)"
    )
  )

## EXPORT RESULTS ##############################################################

saveRDS(
  object = gains_from_trade_multiple_scenarios,
  file = here("results", "output_data", "gains_from_trade_bubbles_no_mpas.rds")
)
