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
library(here)
library(cowplot)
library(tidyverse)

export_files <- F#T

# Load data --------------------------------------------------------------------
eez_cb <-
  readRDS(
    file = here(
      "results",
      "processed_data",
      "supply_curves",
      "with_mpas",
      "global_eez_supply_curves_with_mpas_abt.rds"
    )
  )

eez_h_sum_cb <-
  readRDS(
    file = here(
      "results",
      "processed_data",
      "supply_curves",
      "with_mpas",
      "global_supply_curve_with_mpas_abt.rds"
    )
  )

hem_eez_cb <-
  readRDS(
    file = here(
      "results",
      "processed_data",
      "supply_curves",
      "with_mpas",
      "hemisphere_eez_supply_curves_with_mpas_abt.rds"
    )
  )
hem_h_sum <-
  readRDS(
    file = here(
      "results",
      "processed_data",
      "supply_curves",
      "with_mpas",
      "hemisphere_supply_curves_with_mpas_abt.rds"
    )
  )

rlm_eez_cb <-
  readRDS(
    file = here(
      "results",
      "processed_data",
      "supply_curves",
      "with_mpas",
      "realm_eez_supply_curves_with_mpas_abt.rds"
    )
  )
rlm_h_sum <-
  readRDS(
    file = here(
      "results",
      "processed_data",
      "supply_curves",
      "with_mpas",
      "realm_supply_curves_with_mpas_abt.rds"
    )
  )


# Province
pro_eez_cb <- readRDS(
  file = here(
    "results",
    "processed_data",
    "supply_curves",
    "with_mpas",
    "province_eez_supply_curves_with_mpas_abt.rds"
  )
)
pro_h_sum <- readRDS(
  file = here(
    "results",
    "processed_data",
    "supply_curves",
    "with_mpas",
    "province_supply_curves_with_mpas_abt.rds"
  )
)

# Ecoregion
eco_eez_cb <- readRDS(
  file = here(
    "results",
    "processed_data",
    "supply_curves",
    "with_mpas",
    "ecoregion_eez_supply_curves_with_mpas_abt.rds"
  )
)
eco_h_sum <- readRDS(
  file = here(
    "results",
    "processed_data",
    "supply_curves",
    "with_mpas",
    "ecoregion_supply_curves_with_mpas_abt.rds"
  )
)


# Define a function that estiamtes gains from trade ----------------------------
get_segmented_market_gains <-
  function(r, curves, agg_curves, group, write = F) {
    # browser()
    # Get conserving nations
    conserving_nations <- curves %>%
      select(iso3, {
        {
          group
        }
      }) %>%
      distinct()
    
    already_reached <- curves %>%
      filter(pct_protected >= r) %>%
      select(iso3, {
        {
          group
        }
      }, pct_protected) %>%
      distinct()
    
    trading_prices <- agg_curves %>%
      group_by_at(group) %>%
      mutate(pixel_fraction = pmin(1 - ((tb - ((tb * r) / pct
      )) / benefit), 1)) %>%
      filter(pixel_fraction >= 0) %>%
      slice_max(mc) %>%
      ungroup() %>%
      select({
        {
          group
        }
      }, trading_price = mc)
    
    
    realized_bau_cb <- curves %>%
      mutate(pixel_fraction = pmin(1 - ((tb - ((tb * r) / pct
      )) / benefit), 1)) %>%
      filter(pixel_fraction >= 0) %>%
      mutate(benefit = benefit * pixel_fraction,
             cost = cost * pixel_fraction) %>%
      group_by_at(c("iso3", group)) %>%
      summarize(
        bau_tb = sum(benefit, na.rm = T),
        bau_tc = sum(cost, na.rm = T),
        bau_area = sum(area, na.rm = T),
        mc_stop = max(mc, na.rm = T),
        .groups = "drop_last"
      ) %>%
      ungroup()
    
    # For a market
    realized_mkt_cb <- curves %>%
      left_join(trading_prices, by = group) %>%
      filter(mc <= trading_price) %>%               # Keep all patches in each country with a cost < trading price
      group_by_at(group) %>%
      mutate(pixel_fraction = ifelse(mc < max(mc), 1, 1 - ((tb - ((tb * r) / pct
      )) / benefit))) %>%
      ungroup() %>%
      mutate(benefit = pixel_fraction * benefit,
             cost = pixel_fraction * cost) %>%
      group_by_at(c("iso3", group, "trading_price")) %>%
      summarize(
        mkt_tb = sum(benefit, na.rm = T),
        mkt_tc = sum(cost, na.rm = T),
        mkt_area = sum(area, na.rm = T),
        .groups = "drop_last"
      ) %>%
      ungroup()
    
    # Create a data.frame with the combined summarized outcomes
    combined_outcomes <- conserving_nations %>%
      left_join(realized_mkt_cb, by = c("iso3", group)) %>%
      left_join(realized_bau_cb, by = c("iso3", group)) %>%
      left_join(already_reached, by = c("iso3", group)) %>%
      replace_na(replace = list(
        mkt_tb = 0,
        mkt_tc = 0,
        bau_tb = 0,
        bau_tc = 0
      )) %>%
      select(-contains("app"),-trading_price) %>%
      left_join(trading_prices, by = group) %>%
      mutate(
        rect = abs(bau_tb - mkt_tb) * trading_price,
        mkt_tc_b = bau_tc - mkt_tc - rect,
        mkt_tc_s = rect - mkt_tc + bau_tc,
        savings = ifelse(mkt_tb < bau_tb, mkt_tc_b, mkt_tc_s),
        transaction = case_when(
          !is.na(pct_protected) | near(savings, 0) ~ "Doesn't participate",
          mkt_tc < bau_tc ~ "Buyers",
          mkt_tc > bau_tc ~ "Sellers"
        )
      )
    
    if (write) {
      r <- formatC(format = "f",
                   x = r,
                   digits = 2)
      write_csv(
        x = combined_outcomes,
        file = here(
          "results",
          "output_data",
          "trade_outcomes",
          group,
          paste0("r_", r, "_iso3_outcomes.csv")
        )
      )
    }
    
    
    gains_from_trade <- combined_outcomes %>%
      select(bau_tb, mkt_tb, bau_tc, difference = savings) %>%
      summarize_all(sum, na.rm = T) %>%
      mutate(ratio = difference / bau_tc,
             bubble = group)
    
    return(gains_from_trade)
  }

## PROCESSING ##################################################################

# Define range of targets ------------------------------------------------------
rs <- seq(0.1, 1, by = 0.01)

# Simulate all global ----------------------------------------------------------
gains_from_trade_multiple_scenarios_global <- tibble(r = rs) %>%
  mutate(
    data = map(
      r,
      get_segmented_market_gains,
      curves = eez_cb,
      agg_curves = eez_h_sum_cb,
      group = "global",
      write = export_files
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
      group = "hemisphere",
      write = export_files
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
      group = "realm",
      write = export_files
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
      group = "province",
      write = export_files
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
      group = "ecoregion",
      write = export_files
    )
  ) %>%
  unnest(data)

# Combine results --------------------------------------------------------------
gains_from_trade_multiple_scenarios <- rbind(
  gains_from_trade_multiple_scenarios_global,
  gains_from_trade_multiple_scenarios_hem,
  gains_from_trade_multiple_scenarios_rlm,
  gains_from_trade_multiple_scenarios_pro,
  gains_from_trade_multiple_scenarios_eco
) %>%
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
  file = here("results", "output_data", "gains_from_trade_bubbles_abt.rds")
)
