######################################################
#title#
######################################################
#
# Purpose
#
######################################################

## SET UP ######################################################################
# Load packages
library(tidyverse)

# Load data
master_data <- readRDS(file = file.path(
  project_path,
  "processed_data",
  "master_costs_and_benefits.rds"
))

build_curve_with_mpas <- function(data, by = NULL) {

    achievements <- data %>% 
    group_by_at(vars(all_of(by))) %>% 
    summarize(protected = sum(benefit * mpa, na.rm = T),
              pct_proteced = protected / sum(benefit)) %>% 
    ungroup()
  
  curves <- data %>% 
    filter(is.na(mpa)) %>% 
    select({{by}}, lon, lat, suitability, cost, area, mpa, benefit, bcr, mc) %>%
    left_join(achievements, by = by) %>%
    group_by_at(vars(one_of(by))) %>%
    arrange(desc(bcr)) %>%
    mutate(
      tb = protected + cumsum(benefit),
      tc = cumsum(cost),
      pct = tb / (sum(benefit) + protected)
    ) %>%
    ungroup()
  
  return(curves)
}

sum_horizontally <- function(supply_curves, by = NULL){
  supply_curves %>% 
    filter(is.na(mpa)) %>% 
    group_by_at(vars(all_of(by))) %>% 
    arrange(desc(bcr)) %>%
    mutate(
      tb = protected + cumsum(benefit),
      tc = cumsum(cost),
      pct = tb / (sum(benefit) + protected)
    ) %>%
    ungroup()
}

eez_supply_curves_w_mpas <- build_curve_with_mpas(master_data, "iso3")
global_supply_curve_w_mpas <- sum_horizontally(eez_supply_curves_w_mpas)

hemisphere_eez_supply_curves_w_mpas <- build_curve_with_mpas(master_data, c("iso3", "hemisphere"))
hemisphere_supply_curves_w_mpas <- sum_horizontally(hemisphere_eez_supply_curves_w_mpas, by = "hemisphere")

realm_eez_supply_curves_w_mpas <- build_curve_with_mpas(master_data, c("iso3", "realm"))
realm_supply_curves_w_mpas <- sum_horizontally(realm_eez_supply_curves_w_mpas, by = "realm")

province_eez_supply_curves_w_mpas <- build_curve_with_mpas(master_data, c("iso3", "province"))
province_supply_curves_w_mpas <- sum_horizontally(province_eez_supply_curves_w_mpas, by = "province")



