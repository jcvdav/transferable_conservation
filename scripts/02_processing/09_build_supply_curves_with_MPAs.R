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
# browser()
    achievements <- data %>% 
    group_by_at(vars(all_of(by))) %>% 
    summarize(protected = sum(benefit * mpa, na.rm = T),
              pct_protected = protected / sum(benefit)) %>% 
    ungroup()
  
  curves <- data %>% 
    filter(is.na(mpa)) %>% 
    select({{by}}, lon, lat, suitability, cost, area, mpa, benefit, bcr, mc) %>%
    left_join(achievements, by = by) %>%
    group_by_at(vars(all_of(by))) %>%
    arrange(desc(bcr)) %>%
    mutate(
      tb = protected + cumsum(benefit),
      tc = cumsum(cost),
      pct = tb / (sum(benefit) + protected)
    ) %>%
    ungroup()
  
  return(curves)
}


eez_supply_curves_w_mpas <- build_curve_with_mpas(master_data, c("iso3", "global"))
global_supply_curve_w_mpas <- build_curve_with_mpas(master_data, "global")

hemisphere_eez_supply_curves_w_mpas <- build_curve_with_mpas(master_data, c("iso3", "hemisphere"))
hemisphere_supply_curves_w_mpas <- build_curve_with_mpas(master_data, by = "hemisphere")

realm_eez_supply_curves_w_mpas <- build_curve_with_mpas(master_data, c("iso3", "realm"))
realm_supply_curves_w_mpas <- build_curve_with_mpas(master_data, by = "realm")

province_eez_supply_curves_w_mpas <- build_curve_with_mpas(master_data, c("iso3", "province"))
province_supply_curves_w_mpas <- build_curve_with_mpas(master_data, by = "province")



