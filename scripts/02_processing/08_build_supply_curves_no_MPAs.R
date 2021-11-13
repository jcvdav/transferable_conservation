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

## BUILD CURVES ################################################################

# Function to build curves
build_curve_without_mpas <- function(data, by = NULL) {
  curves <- data %>%
    select({{by}}, lon, lat, suitability, cost, area, benefit, bcr, mc) %>%
    group_by_at(vars(one_of(by))) %>%
    arrange(desc(bcr)) %>%
    mutate(
      tb = cumsum(benefit),
      tc = cumsum(cost),
      pct = tb / sum(benefit)
    ) %>%
    ungroup()
  
  return(curves)
}

# Global trading market
eez_supply_curves <- build_curve_without_mpas(master_data, "iso3")
global_supply_curve <- build_curve_without_mpas(eez_supply_curves)

# Hemisphere markets
hemisphere_eez_supply_curves <-
  build_curve_without_mpas(master_data, c("iso3", "hemisphere"))
hemisphere_supply_curves <-
  build_curve_without_mpas(hemisphere_eez_supply_curves, "hemisphere")

# Realm markets
realm_eez_supply_curves <-
  build_curve_without_mpas(master_data, c("iso3", "realm"))
realm_supply_curves <- build_curve_without_mpas(realm_eez_supply_curves, "realm")

# Province markets
province_eez_supply_curves <-
  build_curve_without_mpas(master_data, c("iso3", "province"))
province_supply_curves <-
  build_curve_without_mpas(province_eez_supply_curves, "province")




## DATA EXPORT #################################################################
# Export country-level data
saveRDS(
  eez_supply_curves,
  file = file.path(
    project_path,
    "processed_data",
    "supply_curves",
    "no_mpas",
    "global_eez_supply_curves_no_mpas.rds"
  )
)

# Export horizontally summed
saveRDS(
  global_supply_curve,
  file = file.path(
    project_path,
    "processed_data",
    "supply_curves",
    "no_mpas",
    "global_supply_curve_no_mpas.rds"
  )
)

# Export hemisphere-level data
saveRDS(
  hemisphere_eez_supply_curves,
  file = file.path(
    project_path,
    "processed_data",
    "supply_curves",
    "no_mpas",
    "hemisphere_eez_supply_curves_no_mpas.rds"
  )
)

# Export horizontally summed hemisphere data
saveRDS(
  hemisphere_supply_curves,
  file = file.path(
    project_path,
    "processed_data",
    "supply_curves",
    "no_mpas",
    "hemisphere_supply_curves_no_mpas.rds"
  )
)

# Export realm level data
saveRDS(
  realm_eez_supply_curves,
  file = file.path(
    project_path,
    "processed_data",
    "supply_curves",
    "no_mpas",
    "realm_eez_supply_curves_no_mpas.rds"
  )
)

# Export horizontally summed realms
saveRDS(
  realm_supply_curves,
  file = file.path(
    project_path,
    "processed_data",
    "supply_curves",
    "no_mpas",
    "realm_supply_curves_no_mpas.rds"
  )
)


# Export province level data
saveRDS(
  province_eez_supply_curves,
  file = file.path(
    project_path,
    "processed_data",
    "supply_curves",
    "no_mpas",
    "province_eez_supply_curves_no_mpas.rds"
  )
)

# Export horizontally summed province level data
saveRDS(
  province_supply_curves,
  file = file.path(
    project_path,
    "processed_data",
    "supply_curves",
    "no_mpas",
    "province_supply_curves_no_mpas.rds"
  )
)
