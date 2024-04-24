######################################################
#title#
######################################################
#
# This build supply curves assuming As don'te xist
#
######################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  tidyverse
)

# Export dir -------------------------------------------------------------------
export_dir <- here("results", "processed_data", "supply_curves", "no_mpas")

# Load data --------------------------------------------------------------------
master_data <- readRDS(
  file = here(
    "results",
    "processed_data",
    "master_costs_and_benefits.rds"
  ))

## BUILD CURVES ################################################################

# Function to build curves
build_curve_with_no_mpas <- function(data, by = NULL) {
  curves <- data %>%
    select({{by}}, lon, lat, suitability, cost, area, mpa, benefit, bcr, mc) %>%
    group_by_at(vars(all_of(by))) %>%
    arrange(desc(bcr)) %>%
    mutate(
      tb = cumsum(benefit),
      tc = cumsum(cost),
      pct = tb / sum(benefit)
    ) %>%
    ungroup()
  
  return(curves)
}

# Build global -----------------------------------------------------------------
eez_supply_curves_wo_mpas <-
  build_curve_with_no_mpas(master_data, c("iso3", "global"))
global_supply_curve_wo_mpas <-
  build_curve_with_no_mpas(master_data, "global")

# Build hemisphere -------------------------------------------------------------
hemisphere_eez_supply_curves_wo_mpas <-
  build_curve_with_no_mpas(master_data, c("iso3", "hemisphere"))
hemisphere_supply_curves_wo_mpas <-
  build_curve_with_no_mpas(master_data, by = "hemisphere")

# Build realm ------------------------------------------------------------------
realm_eez_supply_curves_wo_mpas <-
  build_curve_with_no_mpas(master_data, c("iso3", "realm"))
realm_supply_curves_wo_mpas <-
  build_curve_with_no_mpas(master_data, by = "realm")

# Build province ---------------------------------------------------------------
province_eez_supply_curves_wo_mpas <-
  build_curve_with_no_mpas(master_data, c("iso3", "province"))
province_supply_curves_wo_mpas <-
  build_curve_with_no_mpas(master_data, by = "province")

# Build ecoregion --------------------------------------------------------------
ecoregion_eez_supply_curves_wo_mpas <-
  build_curve_with_no_mpas(master_data, c("iso3", "ecoregion"))
ecoregion_supply_curves_wo_mpas <-
  build_curve_with_no_mpas(master_data, by = "ecoregion")


## DATA EXPORT #################################################################
# Export country-level data ----------------------------------------------------
saveRDS(object = eez_supply_curves_wo_mpas,
        file = here(export_dir, "global_eez_supply_curves_with_no_mpas.rds"))
# Export horizontally summed
saveRDS(object = global_supply_curve_wo_mpas,
        file = here(export_dir, "global_supply_curve_with_no_mpas.rds"))

# Export hemisphere-level data -------------------------------------------------
saveRDS(object = hemisphere_eez_supply_curves_wo_mpas,
        file = here(export_dir, "hemisphere_eez_supply_curves_with_no_mpas.rds"))
# Export horizontally summed hemisphere data
saveRDS(object = hemisphere_supply_curves_wo_mpas,
        file = here(export_dir, "hemisphere_supply_curves_with_no_mpas.rds"))

# Export realm level data ------------------------------------------------------
saveRDS(object = realm_eez_supply_curves_wo_mpas,
        file = here(export_dir, "realm_eez_supply_curves_with_no_mpas.rds"))
# Export horizontally summed realms
saveRDS(object = realm_supply_curves_wo_mpas,
        file = here(export_dir, "realm_supply_curves_with_no_mpas.rds"))

# Export province level data ---------------------------------------------------
saveRDS(object = province_eez_supply_curves_wo_mpas,
        file = here(export_dir, "province_eez_supply_curves_with_no_mpas.rds"))
# Export horizontally summed province level data
saveRDS(object = province_supply_curves_wo_mpas,
        file = here(export_dir, "province_supply_curves_with_no_mpas.rds"))

# Export ecoregion level data --------------------------------------------------
saveRDS(object = ecoregion_eez_supply_curves_wo_mpas,
        file = here(export_dir, "ecoregion_eez_supply_curves_with_no_mpas.rds"))
# Export horizontally summed province level data
saveRDS(object = ecoregion_supply_curves_wo_mpas,
        file = here(export_dir, "ecoregion_supply_curves_with_no_mpas.rds"))
