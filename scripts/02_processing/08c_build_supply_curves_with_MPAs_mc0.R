################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Builds conservation upply curves under different bubble policies, accounting
# for the presence of MPAs. Here, we assume that existing MPAs would be immediaely
# inscribed into the BAU or MKT scenarios, and then the remaining conservation
# follows. This version of the supply curves ensures that existing MPAs
# are the first to be protected, and that TB_bau and TB_mkt are always the same.
# 
# The main point is to sort pixels in descending order in marginal benefits
# (highest first), and calculates the horizontally summed supply curve for
# conservation.
#
# The process is then repeated at different levels of spatial hierarchy:
# - eez
# - eez and hemisphere
# - eez and realm
# - eez and province
# - eez and ecoregion

#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  tidyverse
)

# Export dir -------------------------------------------------------------------
export_dir <- here("results", "processed_data", "supply_curves", "with_mpas_mc0")


# Load data --------------------------------------------------------------------
master_data <- readRDS(file = here("results",
                                   "processed_data",
                                   "master_costs_and_benefits.rds")) %>% 
  replace_na(replace = list(mpa = 0)) %>%
  mutate(cost = ifelse(mpa == 1, 0, cost),
         mc = cost / benefit,
         bcr = benefit / cost) 

# Define a function that builds the supply curves ------------------------------
build_curve_with_mpas <- function(data, by = NULL) {
  # browser()
  curves <- data %>%
    select({
      {
        by
      }
    }, lon, lat, suitability, cost, area, mpa, benefit, bcr, mc) %>%
    group_by_at(vars(all_of(by))) %>%
    arrange(desc(bcr), desc(benefit)) %>%
    mutate(
      tb = cumsum(benefit),
      tc = cumsum(cost),
      pct = tb / sum(benefit)
    ) %>%
    ungroup()
  
  return(curves)
}

# Build global -----------------------------------------------------------------
eez_supply_curves_w_mpas <-
  build_curve_with_mpas(master_data, c("iso3", "global"))
global_supply_curve_w_mpas <-
  build_curve_with_mpas(master_data, "global")

# Build hemisphere -------------------------------------------------------------
hemisphere_eez_supply_curves_w_mpas <-
  build_curve_with_mpas(master_data, c("iso3", "hemisphere"))
hemisphere_supply_curves_w_mpas <-
  build_curve_with_mpas(master_data, by = "hemisphere")

# Build realm ------------------------------------------------------------------
realm_eez_supply_curves_w_mpas <-
  build_curve_with_mpas(master_data, c("iso3", "realm"))
realm_supply_curves_w_mpas <-
  build_curve_with_mpas(master_data, by = "realm")

# Build province ---------------------------------------------------------------
province_eez_supply_curves_w_mpas <-
  build_curve_with_mpas(master_data, c("iso3", "province"))
province_supply_curves_w_mpas <-
  build_curve_with_mpas(master_data, by = "province")

# Build ecoregion --------------------------------------------------------------
ecoregion_eez_supply_curves_w_mpas <-
  build_curve_with_mpas(master_data, c("iso3", "ecoregion"))
ecoregion_supply_curves_w_mpas <-
  build_curve_with_mpas(master_data, by = "ecoregion")


## DATA EXPORT #################################################################
# Export country-level data ----------------------------------------------------
saveRDS(object = eez_supply_curves_w_mpas,
        file = here(export_dir, "global_eez_supply_curves_with_mpas_mc0.rds"))
# Export horizontally summed
saveRDS(object = global_supply_curve_w_mpas,
        file = here(export_dir, "global_supply_curve_with_mpas_mc0.rds"))

# Export hemisphere-level data -------------------------------------------------
saveRDS(object = hemisphere_eez_supply_curves_w_mpas,
        file = here(export_dir, "hemisphere_eez_supply_curves_with_mpas_mc0.rds"))
# Export horizontally summed hemisphere data
saveRDS(object = hemisphere_supply_curves_w_mpas,
        file = here(export_dir, "hemisphere_supply_curves_with_mpas_mc0.rds"))

# Export realm level data ------------------------------------------------------
saveRDS(object = realm_eez_supply_curves_w_mpas,
        file = here(export_dir, "realm_eez_supply_curves_with_mpas_mc0.rds"))
# Export horizontally summed realms
saveRDS(object = realm_supply_curves_w_mpas,
        file = here(export_dir, "realm_supply_curves_with_mpas_mc0.rds"))

# Export province level data ---------------------------------------------------
saveRDS(object = province_eez_supply_curves_w_mpas,
        file = here(export_dir, "province_eez_supply_curves_with_mpas_mc0.rds"))
# Export horizontally summed province level data
saveRDS(object = province_supply_curves_w_mpas,
        file = here(export_dir, "province_supply_curves_with_mpas_mc0.rds"))

# Export ecoregion level data --------------------------------------------------
saveRDS(object = ecoregion_eez_supply_curves_w_mpas,
        file = here(export_dir, "ecoregion_eez_supply_curves_with_mpas_mc0.rds"))
# Export horizontally summed province level data
saveRDS(object = ecoregion_supply_curves_w_mpas,
        file = here(export_dir, "ecoregion_supply_curves_with_mpas_mc0.rds"))

## END OF SCRIPT ###############################################################
