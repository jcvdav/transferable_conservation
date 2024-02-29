################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# Oct 8, 2022
#
# Calculates outcomes for a 30% target and a global market
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
library(here)
library(tidyverse)

## PROCESSING ##################################################################

# Identify pixels protected under a 30% target and unilateral conservation -----
bau <-
  readRDS(
    file = here(
      "results",
      "processed_data",
      "supply_curves",
      "with_mpas",
      "global_eez_supply_curves_with_mpas.rds"
    )
  ) %>%
  mutate(pixel_fraction = pmin(1 - ((tb - ((tb * 0.3) / pct
  )) / benefit), 1)) %>%
  filter(pixel_fraction >= 0) %>%
  select(lat, lon) %>%
  mutate(bau = 1)

# Identify pixels protected under a 30% target and trade -----------------------
mkt <-
  readRDS(
    file = here(
      "results",
      "processed_data",
      "supply_curves",
      "with_mpas",
      "global_supply_curve_with_mpas.rds"
    )
  ) %>%
  mutate(pixel_fraction = pmin(1 - ((tb - ((tb * 0.3) / pct
  )) / benefit), 1)) %>%
  filter(pixel_fraction >= 0) %>%
  select(lat, lon) %>%
  mutate(mkt = 1)

# Load all the values ----------------------------------------------------------
base <- readRDS(
  file = here(
    "results",
    "processed_data",
    "supply_curves",
    "with_mpas",
    "global_eez_supply_curves_with_mpas_abt.rds"
  )
) %>%
  select(lat, lon, suitability, area, cost, benefit) %>%
  left_join(bau, by = c("lat", "lon")) %>%                                      # Mark pixel as protected under BAU
  left_join(mkt, by = c("lat", "lon")) %>%                                      # Mark pixel as protected under MKT
  replace_na(list("bau" = 0,
                  "mkt" = 0)) %>%
  mutate(
    both = mkt + bau,
    protected = case_when(
      mkt == 1 & bau == 1 ~ "Protected under both",
      mkt == 0 &
        bau == 1 ~ "Protected only under BAU",
      mkt == 1 &
        bau == 0 ~ "Protected only under MKT",
      T ~ "Not protected"
    )
  ) %>%
  mutate(cost = log10(cost)) %>%
  filter(!protected == "Not protected")

# Export #######################################################################

saveRDS(
  object = base,
  file = here(
    "results",
    "output_data",
    "bau_and_mkt_otucomes_global_30.rds"
  )
)
