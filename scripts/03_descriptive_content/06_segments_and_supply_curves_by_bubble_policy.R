################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Description
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  tidyverse
)

# Load data --------------------------------------------------------------------
master_cb <- readRDS(file = here("results/processed_data/master_costs_and_benefits.rds"))

## PROCESSING ##################################################################

# Get number of segments -------------------------------------------------------
n_hems <- length(unique(master_cb$hemisphere))
n_rlm <- length(unique(master_cb$realm))
n_pro <- length(unique(master_cb$province))
n_eco <- length(unique(master_cb$ecoregion))

# Get numner of supply curves --------------------------------------------------
n_eez <- master_cb %>% 
  pull(iso3) %>% 
  n_distinct()

n_eez_hem <- master_cb %>% 
  mutate(eez_hem = paste0(iso3, hemisphere)) %>% 
  pull(eez_hem) %>% 
  n_distinct()

n_eez_rlm <- master_cb %>% 
  mutate(eez_rlm = paste0(iso3, realm)) %>% 
  pull(eez_rlm) %>% 
  n_distinct()

n_eez_pro <- master_cb %>% 
  mutate(eez_pro = paste0(iso3, province)) %>% 
  pull(eez_pro) %>% 
  n_distinct()

n_eez_eco <- master_cb %>% 
  mutate(eez_eco = paste0(iso3, ecoregion)) %>% 
  pull(eez_eco) %>% 
  n_distinct()

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
tibble(Bubble = c("Global", "Hemisphere", "Realm", "Province", "Ecoregion"),
       N_segments = c(1, n_hems, n_rlm, n_pro, n_eco),
       N_supply_curves = c(n_eez, n_eez_hem, n_eez_rlm, n_eez_pro, n_eez_eco)) %>% 
  knitr::kable(linesep = "",
               booktabs = T,
               format = "latex",
               col.names = c("Bubble policy", "# of Segments", "# of supply curves"))
