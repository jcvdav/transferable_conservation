################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# Modified on: May 30, 2024
# 
#
################################################################################
#                                                                              #
#                                                                              #
# >>>>>>>>>>>>>>>>      IT IS IMPERATIVE THAT YOU READ THIS      <<<<<<<<<<<<<<<
#                                                                              #
#                                                                              #
################################################################################
#
#
#
#
#
# The repository contains many different files. The purpose of this script is to
# provide the minimum necesary information for QUASI-replication of our findings
# and figures. If you want an exact replication, take a look at the script called
# "scripts/04_analysis/02_all_segments_all_scenarios.R". This script is provided
# just as a minimum reproducible example. The script was also produced AFTER the
# paper had been accepted, and no output of this script is used in our work.
#
# The script proceeds as follows:
# STEP 1 - Two packages are loaded: here (for path management) and the tidyverse
# STEP 2 -  A source directory is indicated
# STEP 3 -  Load supply curves. Each "bubble policy" has two sets of them.
#   One set is the bubble-eez level supply curve (e.g.: Mexico-by-realm)
#   The other set is the aggregate bubble-level suppl curve (e.g. by realm).
# STEP 4 - A function is defined. See further details there, but all it does is
#   filter each set of supply curves based on a target. It then sums across the
#   pixel-level  costs to get the total costs under BAU and under MKT. It then
#   calculates the relative gains as (BAU - MKT) / BAU.
# STEP 5 - The function is called 455 times: 5 bubble policies X 91 conservation
#   targets (from 10% to 100% in 1% increments)
# STEP 6 - Results are combined into a data-frame and two figures are produced:
#   - A bar chart showing gains form trade under 30 by 30 for 5 bubble policies
#   - A line chart showing gains from trade for different targets and policies
#
#
#
#
#
################################################################################

## SET UP ######################################################################

# STEP1: Load packages ---------------------------------------------------------
pacman::p_load(
  here,
  tidyverse
)

# STEP 2: Create a pointer to a source directory
source_dir <- here("results", "processed_data", "supply_curves", "with_mpas")

# Step3: Load supply curves ----------------------------------------------------
eez_cb <- readRDS(file = here(source_dir, "global_eez_supply_curves_with_mpas.rds"))
eez_h_sum_cb  <- readRDS(file = here(source_dir, "global_supply_curve_with_mpas.rds"))

hem_eez_cb <- readRDS(file = here(source_dir, "hemisphere_eez_supply_curves_with_mpas.rds"))
hem_h_sum <- readRDS(file = here(source_dir, "hemisphere_supply_curves_with_mpas.rds"))

rlm_eez_cb <- readRDS(file = here(source_dir, "realm_eez_supply_curves_with_mpas.rds"))
rlm_h_sum <- readRDS(file = here(source_dir, "realm_supply_curves_with_mpas.rds"))

pro_eez_cb <- readRDS(file = here(source_dir, "province_eez_supply_curves_with_mpas.rds"))
pro_h_sum <- readRDS(file = here(source_dir, "province_supply_curves_with_mpas.rds"))

eco_eez_cb <- readRDS(file = here(source_dir, "ecoregion_eez_supply_curves_with_mpas.rds"))
eco_h_sum <- readRDS(file = here(source_dir, "ecoregion_supply_curves_with_mpas.rds"))

## PROCESSING ##################################################################

# STEP 4: Define a gft function --------------------------------------------------------
# gft = "Gains From Trade". This is a quick and dirty function to get the gains from trade,
# without paying attention to who pays / gets paid, and fractional pixels. The 
# process is simple and involves two filters:
# - Under BAU, all nations protect along their individual supply curves up to the target.
#   The costs is the total cost is the area under the curve
# - Under MKT, protection is done along the aggregate supply curve.
gft <- function(tar, group = "global", ind = eez_cb, agg = eez_h_sum_cb) {
  # BAU
  bau <- ind %>% # Uses the eez-by-bubble policy supply curves
    group_by_at(group) %>% 
    filter(pct <= tar) %>%# Under BAU, all nations protect up to the target
    pull(cost) %>% 
    sum()
  
  mkt <- agg %>% # Uses the aggregate supply curves
    group_by_at(group) %>% 
    filter(pct <= tar) %>% # Under BAU, the world protects up to the target
    pull(cost) %>% 
    sum()
  
  gains <- (bau - mkt) / bau
  
  return(gains)
}

targets <- seq(0.1, 1, by = 0.01)

# STEP 5: Call the function ----------------------------------------------------
global <- map_dbl(targets, gft)
hemisphere <- map_dbl(targets, gft, group = "hemisphere", ind = hem_eez_cb, agg = hem_h_sum)
realm <- map_dbl(targets, gft, group = "realm", ind = rlm_eez_cb, agg = rlm_h_sum)
province <- map_dbl(targets, gft, group = "province", ind = pro_eez_cb, agg = pro_h_sum)
ecoregion <- map_dbl(targets, gft, group = "ecoregion", ind = eco_eez_cb, agg = eco_h_sum)

results <- tibble(target = targets,
                  global,
                  hemisphere,
                  realm,
                  province,
                  ecoregion)

## STEP6: Visualize ############################################################

# 30X30 Bar chart --------------------------------------------------------------
results %>% 
  filter(near(target, 0.3)) %>% 
  pivot_longer(cols = c(global, hemisphere, realm, province, ecoregion),
               names_to = "bubble_policy",
               values_to = "gains_from_trade") %>% 
  mutate(bubble_policy = fct_reorder(bubble_policy, -gains_from_trade, max)) %>% 
  ggplot(aes(x = bubble_policy, y = gains_from_trade, fill = bubble_policy)) +
  geom_col()

# All policies all targets Line chart ------------------------------------------
results %>% 
  pivot_longer(cols = c(global, hemisphere, realm, province, ecoregion),
               names_to = "bubble_policy",
               values_to = "gains_from_trade") %>% 
  mutate(bubble_policy = fct_reorder(bubble_policy, -gains_from_trade, max)) %>% 
  ggplot(aes(x = target, y = gains_from_trade, color = bubble_policy)) +
  geom_line() +
  labs(subtitle = "Note the squigliness of some lines (e.g. Ecoregion). This is due to not allowing for the protection of fractional pixels, which is addressed in the detailed analysis.")

