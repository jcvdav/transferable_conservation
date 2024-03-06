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
  startR,
  here,
  tidyverse
)

# Load data --------------------------------------------------------------------
glob <- readRDS(
  file = here(
    "results",
    "processed_data",
    "supply_curves",
    "with_mpas",
    "global_supply_curve_with_mpas.rds"
  )
) %>% 
  filter(pct <= 0.3) %>%
  tail(1) %>% 
  select(mc)

hem <- readRDS(
  file = here(
    "results",
    "processed_data",
    "supply_curves",
    "with_mpas",
    "hemisphere_supply_curves_with_mpas.rds"
  )
) %>% 
  filter(pct <= 0.3) %>%
  group_by(hemisphere) %>%
  slice_max(mc) %>% 
  ungroup() %>% 
  select(mc)

rlm <- readRDS(
  file = here(
    "results",
    "processed_data",
    "supply_curves",
    "with_mpas",
    "realm_supply_curves_with_mpas.rds"
  )
) %>% 
  filter(pct <= 0.3) %>%
  group_by(realm) %>%
  slice_max(mc) %>% 
  ungroup() %>% 
  select(mc)

pro <- readRDS(
  file = here(
    "results",
    "processed_data",
    "supply_curves",
    "with_mpas",
    "province_supply_curves_with_mpas.rds"
  )
) %>% 
  filter(pct <= 0.3) %>%
  group_by(province) %>%
  slice_max(mc) %>% 
  ungroup() %>% 
  select(mc)


eco <- readRDS(
  file = here(
    "results",
    "processed_data",
    "supply_curves",
    "with_mpas",
    "ecoregion_supply_curves_with_mpas.rds"
  )
) %>% 
  filter(pct <= 0.3) %>%
  group_by(ecoregion) %>%
  slice_max(mc) %>% 
  ungroup() %>% 
  select(mc)

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
data <- bind_rows(Global = glob,
                  Hemisphere = hem,
                  Realm = rlm,
                  Province = pro,
                  Ecoregion = eco,
                  .id = "source") %>%
  mutate(source = fct_relevel(source, "Global", "Hemisphere", "Realm", "Province", "Ecoregion"),
         mc = mc / 0.85)

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
p <- ggplot(data = data,
            mapping = aes(x = source, y = mc)) + 
  geom_hline(yintercept = 148, linetype = "dashed") +
  # The message "Removed 1 rows containing missing values (`geom_segment()`)." 
  # appears because the global case doesn't have a market-clearing price.
  stat_summary(geom = "pointrange",
               fun.data = mean_se, 
               fill = "steelblue", shape = 21) +
  labs(x = "Bubble policy",
       y = expression("Market clearing price ($ / Km"^2~")"))

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
lazy_ggsave(plot = p,
            file = "market_clearing_price",
            width = 8,
            height = 4)
