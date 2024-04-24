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
master_data <- readRDS(here("results/processed_data/master_costs_and_benefits.rds")) %>% 
  select(lon, lat, hemisphere, realm, province, ecoregion, iso3)

# Load data --------------------------------------------------------------------
glob <- readRDS(
  file = here(
    "results",
    "processed_data",
    "supply_curves",
    "with_mpas",
    "global_eez_supply_curves_with_mpas.rds"
  )
) %>% 
  mutate(pixel_fraction = pmin(1 - ((tb - ((tb * 0.3) / pct)) / benefit), 1)) %>%
  filter(pixel_fraction >= 0) %>% 
  mutate(benefit = benefit * pixel_fraction)

hem <- readRDS(
  file = here(
    "results",
    "processed_data",
    "supply_curves",
    "with_mpas",
    "hemisphere_eez_supply_curves_with_mpas.rds"
  )
) %>% 
  mutate(pixel_fraction = pmin(1 - ((tb - ((tb * 0.3) / pct)) / benefit), 1)) %>%
  filter(pixel_fraction >= 0) %>% 
  mutate(benefit = benefit * pixel_fraction)

rlm <- readRDS(
  file = here(
    "results",
    "processed_data",
    "supply_curves",
    "with_mpas",
    "realm_eez_supply_curves_with_mpas.rds"
  )
) %>% 
  mutate(pixel_fraction = pmin(1 - ((tb - ((tb * 0.3) / pct)) / benefit), 1)) %>%
  filter(pixel_fraction >= 0) %>% 
  mutate(benefit = benefit * pixel_fraction)

pro <- readRDS(
  file = here(
    "results",
    "processed_data",
    "supply_curves",
    "with_mpas",
    "province_eez_supply_curves_with_mpas.rds"
  )
) %>% 
  mutate(pixel_fraction = pmin(1 - ((tb - ((tb * 0.3) / pct)) / benefit), 1)) %>%
  filter(pixel_fraction >= 0) %>% 
  mutate(benefit = benefit * pixel_fraction)


eco <- readRDS(
  file = here(
    "results",
    "processed_data",
    "supply_curves",
    "with_mpas",
    "ecoregion_eez_supply_curves_with_mpas.rds"
  )
) %>% 
  mutate(pixel_fraction = pmin(1 - ((tb - ((tb * 0.3) / pct)) / benefit), 1)) %>%
  filter(pixel_fraction >= 0) %>% 
  mutate(benefit = benefit * pixel_fraction) %>% 
  select(-ecoregion)

eco_base <- readRDS(
  file = here(
    "results",
    "processed_data",
    "supply_curves",
    "with_mpas",
    "ecoregion_supply_curves_with_mpas.rds"
  )) %>% 
  group_by(ecoregion, protected) %>% 
  summarize(tot = sum(benefit)) %>% 
  mutate(tot2 = tot + protected) %>% 
  rename(eco_protected = protected)

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
glob_bubble <- master_data %>% 
  inner_join(glob, by = c("lat", "lon")) %>% 
  group_by(ecoregion) %>% 
  summarize(tb = sum(benefit), .groups = "drop") %>% 
  mutate(policy = "Global (N = 1)")

hem_bubble <- master_data %>% 
  inner_join(hem, by = c("lat", "lon")) %>% 
  group_by(ecoregion) %>% 
  summarize(tb = sum(benefit), .groups = "drop") %>% 
  mutate(policy = "Hemisphere (N = 4)")

rlm_bubble <- master_data %>% 
  inner_join(rlm, by = c("lat", "lon")) %>% 
  group_by(ecoregion) %>% 
  summarize(tb = sum(benefit), .groups = "drop") %>% 
  mutate(policy = "Realm (N = 12)")

pro_bubble <- master_data %>% 
  inner_join(pro, by = c("lat", "lon")) %>% 
  group_by(ecoregion) %>% 
  summarize(tb = sum(benefit), .groups = "drop") %>% 
  mutate(policy = "Province (N = 60)")

eco_bubble <- master_data %>% 
  inner_join(eco, by = c("lat", "lon")) %>% 
  group_by(ecoregion) %>% 
  summarize(tb = sum(benefit), .groups = "drop") %>% 
  mutate(policy = "Ecoregion (N = 219)")



data <- bind_rows(glob_bubble,
                  hem_bubble,
                  rlm_bubble,
                  pro_bubble,
                  eco_bubble) %>% 
  complete(ecoregion, nesting(policy)) %>%
  inner_join(eco_base, by = "ecoregion") %>% 
  replace_na(replace = list(tb = 0,
                            pct = 0)) %>% 
  mutate(tb = tb + eco_protected,
         pct = tb / tot2) %>% 
  replace_na(replace = list(tb = 0,
                            pct = 0)) %>% 
  mutate(policy = fct_relevel(policy, "Global (N = 1)",
                              "Hemisphere (N = 4)",
                              "Realm (N = 12)",
                              "Province (N = 60)",
                              "Ecoregion (N = 219)"))
## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
set.seed(1)
p <- ggplot(data, aes(x = policy, y = pct, fill = policy)) +
  geom_hline(yintercept = 0,
             linetype = "dashed") +
  geom_hline(yintercept = 0.3,
             linetype = "dashed") +
  geom_hline(yintercept = 1,
             linetype = "dashed") +
  geom_jitter(height = 0, width = 0.2,
              size = 2) +
  theme_bw() +
  labs(x = "Bubble policy",
       y = "% of Ecoregion protected") +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0.1, 1, by = 0.1)) +
  scale_fill_manual(values = c("#C13832", "#D28E00", "#9ECEEB", "#D4BF95", "#91B9A4")) +
  theme(legend.position = "None")

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
lazy_ggsave(plot = p,
            filename = "30_by_segment/bubble_policy_vs_ecoregion_protection_BAU",
            width = 15,
            height = 9)
