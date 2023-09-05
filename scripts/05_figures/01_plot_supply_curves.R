################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Plots the supply curves
#
################################################################################

## SET UP ######################################################################

# SET UP ################################################################################################
# Load packages
library(here)
library(startR)
library(cowplot)
library(tidyverse)

# Load data
## Global market
### By country
eez_cb <- readRDS(
  file = here(
    "results",
    "processed_data",
    "supply_curves",
    "with_mpas",
    "global_eez_supply_curves_with_mpas.rds"
  )
)
### Horizontally summed
eez_h_sum <- readRDS(
  file = here(
    "results",
    "processed_data",
    "supply_curves",
    "with_mpas",
    "global_supply_curve_with_mpas.rds"
  )
)

## Hemisphere market
### By country-hemisphere
hem_cb <- readRDS(
  file = here(
    "results",
    "processed_data",
    "supply_curves",
    "with_mpas",
    "hemisphere_eez_supply_curves_with_mpas.rds"
  )
)

### Horizontally summed for each hemishpere
hem_h_sum <- readRDS(
  file = here(
    "results",
    "processed_data",
    "supply_curves",
    "with_mpas",
    "hemisphere_supply_curves_with_mpas.rds"
  )
)

## Realm eez
### For each realm and country
rlm_eez_cb <- readRDS(
  file = here(
    "results",
    "processed_data",
    "supply_curves",
    "with_mpas",
    "realm_eez_supply_curves_with_mpas.rds"
  )
)

### Horizontally summed for each realm
rlm_h_sum <- readRDS(
  file = here(
    "results",
    "processed_data",
    "supply_curves",
    "with_mpas",
    "realm_supply_curves_with_mpas.rds"
  )
)

## Province eez
### For each province, realm and country
pro_eez_cb <- readRDS(
  file = here(
    "results",
    "processed_data",
    "supply_curves",
    "with_mpas",
    "province_eez_supply_curves_with_mpas.rds"
  )
)
### Horizontaly summed for each province
pro_h_sum <- readRDS(
  file = here(
    "results",
    "processed_data",
    "supply_curves",
    "with_mpas",
    "province_supply_curves_with_mpas.rds"
  )
)

# Ecoregion
eco_eez_cb <- readRDS(
  file = here(
    "results",
    "processed_data",
    "supply_curves",
    "with_mpas",
    "ecoregion_eez_supply_curves_with_mpas.rds"
  )
)
eco_h_sum <- readRDS(
  file = here(
    "results",
    "processed_data",
    "supply_curves",
    "with_mpas",
    "ecoregion_supply_curves_with_mpas.rds"
  )
)


# PROCESSING ############################################################################################

# Global curve
global <- ggplot(data = eez_h_sum,
                 mapping = aes(x = tb / 1e3, y = mc)) +
  geom_line(size = 1) +
  ggtheme_plot() +
  labs(
    x = bquote("Conservation benefit (HS weighted; Thousand " ~ Km ^ 2 ~ ")"),
    y = bquote("Marginal cost ($/Q)")
  ) +
  scale_x_continuous(expand = expansion(0.01, 0)) +
  scale_y_continuous(expand = expansion(0.01, 0))

# Country-level supply curves
eez <- ggplot(data = eez_cb,
              mapping = aes(x = tb / 1e3, y = mc, group = iso3)) +
  geom_line() +
  guides(color = "none") +
  ggtheme_plot() +
  labs(
    x = bquote("Conservation benefit (HSI-weighted Thousand " ~ Km ^ 2 ~ ")"),
    y = bquote("Marginal cost ($/HSI-weighted " ~ Km ^ 2 ~ ")")
  ) +
  scale_x_continuous(expand = expansion(0.01, 0)) +
  scale_y_continuous(expand = expansion(0.01, 0))

eez_supply_curve <-
  plot_grid(eez, global, ncol = 2, labels = "AUTO")

# Hemisphere

eez_hem <-
  ggplot(data = hem_cb,
         mapping = aes(x = tb / 1e3, y = mc, group = iso3)) +
  geom_line() +
  guides(color = "none") +
  scale_color_viridis_d() +
  facet_wrap(~ hemisphere, scales = "free") +
  ggtheme_plot() +
  labs(
    x = bquote("Conservation benefit (HS weighted; Thousand " ~ Km ^ 2 ~ ")"),
    y = bquote("Fisheries revenue ($/Q)")
  ) +
  scale_x_continuous(expand = expansion(0.01, 0)) +
  scale_y_continuous(expand = expansion(0.01, 0))

# Hem summed
hem <- ggplot(data = hem_h_sum,
              mapping = aes(x = tb, y = mc, color = hemisphere)) +
  geom_line() +
  scale_color_viridis_d() +
  ggtheme_plot() +
  labs(
    x = bquote("Conservation benefit (HS weighted; Thousand " ~ Km ^ 2 ~ ")"),
    y = bquote("Fisheries revenue ($/Q)")
  ) +
  scale_x_continuous(expand = expansion(0.01, 0)) +
  scale_y_continuous(expand = expansion(0.01, 0))

eez_hem_supply_curve <-
  plot_grid(eez_hem, hem, ncol = 1, labels = "AUTO")


# Country realm
eez_rlm <-
  ggplot(data = rlm_eez_cb,
         mapping = aes(x = tb / 1e3, y = mc, group = iso3)) +
  geom_line() +
  facet_wrap(~ realm, scales = "free") +
  ggtheme_plot() +
  labs(
    x = bquote("Conservation benefit (HS weighted; Thousand " ~ Km ^ 2 ~ ")"),
    y = bquote("Fisheries revenue ($/Q)")
  ) +
  scale_x_continuous(expand = expansion(0.01, 0)) +
  scale_y_continuous(expand = expansion(0.01, 0))

# realm summed
rlm <- ggplot(data = rlm_h_sum,
              mapping = aes(x = tb, y = mc, color = realm)) +
  geom_line() +
  scale_color_viridis_d() +
  ggtheme_plot() +
  labs(
    x = bquote("Conservation benefit (HS weighted; Thousand " ~ Km ^ 2 ~ ")"),
    y = bquote("Fisheries revenue ($/Q)")
  ) +
  scale_x_continuous(expand = expansion(0.01, 0)) +
  scale_y_continuous(expand = expansion(0.01, 0))

eez_rlm_supply_curve <-
  plot_grid(eez_rlm, rlm, ncol = 1, labels = "AUTO")

# Country province
eez_pro <-
  ggplot(data = pro_eez_cb,
         mapping = aes(x = tb / 1e3, y = mc, group = iso3)) +
  geom_line() +
  facet_wrap(~ province, scales = "free", ncol = 6) +
  ggtheme_plot(font_size = 8) +
  labs(
    x = bquote("Conservation benefit (HS weighted; Thousand " ~ Km ^ 2 ~ ")"),
    y = bquote("Fisheries revenue ($/Q)")
  ) +
  scale_x_continuous(expand = expansion(0.01, 0)) +
  scale_y_continuous(expand = expansion(0.01, 0))

# province summed
pro <- ggplot(data = pro_h_sum,
              mapping = aes(x = tb, y = mc, color = province)) +
  geom_line() +
  scale_color_viridis_d() +
  ggtheme_plot() +
  theme(legend.position = "None") +
  labs(
    x = bquote("Conservation benefit (HS weighted; Thousand " ~ Km ^ 2 ~ ")"),
    y = bquote("Fisheries revenue ($/Q)")
  ) +
  scale_x_continuous(expand = expansion(0.01, 0)) +
  scale_y_continuous(expand = expansion(0.01, 0))

eez_pro_supply_curve <-
  plot_grid(eez_pro, rlm, ncol = 1, labels = "AUTO")

# EXPORT PLOTS ##############################################################################################
# Panels
lazy_ggsave(
  plot = eez_supply_curve,
  filename = "supply_curves/eez_supply_curve_with_mpas",
  width = 20,
  height = 7
)

lazy_ggsave(
  plot = eez_hem_supply_curve ,
  filename = "supply_curves/eez_hem_supply_curve_with_mpas",
  width = 20,
  height = 20
)

lazy_ggsave(
  plot = eez_rlm_supply_curve ,
  filename = "supply_curves/eez_rlm_supply_curve_with_mpas",
  width = 20,
  height = 20
)

# Singles
lazy_ggsave(
  plot = eez,
  filename = "supply_curves/eez_supply_curve_no_hsum_with_mpas",
  width = 10,
  height = 7
)

lazy_ggsave(
  plot = eez_hem,
  filename = "supply_curves/hem_supply_curve_no_hsum_with_mpas",
  width = 15,
  height = 10
)

lazy_ggsave(
  plot = eez_rlm,
  filename = "supply_curves/rlm_supply_curve_no_hsum_with_mpas",
  width = 17.5,
  height = 10
)

lazy_ggsave(
  plot = eez_pro,
  filename = "supply_curves/pro_supply_curve_no_hsum_with_mpas",
  width = 20,
  height = 25
)

# Presentation figures

labs <- eez_cb %>%
  filter(iso3 %in% c("MEX", "PER"),
         pct == 1)

eez_all <- ggplot(data = eez_cb,
                  mapping = aes(x = tb / 1e3, y = mc, group = iso3)) +
  geom_line(color = "transparent") +
  guides(color = "none") +
  ggtheme_plot() +
  labs(
    x = bquote("Conservation benefit (HS weighted; Thousand " ~ Km ^ 2 ~ ")"),
    y = bquote("Fisheries revenue ($/Q)")
  ) +
  scale_x_continuous(expand = expansion(0.01, 0)) +
  scale_y_continuous(expand = expansion(0.01, 0))

eez_supply_curve_PER <- eez_all +
  geom_line(
    data = filter(eez_cb, iso3 == "PER"),
    mapping = aes(x = tb / 1e3, y = mc, group = iso3),
    size = 1
  ) +
  guides(color = "none") +
  ggtheme_plot() +
  labs(
    x = bquote("Conservation benefit (HS weighted; Thousand " ~ Km ^ 2 ~ ")"),
    y = bquote("Fisheries revenue ($/Q)")
  ) +
  geom_text(data = labs %>% filter(iso3 == "PER"), aes(label = iso3)) +
  scale_x_continuous(expand = expansion(0.01, 0)) +
  scale_y_continuous(expand = expansion(0.01, 0))

eez_supply_curve_PER_MEX <- eez_all +
  geom_line(data = filter(eez_cb, iso3 %in% c("MEX", "PER")), size = 1) +
  geom_text(data = labs, aes(label = iso3)) +
  scale_x_continuous(expand = expansion(0.01, 0)) +
  scale_y_continuous(expand = expansion(0.01, 0))

eez_all <- eez_all +
  geom_line() +
  geom_text(data = labs, aes(label = iso3))


lazy_ggsave(
  plot = eez_supply_curve_PER,
  filename = "supply_curves/eez_supply_curve_no_hsum_with_mpas_PER",
  width = 10,
  height = 7
)

lazy_ggsave(
  plot = eez_supply_curve_PER_MEX,
  filename = "supply_curves/eez_supply_curve_no_hsum_with_mpas_PER_MEX",
  width = 10,
  height = 7
)

lazy_ggsave(
  plot = eez_all,
  filename = "supply_curves/eez_supply_curve_no_hsum_with_mpas_PER_MEX_all",
  width = 10,
  height = 7
)


# END OF SCRIPT
