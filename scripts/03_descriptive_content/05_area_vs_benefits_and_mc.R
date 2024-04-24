
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
master_cb <- readRDS(file = here("results/processed_data/master_costs_and_benefits.rds"))


## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
p <- ggplot(master_cb, aes(x = area, y = benefit, color = mc)) +
  geom_jitter(shape = ".", height = 5, width = 5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  scale_color_viridis_c(trans = "log10") +
  ggtheme_plot() +
  coord_equal() +
  labs(x = "Grid area (Km2)",
       y = "Conservation benefit if protected (HSI-weighted-KM2)",
       color = "Marginal Costs") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(legend.position = c(0, 1),
        legend.justification = c(0,1))

ggplot(master_cb,
       aes(x = benefit, y = log(cost), color = mc)) +
  geom_point(shape = ".") +
  scale_color_viridis_c(trans = "log10") +
  ggtheme_plot() +
  labs(x = "Conservation benefit if protected (HSI-weighted-Km2)",
       y = "Costo of conserving pixel (log-transformed)",
       color = "Marginal Costs") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(legend.position = c(0, 1),
        legend.justification = c(0,1))

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
lazy_ggsave(plot = p,
            filename = "area_vs_benefits_and_mc",
            width = 10,
            height = 10)

