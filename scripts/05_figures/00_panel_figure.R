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
library(here)
library(cowplot)
library(tidyverse)

# Load data --------------------------------------------------------------------
savings_map <- readRDS(file = here("results", "ggplots", "savings_map.rds")) +
  theme(legend.box.spacing = unit(0, "pt")) +
  labs(title = "Gains from trade by nation under 30x30 and a global market")
sources_of_efficiency <- readRDS(file = here("results", "ggplots", "sources_of_efficiency.rds"))
gains_from_trade_panel <- readRDS(file = here("results", "ggplots", "gains_from_trade_panel.rds"))

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
p <- plot_grid(
  savings_map,
  plot_grid(sources_of_efficiency,
            gains_from_trade_panel,
            ncol = 2),
  rel_heights = c(1.1, 1),
  ncol = 1
)

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
startR::lazy_ggsave(plot = p, filename = "panel",
                    width = 18,
                    height = 21)
