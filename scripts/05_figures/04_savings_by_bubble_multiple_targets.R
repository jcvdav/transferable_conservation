################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# Oct 8, 2022
#
# Creates figure 4 in the paper
#
################################################################################

## SET UP ######################################################################
# Load packages
library(startR)
library(here)
library(cowplot)
library(tidyverse)

# Load data
outcome_data <-
  readRDS(here("results",
               "output_data",
               "gains_from_trade_bubbles.rds")) %>%
  mutate(
    bubble = fct_relevel(
      bubble,
      "Global (N = 1)",
      "Hemisphere (N = 4)",
      "Realm (N = 12)",
      "Province (N = 60)",
      "Ecoregion (N = 219)"
    )
  )

## PROCESSING ##################################################################

# Bar chart of savings by bubble for a 30% target ------------------------------

# Set target for plot
r_interest <- 0.3

# Make plot
savings_plot <- outcome_data %>%
  filter(near(r, r_interest)) %>%
  mutate(bubble = fct_reorder(bubble, ratio, .desc = T),
         r = paste0(round(r * 100), "%")) %>%
  ggplot(aes(x = bubble, y = ratio, fill = bubble)) +
  geom_col(position = "dodge") +
  scale_y_continuous(
    labels = scales::percent,
    expand = c(0, 0),
    limits = c(0, 1.01)
  ) +
  scale_fill_manual(values = c("#C13832", "#D28E00", "#9ECEEB", "#D4BF95", "#91B9A4")) +
  labs(x = "Bubble policy",
       y = "% Cost savings (difference / BAU)",
       fill = "Bubble policy",
       title = "Gains from trade under different spatial constraints on trade") +
  theme(legend.position = "None")

# Line chart of relative gains from trade by bubble policy and all targets -----

rel <- ggplot(outcome_data, aes(x = r, y = ratio, color = bubble)) +
  geom_rect(
    xmin = 0,
    xmax = 0.1,
    ymin = 0,
    ymax = Inf,
    color = "transparent",
    fill = "gray50",
    alpha = 0.1
  ) +
  geom_vline(xintercept = c(0.1, 0.3, 0.5), linetype = "dashed") +
  geom_line(linewidth = 2) +
  labs(x = "% Conservation Benefits",
       y = "Costs avoided (difference / BAU)",
       color = "Bubble policy",
       title = "Gains from trade for other conservation targets") +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(0.1, 1, by = 0.1),
    limits = c(0, 1.01),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    labels = scales::percent,
    expand = c(0, 0),
    limits = c(0, 1.01)
  ) +
  scale_color_manual(values = c("#C13832", "#D28E00", "#9ECEEB", "#D4BF95", "#91B9A4")) +
  theme(legend.position = "None")

# Combine figures --------------------------------------------------------------

figure <- plot_grid(savings_plot,
                    rel,
                    align = "hv",
                    ncol = 1)

## EXPORT FIGURES ##############################################################
lazy_ggsave(
  plot = figure,
  filename = "gains_from_trade_panel",
  width = 18,
  height = 18
)

saveRDS(object = figure,
        file = here("results", "ggplots", "gains_from_trade_panel.rds"))
