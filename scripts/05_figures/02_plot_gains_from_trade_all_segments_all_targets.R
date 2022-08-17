######################################################
#title#
######################################################
# 
# Purpose
#
######################################################

## SET UP ######################################################################
# Load packages
library(tidyverse)

# Load data
gains_from_trade_multiple_scenarios <-
  read_csv(file = file.path(project_path, "output_data", "gains_from_trade_bubbles.csv")) %>% 
  mutate(bubble = fct_relevel(bubble, "Global (N = 1)", "Hemisphere (N = 4)", "Realm (N = 12)", "Province (N = 60)", "Ecoregion (N = 219)"))

## PROCESSING ##################################################################
# Visualize absolute savings
abs <- ggplot(gains_from_trade_multiple_scenarios, aes(x = r, y = difference / 1e9, color = bubble)) +
  geom_rect(xmin = 0, xmax = 0.1, ymin = 0, ymax = 25e9, color = "transparent", fill = "gray") +
  geom_line(size = 1) +
  geom_vline(xintercept = c(0.1, 0.3, 0.5), linetype = "dashed") +
  ggtheme_plot() +
  labs(x = "% Conservation Benefits",
       y = "Costs avoided (BAU - MKT; billions)",
       color = "Bubble policy") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), breaks = seq(0.1, 1, by = 0.1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 22)) +
  scale_color_brewer(palette = "Set1") +
  theme(legend.justification = c(0, 1),
        legend.position = c(0, 1))

# Visualize relative savings
rel <- ggplot(gains_from_trade_multiple_scenarios, aes(x = r, y = ratio, color = bubble)) +
  geom_rect(xmin = 0, xmax = 0.1, ymin = 0, ymax = 1, color = "transparent", fill = "gray", alpha = 0.1) +
  geom_vline(xintercept = c(0.1, 0.3, 0.5), linetype = "dashed") +
  geom_line(size = 1) +
  ggtheme_plot() +
  labs(x = "% Conservation Benefits", 
       y = "Costs avoided (difference / BAU)",
       color = "Bubble policy") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = seq(0.1, 1, by = 0.1),
                     limits = c(0, 1.01),
                     expand = c(0, 0)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0),
                     limits = c(0, 1.01)) +
  scale_color_brewer(palette = "Set1")

# Combine relative and absolute
gain_from_trade_segmented_market_plot <- 
  plot_grid(abs,
            rel +
              theme(legend.position = "None"),
            ncol = 2, labels = "AUTO")


## EXPORT ######################################################################

lazy_ggsave(plot = abs,
            file = "abs_gain_from_trade_segmented_market_plot",
            width = 15,
            height = 10)

lazy_ggsave(plot = rel,
            file = "rel_gain_from_trade_segmented_market_plot",
            width = 15,
            height = 10)

lazy_ggsave(plot = gain_from_trade_segmented_market_plot,
            file = "gain_from_trade_segmented_market_plot",
            width = 25,
            height = 10)
