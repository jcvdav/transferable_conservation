######################################################
#title#
######################################################
#
# Purpose
#
######################################################

## SET UP ######################################################################
# Load packages
library(startR)
library(tidyverse)

# Load data
outcome_data <-
  read.csv(file.path(project_path, "output_data", "gains_from_trade_bubbles.csv"))

# Set target for plot
r_interest <- 0.3

## PROCESSING ##################################################################

savings_plot <- filter(outcome_data, r == r_interest) %>%
  mutate(bubble = fct_reorder(bubble, ratio, .desc = T)) %>% 
  ggplot(aes(x = bubble, y = ratio)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = paste0(round(ratio * 100, 2), "%")), nudge_y = -0.1, size = 5) +
  cowplot::theme_half_open() +
  labs(x = "Bubble policy",
       y = "% Cost savings (difference / BAU)")


lazy_ggsave(plot = savings_plot,
            filename = "savings_30",
            width = 16,
            height = 9)
