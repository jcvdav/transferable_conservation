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
r_interest <- c(0.3)

## PROCESSING ##################################################################

savings_plot <- outcome_data %>%
  filter(r %in% r_interest) %>%
  mutate(bubble = str_remove(string = bubble, pattern = "\\("),
         bubble = fct_reorder(bubble, ratio, .desc = T),
         r = paste0(round(r * 100), "%")) %>% 
  ggplot(aes(x = bubble, y = ratio, fill = bubble)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Set1") +
  ggtheme_plot() +
  labs(x = "Bubble policy",
       y = "% Cost savings (difference / BAU)",
       fill = "Bubble policy") +
  theme(legend.position = c(1, 1),
        legend.justification = c(1, 1))


lazy_ggsave(plot = savings_plot,
            filename = "savings_30",
            width = 18,
            height = 9)
