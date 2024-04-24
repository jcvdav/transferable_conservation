######################################################
#title#
######################################################
# 
# Draw a histogram of the Habitat Suitability Index
#
######################################################
library(here)
library(tidyverse)

master_cb <- readRDS(file = here("results", "processed_data", "master_costs_and_benefits.rds"))


hsi_hist <- ggplot(data = master_cb,
       mapping = aes(x = suitability)) +
  geom_histogram(binwidth = 0.01,
                 color = "black",
                 fill = "steelblue") +
  startR::ggtheme_plot() +
  labs(x = "HSI",
       y = "N",
       subtitle = "N = 52,879")
 

lazy_ggsave(hsi_hist,
            "hsi_hist",
            width = 15,
            height = 7)
