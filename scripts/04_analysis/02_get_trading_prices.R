#######################################################
# This scripts takes the five global conservation targets
# identified before, and calculates the marginal cost.
# 
# This value becomes the trading price. If a country hits this spot,
# it is cheaper for it to pay to conserve elsewhere, where marginal costs are lower.
# 
#######################################################

# SET UP ################################################################################################
# Load packages
library(startR)
library(tidyverse)

# Load data
## Global data
eez_h_sum_cb <- readRDS(
  file = file.path(project_path,"processed_data","eez_h_sum_costs_and_benefits.rds")
)

## National data
eez_cb <- readRDS(
  file = file.path(project_path, "processed_data", "eez_costs_and_benefits.rds")
)

# Load the calculated conservation targets
conservation_targets <- readRDS(
  file.path(project_path, "output_data", "conservation_targets.rds")
)


# PROCESSING ############################################################################################

# Calculate the trading price
trading_prices <- conservation_targets %>% 
  mutate(mc = map_dbl(tb, get_trading_price, eez_h_sum_cb))

# Figure of trading prices
trading_prices_plot <- 
  ggplot(mapping = aes(x = tb, y = mc)) +
  geom_line(data = eez_h_sum_cb,
            size = 1) +
  geom_segment(data = trading_prices, aes(x = tb, xend = tb, y = -5000, yend = mc),
               linetype = "dashed") +
  geom_segment(data = trading_prices, aes(x = 0, xend = tb, y = mc, yend = mc),
               linetype = "dashed") +
  geom_point(data = trading_prices, aes(fill = type), color = "black") +
  geom_text(data = trading_prices, aes(label = round(mc, 2)), nudge_y = c(3, 5, 1, 4, 2) * 1e6) +
  ggtheme_plot() +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  theme(legend.justification = c(0, 1),
        legend.position = c(0, 1)) +
  labs(x = "Biodiversity",
       y = "Marginal Costs",
       fill = "Approach") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))

# Export stuff
saveRDS(trading_prices,
        file = file.path(project_path, "output_data", "global_trading_prices.rds"))

lazy_ggsave(trading_prices_plot,
            "global_conservation_targets_and_trading_prices",
            width = 14,
            height = 7)

# Table

trading_prices %>% 
  mutate(type = str_to_sentence(type)) %>% 
  knitr::kable(format = "latex",
               digits = 2,
               col.names = c("Type", "Biodiversity", "Trading price"),
               label = "trading-prices",
               caption = "Trading prices for each of the five biodiversity targets, depending on the approach taken.") %>% 
  cat(file = here::here("results", "tab", "trading-prices.tex"))
















