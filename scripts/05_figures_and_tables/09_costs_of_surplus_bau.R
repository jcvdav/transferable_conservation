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
gains_from_trade_bubbles <- readRDS(file = here("results", "output_data", "gains_from_trade_bubbles.rds")) %>% 
  mutate(bubble = fct_relevel(bubble, "Global (N = 1)",
                              "Hemisphere (N = 4)",
                              "Realm (N = 12)",
                              "Province (N = 60)",
                              "Ecoregion (N = 219)")) 

summarized_gains <-  gains_from_trade_bubbles %>% 
  select(r, bubble, bau_tc, difference)

metadata <- tibble(path = list.files(path = here("results",
                                                 "output_data",
                                                 "trade_outcomes"),
                                     recursive = T,
                                     full.names = T),
                   r = str_extract(string = path,
                                   pattern = "[:digit:].[:digit:]{2}"),
                   bubble = str_extract(string = path,
                                        pattern = "global|hemisphere|realm|province|ecoregion"))

gains <- metadata %>% 
  mutate(data = map(path, read_csv)) %>% 
  select(r, bubble, data) %>% 
  unnest(data)

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
data <- gains %>%
  filter(!is.na(mc_stop), bau_tb > 0, !is.na(trading_price)) %>%                                # Remove places that don't produce any additional conservation becuase they've already met it
  select(r, bubble, trading_price, mkt_tb, bau_tb, mc_stop) %>%
  group_by(r, bubble, trading_price) %>%
  summarize(mkt_tb = sum(mkt_tb, na.rm = T),
            bau_tb = sum(bau_tb, na.rm = T),
            mc_stop_median = median(mc_stop[mc_stop >= trading_price], na.rm = T)) %>% 
  ungroup() %>% 
  mutate(dif_tb = bau_tb - mkt_tb,
         dif_tb_pct = dif_tb / mkt_tb,
         dif_tc_median_mc = dif_tb * mc_stop_median,
         dif_tc_trading_price = dif_tb * trading_price,
         r = as.numeric(r)) %>% 
  filter(dif_tb > 0) %>% 
  group_by(r, bubble) %>%
  summarize_all("sum", na.rm = T) %>% 
  ungroup() %>% 
  mutate(
    bubble = stringr::str_to_sentence(bubble),
    bubble = case_when(
      bubble == "Global" ~ "Global (N = 1)",
      bubble == "Hemisphere" ~ "Hemisphere (N = 4)",
      bubble == "Realm" ~ "Realm (N = 12)",
      bubble == "Province" ~ "Province (N = 60)",
      bubble == "Ecoregion" ~ "Ecoregion (N = 219)")) %>% 
  select(r, bubble, dif_tb, dif_tb_pct, dif_tc_median_mc, dif_tc_trading_price) %>% 
  pivot_longer(cols = c(dif_tc_median_mc, dif_tc_trading_price), names_to = "estimate", values_to = "dif_tc") %>% 
  mutate(estimate = ifelse(estimate == "dif_tc_median_mc", "Median marginal cost", "Market clearing price")) %>% 
  left_join(summarized_gains, by = c("r", "bubble")) %>% 
  mutate(bubble = fct_relevel(bubble, "Global (N = 1)",
                              "Hemisphere (N = 4)",
                              "Realm (N = 12)",
                              "Province (N = 60)",
                              "Ecoregion (N = 219)")) 

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
plot1 <- ggplot(data = gains_from_trade_bubbles,
                aes(x = r, y = ((bau_tb - mkt_tb) / mkt_tb), color = bubble)) +
  geom_rect(xmin = 0,
            xmax = 0.1,
            ymin = 0,
            ymax = Inf,
            color = "transparent",
            fill = "gray50",
            alpha = 0.1) +
  geom_vline(xintercept = c(0.1, 0.3, 0.5), linetype = "dashed") +
  geom_line(linewidth = 1) +
  labs(x = "% Conservation Benefits",
       y = "Additional Benefit (BAU - MKT) / MKT",
       color = "Bubble policy",
       title = "Surplus conservation") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = seq(0.1, 1, by = 0.1),
                     limits = c(0, 1.01),
                     expand = c(0, 0)) +
  scale_y_continuous(labels = scales::percent,
                     expand = c(0.1, 0)) +
  scale_color_manual(values = c("#C13832", "#D28E00", "#9ECEEB", "#D4BF95", "#91B9A4")) +
  theme(legend.position = c(1, 0.5),
        legend.justification = c(1, 0.5))

plot2 <- ggplot(data = data,
                aes(x = r, y = (dif_tc / difference), color = bubble, linetype = estimate)) +
  geom_rect(xmin = 0,
            xmax = 0.1,
            ymin = 0,
            ymax = Inf,
            color = "transparent",
            fill = "gray50",
            alpha = 0.1) +
  geom_vline(xintercept = c(0.1, 0.3, 0.5), linetype = "dashed") +
  geom_line(linewidth = 1) +
  guides(color = "none") +
  labs(x = "% Conservation Benefits",
       y = "Additional costs (% of gains from trade)",
       title = "Costs of surplus conservation as a fraction of gains from trade",
       linetype = "Estimte") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = seq(0.1, 1, by = 0.1),
                     limits = c(0, 1.01),
                     expand = c(0, 0)) +
  scale_y_continuous(labels = scales::percent,
                     expand = c(0.1, 0)) +
  scale_color_manual(values = c("#C13832", "#D28E00", "#9ECEEB", "#D4BF95", "#91B9A4")) +
  theme(legend.position = c(1, 0.5),
        legend.justification = c(1, 0.5))


plot <- cowplot::plot_grid(plot1, plot2, ncol = 1)


## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
lazy_ggsave(plot = plot,
            filename = "additional_tb_under_bau",
            width = 10,
            height = 12)
