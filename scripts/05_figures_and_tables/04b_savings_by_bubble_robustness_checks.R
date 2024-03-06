################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
#
#
################################################################################

## SET UP ######################################################################
# Load packages
pacman::p_load(
  startR,
  here,
  cowplot,
  tidyverse
)

# Load data
outcome_data <-
  readRDS(here("results",
               "output_data",
               "gains_from_trade_bubbles.rds")) %>% 
  mutate(type = "Main text")

outcome_data_no_mpa <-
  readRDS(here("results",
               "output_data",
               "gains_from_trade_bubbles_no_mpas.rds")) %>% 
  mutate(type = "no MPAs")

outcome_data_mc0 <-
  readRDS(here("results",
               "output_data",
               "gains_from_trade_bubbles_mc0.rds")) %>% 
  mutate(type = "MPA mc = 0")

outcome_data_abt <-
  readRDS(here("results",
               "output_data",
               "gains_from_trade_bubbles_abt.rds")) %>% 
  mutate(type = "area-based target")

## PROCESSING ##################################################################
# Bar chart of savings by bubble for a 30% target ------------------------------

# Set target for plot
r_interest <- 0.3

# Make plotting functions ------------------------------------------------------
savings_30_plot <- function(baseline, robustness) {
  
  outcome_data <- bind_rows(baseline, robustness) %>% 
    mutate(bubble = fct_relevel(bubble, "Global (N = 1)",
                                "Hemisphere (N = 4)",
                                "Realm (N = 12)",
                                "Province (N = 60)",
                              "Ecoregion (N = 219)")) %>% 
    filter(near(r, r_interest)) %>%
    mutate(bubble = fct_reorder(bubble, ratio, .desc = T),
           r = paste0(round(r * 100), "%"))
    
    
    ggplot(data = outcome_data,
           mapping = aes(x = bubble, y = ratio, fill = bubble, linetype = type)) +
    geom_col(position = "dodge") +
    scale_y_continuous(labels = scales::percent,
                       expand = c(0, 0),
                       limits = c(0, 1.01)) +
    scale_fill_manual(values = c("#C13832", "#D28E00", "#9ECEEB", "#D4BF95", "#91B9A4")) +
    labs(x = "Bubble policy",
         y = "% Cost savings (difference / BAU)",
         fill = "Bubble policy") +
    theme(legend.position = "None")
}

# Line chart of relative gains from trade by bubble policy and all targets -----

rel_plot <- function(baseline, robustness) {
  
  outcome_data <- bind_rows(baseline, robustness) %>% 
    mutate(bubble = fct_relevel(bubble, "Global (N = 1)",
                                "Hemisphere (N = 4)",
                                "Realm (N = 12)",
                                "Province (N = 60)",
                                "Ecoregion (N = 219)")) 
  
  
  ggplot(data = outcome_data,
         mapping = aes(x = r, y = ratio, color = bubble, linetype = type)) +
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
         y = "% Costs avoided (difference / BAU)",
         color = "Bubble policy",
         linetype = "Source") +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                       breaks = seq(0.1, 1, by = 0.1),
                       limits = c(0, 1.01),
                       expand = c(0, 0)) +
    scale_y_continuous(labels = scales::percent,
                       expand = c(0, 0),
                       limits = c(0, 1.01)) +
    scale_color_manual(values = c("#C13832", "#D28E00", "#9ECEEB", "#D4BF95", "#91B9A4")) +
    theme(legend.position = "bottom", legend.box = "vertical")
}

# Build figures ----------------------------------------------------------------

# No MPA 
rel_plot_no_mpa <- rel_plot(outcome_data, outcome_data_no_mpa)
savings_30_plot_no_mpa <- savings_30_plot(outcome_data, outcome_data_no_mpa)

# MC = 0 
rel_plot_mc0 <- rel_plot(outcome_data, outcome_data_mc0)
savings_30_plot_mc0 <- savings_30_plot(outcome_data, outcome_data_mc0)

# Area-based targets
rel_plot_abt <- rel_plot(outcome_data, outcome_data_abt)
savings_30_plot_abt <- savings_30_plot(outcome_data, outcome_data_abt)

# Combine figures --------------------------------------------------------------

no_mpa_figure <- plot_grid(rel_plot_no_mpa,
                           savings_30_plot_no_mpa,
                           align = "hv", axis = "l",
                           rel_heights = c(1.5, 1),
                           ncol = 1)

mc0_figure <- plot_grid(rel_plot_mc0,
                        savings_30_plot_mc0,
                        align = "hv", axis = "l",
                        rel_heights = c(1.5, 1),
                        ncol = 1)

abt_figure <- plot_grid(rel_plot_abt,
                        savings_30_plot_abt,
                        align = "hv", axis = "l",
                        rel_heights = c(1.5, 1),
                        ncol = 1)


## EXPORT FIGURES ##############################################################
lazy_ggsave(
  plot = no_mpa_figure,
  filename = "gains_from_trade_panel_no_mpas",
  width = 13,
  height = 15
)

lazy_ggsave(
  plot = mc0_figure,
  filename = "gains_from_trade_panel_mc0",
  width = 13,
  height = 15
)

lazy_ggsave(
  plot = abt_figure,
  filename = "gains_from_trade_panel_abt",
  width = 13,
  height = 15
)
