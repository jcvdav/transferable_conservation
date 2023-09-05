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
library(startR)
library(here)
library(cowplot)
library(tidyverse)

# Load data --------------------------------------------------------------------
data_30 <- readRDS(here(
  "results",
  "output_data",
  "bau_and_mkt_otucomes_global_30.rds"
))


# Define a plotting function to call multiple times further down ---------------
plot_measure <- function(data,
                         variable,
                         x_lab = "",
                         y_lab,
                         n = F) {
  p <- data %>%
    ggplot(aes(x = protected, y = {
      {
        variable
      }
    }, fill = protected)) +
    geom_violin() +
    scale_fill_manual(values = c("#047C91", "#6D7D33", "#09847A")) +
    ggtheme_plot() +
    labs(x = x_lab,
         y = y_lab) +
    theme(legend.position = "None")
  
  if (n) {
    N <- data %>%
      group_by(protected) %>%
      summarize(n = paste0("N = ", n()),
                y = max({
                  {
                    variable
                  }
                }))
    
    p <- p +
      geom_text(data = N, aes(
        x = protected,
        y = max(y) * 1.1,
        label = n
      ))
  }
  
  return(p)
}

## VISUALIZE ###################################################################

# Plot costs -------------------------------------------------------------------
cost <- plot_measure(
  data = data_30,
  variable = cost,
  x_lab = "",
  y_lab = expression(Costs ~ (log[10] ~ (M ~ USD))),
  n = T
) +
  scale_x_discrete(labels = NULL)

# Plot area
area <- plot_measure(
  data = data_30,
  variable = area,
  x_lab = "",
  y_lab = expression(Area ~ (Km ^ 2))
) +
  scale_x_discrete(labels = NULL)

# Plot suitability
suitability <- plot_measure(
  data = data_30,
  variable = suitability,
  x_lab = "Pixel status",
  y_lab = "Habitat Suitability\nIndex"
)

# Combine plots
p <- plot_grid(
  cost,
  area,
  suitability,
  align = "hv",
  ncol = 1,
  labels = "AUTO",
  label_x = 0.9
)

## EXPORT FIGURES ##############################################################
lazy_ggsave(
  plot = p,
  filename = "30_by_segment/sources_of_efficiency",
  width = 15,
  height = 12
)
