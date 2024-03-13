################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Plots the number of nations vs the number of bubbles in each policy
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
library(startR)
library(here)
library(tidyverse)

# Load data --------------------------------------------------------------------
master_data <- readRDS(file = here("results",
                                   "processed_data",
                                   "master_costs_and_benefits.rds"))

## PROCESSING ##################################################################

data <- master_data %>%
  select(global, hemisphere, realm, province, ecoregion, iso3)

count_partners <- function(group, data) {
  data %>%
    group_by_at(group) %>%
    summarize(n_partners = n_distinct(iso3)) %>%
    ungroup() %>%
    mutate(bubble = group,
           n_bubbles = nrow(.)) %>%
    select(bubble, n_bubbles, n_partners)
}

counts <-
  c("global", "hemisphere", "realm", "province", "ecoregion") %>%
  map_dfr(count_partners, data = data) %>%
  mutate(
    bubble = str_to_sentence(bubble),
    bubble = fct_relevel(
      bubble,
      "Global",
      "Hemisphere",
      "Realm",
      "Province",
      "Ecoregion"
    )
  )

## VISUALIZE ###################################################################
p <- ggplot(data = counts,
            mapping = aes(x = n_bubbles, y = n_partners)) +
  geom_jitter(
    aes(fill = bubble),
    height = 0,
    width = 2,
    alpha = 0.8,
    size = 2
  ) +
  geom_hline(yintercept = 2, linetype = "dashed") +
  labs(x = "Number of bubbles",
       y = "Number of nations per bubble",
       fill = "Bubble policy") +
  scale_fill_manual(values = c("#C13832", "#D28E00", "#9ECEEB", "#D4BF95", "#91B9A4")) +
  scale_y_continuous(expand = expansion(0.05, 0)) +
  scale_x_continuous(expand = expansion(0.01, 0)) +
  startR::ggtheme_plot() +
  theme(legend.justification = c(1, 1),
        legend.position = c(1, 1))

## EXPORT FIGURE ###############################################################
lazy_ggsave(
  plot = p,
  filename = "nations_by_bubble",
  width = 10,
  height = 6
)
