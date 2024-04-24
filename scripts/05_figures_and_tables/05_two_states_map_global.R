################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# Oct 8, 2022
#
# Maps the classification of pixels based on when they are protected
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  startR,
  rnaturalearth,
  tidyverse
)

# Load data --------------------------------------------------------------------
data_30 <- readRDS(here(
  "results",
  "output_data",
  "bau_and_mkt_otucomes_global_30.rds"
))

# Load coastline ---------------------------------------------------------------
coast <- ne_countries(returnclass = "sf")

## VISUALIZE ###################################################################
when <-
  ggplot(data = data_30) +
  geom_sf(data = coast, color = "black", fill = "transparent") +
  geom_tile(aes(x = lon, y = lat, fill = protected)) +
  scale_fill_manual(values = c("#047C91", "#6D7D33", "#09847A")) +
  ggtheme_map() +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) 

## EXPORT ######################################################################
lazy_ggsave(
  plot = when,
  filename = "30_by_segment/two_states_map",
  width = 20,
  height = 10
)
