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
library(here)
library(startR)
library(rnaturalearth)
library(rmapshaper)
library(sf)
library(ggnewscale)
library(tidyverse)

sf_use_s2(F)

# Load coastline ---------------------------------------------------------------
coast <- ne_countries(returnclass = "sf")

# Define target to visualize ---------------------------------------------------
r_target <- "0.30"

# Read data --------------------------------------------------------------------
file <- list.files(
  path = here(
    "results",
    "output_data",
    "trade_outcomes",
    "global"),
  pattern = r_target,
  full.names = T
)

data <- read.csv(file) %>%
  mutate(ratio = savings / bau_tc)

# Load EEZs for visualization --------------------------------------------------
eez <-
  st_read(file.path(project_path, "processed_data", "clean_world_eez_v11.gpkg")) %>%
  rmapshaper::ms_simplify(keep_shapes = T)

# Add results to the EEZs ------------------------------------------------------
eez_with_results <- eez %>%
  left_join(data, by = c("iso3"))

# Df of sellers ----------------------------------------------------------------
sellers <- eez_with_results %>%
  filter(transaction == "Sellers") %>%
  mutate(Sellers = pmin(ratio, 1))

# DF of buyers -----------------------------------------------------------------
buyers <- eez_with_results %>%
  filter(transaction == "Buyers") %>%
  mutate(Buyers = ratio)

#DF of those who neither buy nor sell ------------------------------------------
dont_participate <- eez_with_results %>%
  filter(transaction == "Doesn't participate")

# Define color scale guide -----------------------------------------------------
my_scale <-
  function(x,
           accuracy = NULL,
           scale = 100,
           prefix = "",
           suffix = "%",
           big.mark = " ",
           decimal.mark = ".",
           trim = TRUE,
           ...) {
    prefix <- character()
    prefix[!x == 1] <- ""
    prefix[x == 1] <- ">="
    
    scales::number(
      x = x,
      accuracy = accuracy,
      scale = scale,
      prefix = prefix,
      suffix = suffix,
      big.mark = big.mark,
      decimal.mark = decimal.mark,
      trim = trim,
      ...
    )
  }

## VISUALIZE ###################################################################
savings_map <- ggplot() +
  geom_sf(data = coast) +
  geom_sf(data = buyers, aes(fill = Buyers)) +
  scale_fill_gradient(low = "white", high = "#2166AB", labels = my_scale) +
  guides(
    fill = guide_legend(
      title = "Costs avoided\n by buyers (%BAU)",
      frame.colour = "black",
      ticks.colour = "black"
    )
  ) +
  new_scale_fill() +
  geom_sf(data = sellers, aes(fill = Sellers)) +
  scale_fill_gradient(low = "white", high = "#B1182B", labels = my_scale) +
  guides(
    fill = guide_legend(
      title = "Gains from trade\n by sellers (%BAU)",
      frame.colour = "black",
      ticks.colour = "black"
    )
  )  +
  geom_sf(data = dont_participate, fill = "gray") +
  ggtheme_map() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) 

## EXPORT ######################################################################
lazy_ggsave(
  plot = savings_map,
  filename = "30_by_segment/savings_map",
  width = 18.7,
  height = 9
)
