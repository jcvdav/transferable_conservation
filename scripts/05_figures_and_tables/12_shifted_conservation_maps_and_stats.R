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
library(smoothr)
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
  mutate(ratio = savings / bau_tc,
         pct_global = mkt_tb / sum(mkt_tb, na.rm = T),
         tb_change = mkt_tb / bau_tb)

pol <- tribble(~x, ~y,
               -180, -90,
               -180, 90,
               180, 90,
               180, -90,
               -180, -90) %>%
  as.matrix() %>%
  list() %>%
  st_polygon() %>%
  st_sfc(crs = "EPSG:4326") %>% 
  densify(n = 100L)

# Load EEZs for visualization --------------------------------------------------
eez <-
  st_read(file.path(project_path, "processed_data", "clean_world_eez_v11.gpkg")) %>%
  rmapshaper::ms_simplify(keep_shapes = T) %>%
  st_intersection(pol)


# Add results to the EEZs ------------------------------------------------------
eez_with_results <- eez %>%
  left_join(data, by = c("iso3"))

# Df of sellers ----------------------------------------------------------------
sellers <- eez_with_results %>%
  filter(transaction == "Sellers") %>%
  mutate(Sellers = tb_change)

# DF of buyers -----------------------------------------------------------------
buyers <- eez_with_results %>%
  filter(transaction == "Buyers") %>%
  mutate(Buyers = tb_change)

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

# 1 % Protection under a market
pct_global <- ggplot() +
  geom_sf(data = coast) +
  geom_sf(data = eez_with_results, aes(fill = pct_global)) +
  scale_fill_viridis_c(option = "B", labels = my_scale) +
  guides(
    fill = guide_colorbar(
      title = "% Of global conservation",
      title.position = "top",
      frame.colour = "black",
      ticks.colour = "black"
    )
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(legend.position = "bottom",
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  coord_sf(crs = "ESRI:54009")
  coord_sf(crs = "ESRI:54009")


shifted_map <- ggplot() +
  geom_sf(data = coast) +
  geom_sf(data = eez_with_results, aes(fill = tb_change)) +
  scale_fill_gradient2(low = "#B1182B",
                       high = "#2166AB",
                       midpoint = 1,
                       labels = my_scale) +
  guides(
    fill = guide_colorbar(
      title = "% Conserved (relative to BAU)",
      title.position = "top",
      frame.colour = "black",
      ticks.colour = "black"
    )
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(legend.position = "bottom",
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  coord_sf(crs = "ESRI:54009")



data %>%
  arrange(desc(pct_global)) %>%
  mutate(rank = 1:nrow(.),
         cum_pct = cumsum(pct_global)) %>%
  ggplot(aes(x = rank, y = cum_pct)) + 
  geom_segment(x = -Inf, xend = 10,
               y = 0.5, yend = 0.5,
               inherit.aes = F) +
  geom_segment(x = 10, xend = 10,
               y = 0, yend = 0.5,
               inherit.aes = F) +
  geom_vline(xintercept = 114) +
  geom_point() +
  geom_text(aes(x = rank - 1, y = cum_pct + 0.05, label = ifelse(rank <= 10, iso3, ""))) +
  labs(x = "Number of countries",
       y = "% of 30x30 target achieved") +
  theme(text = element_text(size = 12))


shifted_map <- ggplot() +
  geom_sf(data = coast) +
  geom_sf(data = buyers, aes(fill = Buyers)) +
  scale_fill_gradient(low = "white", high = "#2166AB", labels = my_scale) +
  
  new_scale_fill() +
  geom_sf(data = sellers, aes(fill = Sellers)) +
  scale_fill_gradient(low = "white", high = "#B1182B", ) +
  guides(
    fill = guide_legend(
      title = "Gains from trade\n by sellers (%BAU)",
      frame.colour = "black",
      ticks.colour = "black"
    )
  )  +
  geom_sf(data = dont_participate, fill = "gray") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(legend.position = "bottom",
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  coord_sf(crs = "ESRI:54009")

## EXPORT ######################################################################
lazy_ggsave(
  plot = shifted_map,
  filename = "30_by_segment/shifted_conservation_map",
  width = 18,
  height = 10
)

lazy_ggsave(
  plot = pct_global,
  filename = "30_by_segment/pct_of_global_conservation_map",
  width = 18,
  height = 10
)
