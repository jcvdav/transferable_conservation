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
  here,
  startR,
  rnaturalearth,
  rmapshaper,
  smoothr,
  sf,
  ggnewscale,
  ggrepel,
  tidyverse
)

# Make sure this is turned off
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
  mutate(pct_global = mkt_tb / sum(mkt_tb, na.rm = T),                          # Calculate percent of total conservation produced by each nation
         tb_change = mkt_tb / bau_tb)                                           # The relative amount of conservation producd under a market vs under BAU

# Build a world polygon just to splice the shapefiles to change projections when visualizing
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
  st_read(here("clean_data", "clean_world_eez_v11.gpkg")) %>%
  rmapshaper::ms_simplify(keep_shapes = T) %>%
  st_intersection(pol) # Intersect it with the world polygon to make sure there are no polygons crossing the 180° Meridian


# Add results to the EEZs ------------------------------------------------------
eez_with_results <- eez %>%
  left_join(data, by = c("iso3"))

## VISUALIZE ###################################################################

# Figure 1, conservation attained within each nation as % of global
pct_global <- ggplot() +
  geom_sf(data = coast) +
  geom_sf(data = eez_with_results, aes(fill = pct_global)) +
  scale_fill_viridis_c(option = "B",
                       labels = scales::percent) +
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

# Figure 2, Conservation under market relative to BAU
shifted_map <- ggplot() +
  geom_sf(data = coast) +
  geom_sf(data = eez_with_results, aes(fill = tb_change)) +
  scale_fill_gradient2(low = "#B1182B",
                       high = "#2166AB",
                       midpoint = 1,
                       labels = scales::percent) +
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


# Figure 3 - Cumulative conservation
dist_plot <- data %>%
  arrange(desc(pct_global)) %>%
  mutate(rank = 1:nrow(.),
         cum_pct = cumsum(pct_global)) %>% 
  ggplot(aes(x = rank, y = cum_pct)) + 
  geom_segment(x = -Inf, xend = 10,
               y = 0.5, yend = 0.5,
               linetype = "dashed",
               inherit.aes = F) +
  geom_segment(x = 10, xend = 10,
               y = 0, yend = 0.5,
               linetype = "dashed",
               inherit.aes = F) +
  geom_vline(xintercept = 114) +
  geom_point() +
  geom_text_repel(aes(x = rank, y = cum_pct,
                      label = ifelse(cum_pct <= 0.5, iso3, "")),
                  size = 3,
                  min.segment.length = 0, nudge_x = -10) +
  annotate(geom = "text", x = 50, y = 1.1, label = "113 Nations conserve under MKT") +
  annotate(geom = "text", x = 155, y = 1.1, label = "71 Nations don't conserve under MKT") +
  annotate(geom = "text", x = 50, y = 0.3, label = "Top 11 conserving nations\nproduce >50% of conservation") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Cumulative number of nations",
       y = "% of global conservation (30X30)") +
  theme(text = element_text(size = 12))

## EXPORT ######################################################################
lazy_ggsave(
  plot = shifted_map,
  filename = "30_by_segment/shifted_conservation_map",
  width = 12,
  height = 8
)

lazy_ggsave(
  plot = pct_global,
  filename = "30_by_segment/pct_of_global_conservation_map",
  width = 12,
  height = 8
)

lazy_ggsave(
  plot = dist_plot,
  filename = "30_by_segment/pct_of_global_conservation",
  width = 20,
  height = 10
)


