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
  rnaturalearth,
  raster,
  sf,
  tidyverse
)

# Load data --------------------------------------------------------------------
iso3n <-
  raster(here("clean_data", "eez_raster.tif")) %>% 
  as.data.frame(xy = T)

coast <- rnaturalearth::ne_countries(returnclass = "sf")

# Load HSI rasters
hsi <- raster(here("clean_data", "suitability.tif")) %>% 
  as.data.frame(xy = T)
hsi_all <- raster(here("clean_data", "suitability_with_all_occur_cells.tif")) %>% 
  as.data.frame(xy = T)

# Rischenss rasters
richness <- raster(here("clean_data", "richness.tif")) %>% 
  as.data.frame(xy = T)
richness_all <- raster(here("clean_data", "richness_with_all_occur_cells.tif")) %>% 
  as.data.frame(xy = T)

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
HSI_df <- left_join(hsi, hsi_all, by = c("x", "y")) %>% 
  left_join(iso3n, by = c("x", "y")) %>% 
  mutate(diff = suitability - suitability_with_all_occur_cells) %>% 
  drop_na()

S_df <- left_join(richness, richness_all, by = c("x", "y")) %>% 
  left_join(iso3n, by = c("x", "y")) %>% 
  mutate(diff = richness - richness_with_all_occur_cells) %>% 
  drop_na()

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
# Faceted map of HSI (with and without filter)
HSI <- HSI_df %>% 
  select(-diff) %>% 
  pivot_longer(cols = 3:4, names_to = "measure", values_to = "hsi") %>% 
  ggplot(aes(x = x, y = y, fill = hsi)) +
  geom_raster() +
  geom_sf(data = coast, inherit.aes = F, color = "black", fill = "black") +
  facet_wrap(~measure, ncol = 1) +
  scale_fill_viridis_c() +
  theme_void() +
  theme(panel.background = element_rect(fill = "gray")) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(fill = "HSI")

# Scatterplot
HSI_scatter <- ggplot(HSI_df, aes(x = suitability, y = suitability_with_all_occur_cells)) +
  geom_point(alpha = 0.5, size = 1) +
  labs(x = "HSI with species removed",
       y = "HSI with all species") +
  coord_equal()

HSI_diff <- ggplot(HSI_df, aes(x = x, y = y, fill = diff)) + 
  geom_raster() +
  geom_sf(data = coast, inherit.aes = F, color = "black", fill = "black") +
  scale_fill_gradient2() +
  theme_void() +
  ggtitle("Difference in HSI (Filtered - All data)") +
  labs(fill = "Difference") +
  theme(panel.background = element_rect(fill = "gray")) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))

S_diff <- ggplot(S_df, aes(x = x, y = y, fill = diff / richness_with_all_occur_cells)) + 
  geom_raster() +
  geom_sf(data = coast, inherit.aes = F, color = "black", fill = "black") +
  scale_fill_gradient2() +
  theme_void() +
  ggtitle("Difference in Species richness (Filtered - All data) / All data") +
  labs(fill = "Difference (%)") +
  theme(panel.background = element_rect(fill = "gray")) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))

HSI_plot <- cowplot::plot_grid(HSI,
                               HSI_scatter,
                               ncol = 1,
                               labels = "AUTO")

HSI_full <- cowplot::plot_grid(HSI_plot,
                               cowplot::plot_grid(HSI_diff, S_diff, ncol = 1, labels = c("C", "D")),
                               rel_widths = c(1, 2),
                               ncol = 2)

HSI_full

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
lazy_ggsave(plot = HSI_full,
            filename = "HSI_and richness_without_filters_ocurr_cells",
            width = 25,
            height = 18)
