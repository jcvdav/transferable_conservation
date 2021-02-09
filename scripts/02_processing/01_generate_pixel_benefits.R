library(raster)
library(sf)
library(tidyverse)

source(here::here("common.R"))
source(here::here("functions", "map_priorities.R"))

# Load data 

spp_files <- tibble(filepath = list.files(c(file.path(ng_data_path, "02_processed", "species_distributions", "birdlife"),
                                            file.path(ng_data_path, "02_processed", "species_distributions", "aquamaps")),
                                          full.names = T),
                    valid_sci_name = str_replace_all(str_remove(basename(filepath), "\\.tif"), "_", " ")) %>% 
  arrange(valid_sci_name)

features_df <- stack(spp_files$filepath) %>%
  raster::as.data.frame(xy = T) %>%
  rownames_to_column(var = "cell_id") %>% 
  # inner_join(ocean_matrix) %>%
  select(x, y, cell_id, everything()) %>%
  as_tibble()

n_features <- nrow(spp_files) 


## Normalize and trim

features_matrix <- features_df %>% 
  select(-x,-y,-cell_id) %>% 
  as.matrix()

metadata <- features_df %>% 
  select(x, y, cell_id)

Norm_features_matrix <- sweep(features_matrix, 2, colSums(features_matrix, na.rm = T), FUN = "/")

rownames(Norm_features_matrix) <- features_df$cell_id

# Norm_features_matrix <- Norm_features_matrix[rowSums(is.na(Norm_features_matrix)) != ncol(Norm_features_matrix), , drop = F]

Norm_features_matrix[is.na(Norm_features_matrix)] <- 0

saveRDS(Norm_features_matrix,
        file = file.path(project_path, "processed_data", "norm_features_matrix.rds"))

suitability_layer <- tibble(suitability = matrixStats::rowSums2(Norm_features_matrix),
                            cell_id = rownames(Norm_features_matrix)) %>% 
  left_join(metadata, by = "cell_id") %>% 
  select(x, y, suitability) %>% 
  mutate(suitability = suitability / max(suitability, na.rm = T)) %>%
  rasterFromXYZ(crs = proj_moll)

plot(suitability_layer)

writeRaster(x = suitability_layer, filename = file.path(project_path, "processed_data", "suitability.tif"))

