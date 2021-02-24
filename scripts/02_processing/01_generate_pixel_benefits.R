
## Load packages
library(raster)
library(sf)
library(tidyverse)

# Load ocean mask to remove rogue pixels
ocean_mask <- raster(
  file.path(project_path, "processed_data", "ocean_mask.tif")
)

## Load data 
# First, create a tibble with all the filenames
spp_files <- tibble(filepath = list.files(c(file.path(ng_data_path, "02_processed", "species_distributions", "birdlife"),
                                            file.path(ng_data_path, "02_processed", "species_distributions", "aquamaps")),
                                          full.names = T),
                    valid_sci_name = str_replace_all(str_remove(basename(filepath), "\\.tif"), "_", " ")) %>% 
  arrange(valid_sci_name)

# Read-in all the raster files
features_df <- stack(spp_files$filepath) %>%
  raster::as.data.frame(xy = T) %>%                           # Convert to data.frame
  rownames_to_column(var = "cell_id") %>%                     # Use rownames a column (cell_id)
  select(x, y, cell_id, everything()) %>%                     # Order by lon, lat, cell, id then all the speices
  as_tibble()                                                 # Make into a tibble

## Create features matrix
# Create another object that keeps the stuff discarded above
metadata <- features_df %>% 
  select(x, y, cell_id)

# Create a matrix object that only has probabilities of
# occurrence for each species (column) and pixel (row)
features_matrix <- features_df %>% 
  select(-x,-y,-cell_id) %>%                                 # Discard lon lat, and cell_id
  as.matrix() %>%                                            # Convert to matrix
  pmin(1L) %>%                                               # Make sure values are not greater than 1
  pmax(0L)                                                   # Make sure values are not less than 0

rownames(features_matrix) <- metadata$cell_id                # Assign rownames

features_matrix[is.na(features_matrix)] <- 0L    # Replace NAs with 0L

# Export the features matrix, in case we need it
saveRDS(features_matrix,
        file = file.path(project_path, "processed_data", "features_matrix.rds"))

suitability_layer <- tibble(suitability = matrixStats::rowSums2(features_matrix)) %>% 
  rownames_to_column("cell_id") %>% 
  left_join(metadata, by = "cell_id") %>% 
  select(x, y, suitability) %>% 
  mutate(suitability = suitability / max(suitability, na.rm = T)) %>%
  rasterFromXYZ(crs = proj_moll)

suitability_layer <- suitability_layer * ocean_mask

writeRaster(x = suitability_layer,
            filename = file.path(project_path, "processed_data", "suitability.tif"),
            overwrite = TRUE)

# Normalize each species by the sum of all their probabilities
# (so, col / sum(col), in matrix form)
norm_features_matrix <- sweep(
  x = features_matrix,                                     # From the features matrix
  MARGIN = 2,                                              # To each column
  STATS = colSums(features_matrix, na.rm = T),             # Remove the column sum
  FUN = "/")                                               # By dividing

# Export the normalized features matrix, in case we need it
saveRDS(norm_features_matrix,
        file = file.path(project_path, "processed_data", "norm_features_matrix.rds"))

normalized_suitability_layer <- tibble(normalized_suitability = matrixStats::rowSums2(norm_features_matrix),
                                       cell_id = rownames(norm_features_matrix)) %>% 
  left_join(metadata, by = "cell_id") %>% 
  select(x, y, normalized_suitability) %>% 
  mutate(normalized_suitability = normalized_suitability / max(normalized_suitability, na.rm = T)) %>%
  rasterFromXYZ(crs = proj_moll)

normalized_suitability_layer <- normalized_suitability_layer * ocean_mask

plot(normalized_suitability_layer)

writeRaster(x = normalized_suitability_layer,
            filename = file.path(project_path, "processed_data", "normalized_suitability.tif"),
            overwrite = TRUE)

