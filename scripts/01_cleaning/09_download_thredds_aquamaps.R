######################################################
#title#
######################################################
# 
# Purpose
#
######################################################

## SET UP ######################################################################
# Load packages
library(here)
library(raster)
library(furrr)
library(magrittr)
library(tidyverse)

# Load species list
thredds_spp_list <-
  read_csv(
    file = here("raw_data", "aquamaps_thredds_species_list.csv"),               # Catalog with list of species found at: https://thredds.d4science.org/thredds/catalog/public/netcdf/AquaMaps_11_2019/catalog.html
    col_types = cols("character")) %>% 
  filter(!str_detect(species, "Echinodermata_Echinoidea"))

# Create a safe function to call
safe_raster <- possibly(.f = raster, otherwise = NULL)

## PROCESSING ##################################################################
# Define the strategy
plan("multisession")

# Download the data
rasters <- thredds_spp_list %>%
  mutate(species = str_trim(species),
         path = paste0("https://thredds.d4science.org/thredds/dodsC/public/netcdf/AquaMaps_11_2019/", species),
         species = tools::file_path_sans_ext(species)) %>%
  mutate(r = future_map(path, safe_raster)) %>% 
  filter(!map_lgl(r, is.null))

print("data have been downloaded")

# Creata a data.frame
df <- rasters %>%
  mutate(d = future_map(r,
                        as.data.frame,
                        xy = T,
                        na.rm = T,
                        .options = furrr_options(seed = T))) %>%
  select(species, d) %>%
  unnest(d)

# Export the data.frame with all the data
saveRDS(object = df,
        file = here("raw_data", "thredds_aquamaps_sdm.rds"))
print("data have been exported")


safe_write <- possibly(writeRaster, otherwise = NULL)

print("ready to export rasters")

# Export all the rasters
rasters %>% 
  mutate(filename = here("raw_data", "thredds_aquamaps_rasters", species)) %$% 
  future_walk2(
    r,
    filename,
    safe_write,
    format = "GTiff",
    overwrite = F,
    .options = furrr_options(seed = T))


