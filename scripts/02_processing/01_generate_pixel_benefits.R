######################################################
#title#
######################################################
# 
# Purpose
#
######################################################

## SET UP ######################################################################
# Load packages
library(raster)
library(data.table)
library(janitor)
library(sf)
library(tidyverse)

# Load data

# Read in AquaMaps list of species (this is the list for which AM has available data)
# Tidy it up to match the same format as our list of species of interest
aquamaps_list <-
  read_csv(
    file.path(data_path, "aquamaps-v10-2019", "speciesoccursum.csv"),
    col_types = cols(
      SpeciesID = col_character(),
      SpecCode = col_double(),
      Genus = col_character(),
      Species = col_character(),
      FBname = col_character(),
      OccurCells = col_double(),
      Kingdom = col_character(),
      Phylum = col_character(),
      Class = col_character(),
      Order = col_character(),
      Family = col_character()
    )
  ) %>%
  clean_names() %>%
  filter(occur_cells >= 10) %>%
  mutate(sci_name = paste(genus, species)) %>%
  select(family, genus, sci_name, species_id)

## PROCESSING ##################################################################
# Create a vector of unique species
spec_codes <- aquamaps_list %>%
  pull(species_id) %>%
  unique()

# length(spec_codes) # Number of species contained in our analysis

# Load all aquamaps data
suitability_df <-
  fread(
    file = file.path(
      data_path, "aquamaps-v10-2019", "hcaf_species_native.csv"
      )
    ) %>%
  clean_names() %>%                                                             # Clean column names
  .[species_id %in% spec_codes] %>%                                             # Keep only species with 10 or more records
  .[probability >= 0.5] %>%                                                     # Keep only probability of existing
  as_tibble() %>%                                                               # Convert to a tibble
  group_by(center_long, center_lat) %>%                                         # Group by pixel
  summarize(probability = mean(probability, na.rm = T))                         # Calculate the mean probability of each pixel


# Rasterize suitability
suitability <-
  rasterFromXYZ(
    xyz = suitability_df,                                                       # XWY source
    crs = proj_longlat)                                                         # Coordiante reference system

## EXPORT ######################################################################
writeRaster(
  x = suitability,
  filename = file.path(
    project_path, "processed_data", "suitability.tif"
    ),
  overwrite = TRUE
)