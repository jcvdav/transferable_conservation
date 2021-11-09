


## Set up

# Load packages
library(raster)
library(data.table)
library(tidyverse)

# Load ocean mask to remove rogue pixels
ocean_mask <- raster(file.path(project_path, "processed_data", "ocean_mask.tif"))

# Read in catch data
# List all catch files for industrial fisheries
ind <- list.files(
  path = file.path(rw_path, "industrial"),
  pattern = "*.csv",
  full.names = T
)

# List all non-industrial fisheries
Nind <- list.files(
  path = file.path(rw_path, "nonindustrial"),
  pattern = "*.csv",
  full.names = T
)


catch_files <- c(ind, Nind)                                                     # Combine all file paths

catch_files <-
  catch_files[str_detect(catch_files, "2005|2010|2015")]                        # Keep only file names for 2005-2015

# Read all
dt <-
  map_dfr(catch_files,
          fread,
          sep = ",",
          key = c("Cell", "ID")) %>%                                            # Map across files to read them in
  .[, .(ID, Cell, Reported)]

# Read in the codes
cell_codes <-
  readxl::read_excel(file.path(rw_path, "Codes.xlsx"), sheet = 1L) %>%          # Read in the codes
  as.data.table(key = "Cell")                                                   # Convert to data.table; set key to Cell

taxa_codes <- readRDS(file.path(project_path, "processed_data", "taxa_codes_and_prices.rds")) %>%                     # Read in tax condes with prices
  select(year, taxon_key, price) %>%                                                         # Keep only relevnt variables
  data.table::data.table(key = c("taxon_key", "year"))                                       # Convert to data.table; set key to taxon and year

# Read indices
index <- list.files(rw_path, "*.csv", full.names = T) %>%
  map_dfr(fread, key = "ID") %>%
  .[, .(ID, year = IYear, taxon_key = Taxonkey)]

# Merge the three datasets
merged <-
  dt %>%                                                     # This has catch data at the pixel- species- year-level, coded in ID
  merge(index, all.x = TRUE, by = "ID") %>%                          # This adds taxonkey and year
  merge(taxa_codes,
        all.x = TRUE,
        by = c("taxon_key", "year"))       # This adds

setkey(merged, year, Cell)                                           # Sets a key to make computation faster

# Summarize the data
summary <- merged %>%
  .[, .(revenue = sum(Reported * price, na.rm = T)),                # Calculate revenue as the sum product of catch and price
    by = .(year, Cell)] %>%                                         # Grouping it at the cell-year level
  .[, .(revenue = median(revenue, na.rm = T)),                       # Now calculate the median revenue
    by = .(Cell)] %>%                                               # Grouping it at the cell-level
  merge(cell_codes, all.x = TRUE, by = "Cell")                      # Finally, add coordinates to the data

# Rasterization
watson_raster <- summary %>%
  select(LonCentre, LatCentre, revenue) %>%                               # Select coordinates and variable
  raster::rasterFromXYZ(crs = proj_longlat)                               # Rasterize the revenue data into longlat coords

# Export data
writeRaster(
  watson_raster,
  filename = file.path(project_path, "processed_data", "revenue_raster.tif"),
  overwrite = TRUE
)
