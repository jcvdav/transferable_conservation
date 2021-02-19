

# Load packages
library(raster)
library(data.table)
library(tidyverse)

# Read in catch data
## List the files
ind <- list.files(path = file.path(rw_path, "industrial"),
                          pattern = "*.csv",
                          full.names = T) %>% 
  tail(3)

Nind <- list.files(path = file.path(rw_path, "nonindustrial"),
                    pattern = "*.csv",
                    full.names = T) %>% 
  tail(3)


catch_files <- c(ind, Nind)

# Read all
dt <- map_dfr(catch_files, fread, sep = ",", key = c("Cell", "ID")) %>% 
  .[, .(ID, Cell, Reported)]

# Read in the codes
cell_codes <- readxl::read_excel(file.path(rw_path, "Codes.xlsx"), sheet = 1L) %>% 
  select(-OceanAreasqkm) %>% 
  as.data.table(key = "Cell")

taxa_codes <- readRDS(file.path(project_path, "processed_data", "taxa_codes_and_prices.rds")) %>% 
  select(year, taxon_key, price) %>% 
  data.table::data.table(key = c("taxon_key", "year"))

# Read indices
index <- list.files(rw_path, "*.csv", full.names = T) %>% 
  map_dfr(fread, key = "ID") %>% 
  .[, .(ID, year = IYear, taxon_key = Taxonkey)]


merged <- dt %>% 
  merge(index, all.x = TRUE, by = "ID") %>%
  merge(taxa_codes, all.x = TRUE, by = c("taxon_key", "year"))

setkey(merged, year, Cell)

summary <- merged %>% 
  .[, .(revenue = sum(Reported * price, na.rm = T)),
    by = .(year, Cell)] %>%
  .[,.(revenue = mean(revenue, na.rm = T)),
    by = .(Cell)] %>% 
  merge(cell_codes, all.x = TRUE, by = "Cell")

benefits_raster <-
  raster(
    file.path(
      project_path,
      "processed_data",
      "suitability.tif"
    )
  )

crs(benefits_raster) <- proj_moll
  
watson_raster_longlat <- select(summary, LonCentre, LatCentre, revenue) %>%
  raster::rasterFromXYZ(crs = proj_longlat)

total_longlat <- sum(values(watson_raster_longlat), na.rm = T)

watson_raster_moll <- watson_raster_longlat %>% 
  raster::projectRaster(crs = proj_moll, res = 50000, over = T)

total_moll <- sum(values(watson_raster_moll), na.rm = T)

watson_raster_moll <- (watson_raster_moll / total_moll) * total_longlat


writeRaster(watson_raster_moll,
            filename = file.path(project_path, "processed_data", "revenue_raster.tif"),
            overwrite = TRUE)


