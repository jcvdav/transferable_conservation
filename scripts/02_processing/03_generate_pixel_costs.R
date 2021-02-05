

# Load packages
library(raster)
library(data.table)
library(tidyverse)

# Read in catch data
## List the files
ind <- list.files(path = file.path(rw_path, "industrial"),
                          pattern = "*.csv",
                          full.names = T) %>% 
  tail(2)

Nind <- list.files(path = file.path(rw_path, "nonindustrial"),
                    pattern = "*.csv",
                    full.names = T) %>% 
  tail(2)


catch_files <- c(ind, Nind)

# Read all
dt <- map_dfr(catch_files, fread, sep = ",")
setkey(dt , Cell, ID)

# Read in the codes
cell_codes <- readxl::read_excel(file.path(rw_path, "Codes.xlsx"), sheet = 1L) %>% 
  as.data.table(key = "Cell")

taxa_codes <- readRDS(file.path(project_path, "processed_data", "taxa_codes_and_prices.rds")) %>% 
  data.table::data.table(key = "taxon_key")

# Read indices
index <- list.files(rw_path, "*.csv", full.names = T) %>% 
  map_dfr(fread, key = "ID") %>% 
  .[, ":=" (Year = IYear, TaxonKey = Taxonkey)] %>%
  .[Year >= 2010, .(ID, Year, TaxonKey)]
  

merged <- dt %>% 
  merge(index, all.x = TRUE, by = "ID") %>% 
  merge(taxa_codes, all.x = TRUE, by.x = "TaxonKey", by.y = "taxon_key") %>% 
  merge(cell_codes, all.x = TRUE, by = "Cell")

setkey(merged, Year, Cell)

summary <- merged %>% 
  .[, .(Reported = sum(Reported, na.rm = T),
        Revenue = sum(Reported * price, na.rm = T)),
    by = .(Year, Cell, LatCentre, LonCentre)] %>%
  .[,.(Reported = mean(Reported, na.rm = T),
       Revenue = mean(Revenue, na.rm = T)),
    by = .(Cell, LatCentre, LonCentre)]

benefits_raster <-
  raster(
    file.path(
      project_path,
      "processed_data",
      "suitability.tif"
    )
  )

crs(benefits_raster) <- proj_moll
  
watson_raster_longlat <- select(summary, LonCentre, LatCentre, Revenue) %>%
  raster::rasterFromXYZ(crs = proj_longlat)

total_longlat <- sum(values(watson_raster_longlat), na.rm = T)

watson_raster_moll <- watson_raster_longlat %>% 
  raster::projectRaster(crs = proj_moll, res = 50000, over = T)

total_moll <- sum(values(watson_raster_moll), na.rm = T)

watson_raster_moll <- (watson_raster_moll / total_moll) * total_longlat

back <- projectRaster(watson_raster_moll, crs = proj_longlat, res = 0.5)

sum(raster::values(watson_raster_longlat), na.rm = T)
sum(raster::values(watson_raster_moll), na.rm = T)

writeRaster(watson_raster_moll,
            filename = file.path(project_path, "processed_data", "revenue_raster.tif"),
            overwrite = TRUE)

watson_raster_moll %>% 
  as.data.frame(xy = T) %>% 
  ggplot(aes(x = x, y = y, fill = Revenue)) +
  geom_tile() +
  scale_fill_viridis_c(trans = "log10", na.value = 1e4) +
  coord_equal()



######## Tyler's price dataset

  # left_join(price_asfis, by = c("TaxonName" = "ASFIS_species")) %>% 
  # left_join(price_asfis, by = c("CommonName" = "ASFIS_species")) %>% 
  # # left_join(price_pool_com) %>% 
  # left_join(price_isscaap_group, by = c("TaxonName" = "ISSCAAP_group")) %>%
  # left_join(price_isscaap_group, by = c("CommonName" = "ISSCAAP_group")) %>%
  # left_join(price_isscaap_division, by = c("CommonName" = "ISSCAAP_division")) 

# Extra stuff
# 
# 
# my_validate <- function(species_list, server){
#   browser()
#   tmp <- data.frame(input = species_list, stringsAsFactors = FALSE)
#   
#   synonyms(species_list, server = server) %>% 
#     select(input = synonym, Status, Species) %>% 
#     distinct() %>% 
#     filter(Status %in% c("accepted name", "synonym")) %>% 
#     spread(Status, Species) %>% 
#     rename(accepted_name = `accepted name`) %>% 
#     mutate(Species = ifelse(is.na(accepted_name), synonym, accepted_name)) %>% 
#     right_join(tmp, by = "input") %>% 
#     pull(Species)
#   
# }
# 
# my_fun <- function(sci_name1, sci_name2, sci_name3){
#   sci_name <- sci_name3
#   sci_name[is.na(sci_name)] <- sci_name2[is.na(sci_name)]
#   sci_name[is.na(sci_name)] <- sci_name1[is.na(sci_name)]
#   return(sci_name)
# }

