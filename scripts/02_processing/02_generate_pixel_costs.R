

# Load packages
library(raster)
library(data.table)
library(tidyverse)

# Create common pointer to RegWatson data
rw_path <- file.path(data_path, "reg-watson-global-marine-capture", "global_fisheries_landing_v4")

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

taxa_codes <- readxl::read_excel(file.path(rw_path, "Codes.xlsx"), sheet = 3L) %>% 
  select(TaxonKey, TaxonName) %>% 
  mutate(price = 1:nrow(.))

# Read indices
index <- list.files(rw_path, "*.csv", full.names = T) %>% 
  map_dfr(fread, key = "ID") %>% 
  .[, ":=" (Year = IYear, TaxonKey = Taxonkey)] %>%
  .[Year >= 2010, .(ID, Year, TaxonKey)]
  

merged <- dt %>% 
  merge(index, all.x = TRUE, by = "ID") %>% 
  merge(taxa_codes, all.x = TRUE, by = "TaxonKey") %>% 
  merge(cell_codes, all.x = TRUE, by = "Cell")

setkey(merged, Year, Cell)

summary <- merged %>% 
  .[!is.na(price)] %>% 
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
  
watson_raster <- select(summary, LonCentre, LatCentre, Reported) %>%
  raster::rasterFromXYZ(crs = proj_longlat) %>% 
  raster::projectRaster(benefits_raster)

sum(raster::values(watson_raster), na.rm = T)

raster::plot(watson_raster)

writeRaster(watson_raster,
            filename = file.path(project_path, "processed_data", "catch_raster.tif"),
            overwrite = TRUE)

summary %>% 
  sample_n(5e4) %>% 
  ggplot(aes(x = LonCentre, y = LatCentre, fill = Reported)) +
  geom_tile() +
  scale_fill_viridis_c(trans = "log10") +
  theme(text = element_text(family = "Times", face = "bold", color = "red", size = 24), axis.text.x = element_text(color = "blue"))














