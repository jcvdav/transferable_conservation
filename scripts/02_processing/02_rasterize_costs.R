#  rasterize_rens_crap

## SETUP #############################################################################
# Load packages
library(startR)
library(raster)
library(tidyverse)

# Load data
# Benefits raster as a reference raster
benefits_raster <- raster(
  file.path(
    ng_data_path,
    "03_output/05_spp_wts_smts_provs_MPAs/ranking_raster.tif"
  )
)

## PROCESSING #######################################################################
# Costs raster
costs_raster <-
  readRDS(file = file.path(project_path, "raw_data", "Fig1_PixelLeveDH_raw.rds")) %>% 
  rasterFromXYZ(crs = proj_moll) %>%                                                   # Rasterize
  projectRaster(benefits_raster)                                                       # Reproject to fit resolution

# Export raster #####################################################################
writeRaster(x = costs_raster,
            filename = file.path(
              project_path,
              "processed_data",
              "costs_raster.tif"),
            overwrite = T
)


# END OF SCRIPT