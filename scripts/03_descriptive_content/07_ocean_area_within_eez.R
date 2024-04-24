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
  here,
  terra
)

# Load data --------------------------------------------------------------------
ocean <- rast("clean_data/ocean_mask.tif")
eez <- !is.na(rast("clean_data/eez_raster.tif"))
area <- terra::cellSize(ocean)

# Plot data
plot(ocean)
plot(eez)
plot(area)

# Mask area by ocean and eez
ocean_area <- area * ocean
eez_area <- area * eez

# Plot area
plot(ocean_area)
plot(eez_area)

# Calculate total area of each
tot_ocean <- sum(values(ocean_area), na.rm = T)
tot_eez <- sum(values(eez_area), na.rm = T)

# % of Oceans within EEZ
(tot_eez / tot_ocean) * 100

# A check "3/4ths of the world is water")
(sum(values(ocean_area), na.rm = T) / sum(values(area), na.rm = T)) * 100

