# This script runs all processing scripts

library(here)

source(file = here("scripts", "00_setup.R"), local = F)

# CLEANING SCRIPT
source(file = here("scripts", "02_processing", "01_rasterize_eez_and_meow.R"), local = F)
source(file = here("scripts", "02_processing", "02_get_costs_and_benefits.R"), local = F)

