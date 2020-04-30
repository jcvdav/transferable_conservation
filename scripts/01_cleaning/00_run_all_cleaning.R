# This script runs all cleaning scripts

library(here)

# CLEANING SCRIPT
source(file = here("scripts", "01_cleaning", "01_clean_EEZs.R"), local = F)
source(file = here("scripts", "01_cleaning", "02_clean_MEOWs.R"), local = F)
source(file = here("scripts", "01_cleaning", "03_intersect_eez_and_meow.R"), local = F)
source(file = here("scripts", "01_cleaning", "04_wdi_data_collection_and_cleaning.R"), local = F)
source(file = here("scripts", "01_cleaning", "05_chi_extraction.R"), local = F)

