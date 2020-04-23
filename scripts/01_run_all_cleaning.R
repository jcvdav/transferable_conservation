#script to run all cleaning scripts

library(here)

source(file = here("scripts", "01_cleaning", "01_clean_EEZs.R"), local = F)
source(file = here("scripts", "01_cleaning", "02_clean_MEOWs.R"), local = F)
source(file = here("scripts", "01_cleaning", "03_intersect_eez_and_meow.R"), local = F)

