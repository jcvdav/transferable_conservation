# RUN ALL SCRIPT

library(here)
source(here("scripts", "01_cleaning", "00_run_all_cleaning.R"))
source(here("scripts", "02_processing", "00_run_all_processing.R"))
source(here("scripts", "03_descriptive_content", "00_run_all_descriptive.R"))
source(here("scripts", "04_analysis", "00_run_all_analysis.R"))
