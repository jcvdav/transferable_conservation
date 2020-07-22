# This script runs all scripts that create descriptive content

library(here)

source(file = here("scripts", "00_setup", "01_setup.R"), local = F)

# CLEANING SCRIPT
source(file = here("scripts", "04_analysis", "01_get_targets.R"), local = F)
source(file = here("scripts", "04_analysis", "02_get_trading_prices.R"), local = F)
source(file = here("scripts", "04_analysis", "03_get_eez_level_conservation.R"), local = F)

