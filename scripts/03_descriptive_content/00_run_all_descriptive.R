# This script runs all scripts that create descriptive content

library(here)

source(file = here("scripts", "00_setup", "01_setup.R"), local = F)

# CLEANING SCRIPT
source(file = here("scripts", "03_descriptive_content", "01_plot_supply_curves.R"), local = F)
source(file = here("scripts", "03_descriptive_content", "02_plot_trading_units.R"), local = F)

