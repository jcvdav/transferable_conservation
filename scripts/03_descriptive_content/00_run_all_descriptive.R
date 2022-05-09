# This script runs all scripts that create descriptive content

library(here)
library(tidyverse)
library(furrr)

source(file = here("scripts", "00_setup", "01_setup.R"), local = F)

list.files(here("scripts", "03_descriptive_content"), pattern = "*.R", full.names = T) %>% 
  tail(-1) %>% 
  walk(source, local = F)


