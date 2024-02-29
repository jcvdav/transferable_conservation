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
  tidyverse
)

# Load data --------------------------------------------------------------------
hsi <- readRDS(file = here("results", "output_data", "gains_from_trade_bubbles.rds")) %>% 
  filter(near(r, 0.3)) %>% 
  mutate(type = "HSI")
abt <- readRDS(file = here("results", "output_data", "gains_from_trade_bubbles_abt.rds")) %>% 
  filter(near(r, 0.3)) %>% 
  mutate(type = "ABT")

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
data <- bind_rows(hsi, abt) 
## VISUALIZE ###################################################################

data %>%
  mutate(ratio = ratio * 100) %>%
  mutate(TB = coalesce(mkt_tb_hsi, mkt_tb)) %>% 
  select(bubble, type, TB, ratio) %>%
  group_by(bubble) %>% 
  mutate(TB = round(TB, digits = 0),
         TB = ifelse(TB == max(TB), paste(TB, "*"), TB)) %>% 
  ungroup() %>% 
  pivot_wider(values_from = c(ratio, TB), names_from = "type") %>%
  kableExtra::kbl(digits = c(0, 3, 3, 0, 0),
                  col.names = c("Bubble policy", "HSI", "ABT", "HSI", "ABT")) %>% 
  kableExtra::kable_styling() %>% 
  kableExtra::add_header_above(header = c(" ", "Gains from trade" = 2, "Conservation benefit" = 2))

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------