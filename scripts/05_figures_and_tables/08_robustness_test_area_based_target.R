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
  kableExtra,
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
data <- bind_rows(hsi, abt) %>% 
  mutate(bubble = fct_relevel(bubble, "Global (N = 1)",
                              "Hemisphere (N = 4)",
                              "Realm (N = 12)",
                              "Province (N = 60)",
                              "Ecoregion (N = 219)"))
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
  kableExtra::kbl(caption = "Comparison of gains from trade and conservation benefit
for a 30-by-30 target using extent weighted by the habitat suitability index (labeled HSI)
and using area-based targets (labeled ABT). The first column shows the bubble policy,
the second and third shows the gains from trade under each unit. The fourth and fifth
show the conservation benefit (HSI-weighted area in both cases) attained by each scenario.
The asterisks (*) in columns four and five indicate the highest value in each row.",
                  label = "gains_from_trade_30_abt",
                  format = "latex",
                  booktabs = T,
                  linesep = "",
                  digits = c(0, 3, 3, 0, 0),
                  col.names = c("Bubble policy", "HSI", "ABT", "HSI", "ABT")) %>% 
  kableExtra::kable_styling() %>% 
  kableExtra::add_header_above(header = c(" ", "Gains from trade" = 2, "Conservation benefit" = 2))

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------