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
  mutate(type = "ABT",
         dif = mkt_tb_hsi - bau_tb_hsi,
         pct = (dif / bau_tb_hsi) * 100)

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
  # mutate(TB = coalesce(mkt_tb_hsi, mkt_tb)) %>% 
  select(bubble, type,
         # TB,
         pct,
         ratio) %>%
  pivot_wider(values_from = c(ratio, pct), 
              names_from = "type") %>%
  arrange(bubble) %>% 
  # mutate(TB_pct = ((TB_ABT - TB_HSI) / TB_HSI) * 100) %>% 
  select(bubble, contains("ratio"), pct_ABT) %>% 
  kableExtra::kbl(caption = "Comparison of gains from trade 
  for a 30-by-30 target using extent weighted by the habitat suitability index (labeled HSI)
  and using area-based targets (labeled ABT). The first column shows the bubble policy,
  the second and third columns show the gains from trade under each measure. The fourth
  column shows the change in total conservation benefits (HSI-weighted area), relative to benefits
  under area-based conservation and no trade.",
                  label = "gains_from_trade_30_abt",
                  format = "latex",
                  booktabs = T,
                  linesep = "",
                  digits = c(0, 3, 3, 3),
                  col.names = c("Bubble policy", "HSI", "ABT", "Change in TB (%)")) %>% 
  kableExtra::kable_styling() %>% 
  kableExtra::add_header_above(header = c(" ", "Gains from trade" = 2, " " = 1))

# EXPORT ######################################################################

# X ----------------------------------------------------------------------------