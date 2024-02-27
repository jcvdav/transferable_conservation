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

# X ----------------------------------------------------------------------------


ggplot(data = data,
       aes(x = bubble, y = ratio, fill = bubble, alpha = type)) + 
  geom_col(position = "dodge")

data %>%
  mutate(ratio = ratio * 100) %>%
  select(bubble, type, ratio) %>%
  pivot_wider(values_from = "ratio", names_from = "type") %>%
  knitr::kable(digits = 3)

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------