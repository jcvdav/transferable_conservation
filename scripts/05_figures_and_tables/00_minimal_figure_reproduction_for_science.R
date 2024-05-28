################################################################################
# Minimal Figure Reproduction for Science's Design Team
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu | juancarlos@ucsb.edu
# Code written on May 2, 2024
#
# Description
# The design team at Science has requested the raw data so that they can
# reproduce Figure1 B-C (which will end up being 1A and 1B). This script
# produces a CSV with the information they requested. It also builds a minimal
# reproduction of each figure using ggplot
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(here, tidyverse)

# Preferred color palette
Global = "#C13832"
Hemisphere = "#D28E00"
Realm =  "#9ECEEB"
Province =  "#D4BF95"
Ecoregion = "#91B9A4"

# Load data --------------------------------------------------------------------
# The chunk below is commented, but left here for explanatory purposes. It is
# the code used to read the entire file, selet only the relevant columns,
# rename them as needed, and then exported
readRDS(here("results", "output_data", "gains_from_trade_bubbles.rds")) %>%
  select(bubble_policy = bubble,                                                # A character vector specifying the bubble policy used.
         conservation_ratio = r,                                                # The relative conservation goal, between 0.1 and 1 (i.e. 10 and 100%)
         relative_gains_from_trade =  ratio                                     # The relative gains form trade (used for the y-axis)
         ) %>%
  write_csv(file = here("results", "output_data", "data_for_science_figures.csv"))


plot_data <- read_csv(file = here("results", "output_data", "data_for_science_figures.csv")) %>%
  mutate(
    # Organize bubble policies by number of bubbles in each
    bubble_policy = fct_relevel(bubble_policy,
                                "Global (N = 1)",
                                "Hemisphere (N = 4)",
                                "Realm (N = 12)",
                                "Province (N = 60)",
                                "Ecoregion (N = 219)"
    )
  )

## VISUALIZE ###################################################################
# The following lines show the minimal recretation of the figures, simply to
# showcase how each column in plot_data is mapped onto the x-axis, y-axis, or 
# the fill/color used

# 30x30 column chart -----------------------------------------------------------
# With base plot
barplot(formula = relative_gains_from_trade ~ bubble_policy,                    # formula method shows Y ~ X
        data = plot_data,                                                       # Source of data
        subset = conservation_ratio == 0.3)                                     # Subset to show 30X30

# With ggplot2
ggplot(data = plot_data[plot_data$conservation_ratio == 0.3,],                  # Keep only data pertitent to 30x30 target (i.e. r == 0.3). Should be 5 observations, one per bubbl epolicy
       mapping = aes(x = bubble_policy,                                         # X is the bubble policy
                     y = relative_gains_from_trade,                             # Y re the gains from thrade
                     fill = bubble_policy)) +                                   # The fill of each column is given by the bubble policy
  geom_col() # Use columns to represent the data

# Multiple targets line chart --------------------------------------------------
# With base plot
plot(x = plot_data$conservation_ratio,                                          # X is the conservation target
     y = plot_data$relative_gains_from_trade,                                   # Y are the gains from trade
     col = plot_data$bubble_policy)                                             # The color of each line is given by the bubble policy
abline(v = c(0.1, 0.3, 0.5))                                                    # Add vertical lines at 10, 30, and 50%

# With ggplot2
ggplot(data = plot_data,
       mapping = aes(x = conservation_ratio,                                    # X is the conservation target
                     y = relative_gains_from_trade,                             # Y are the gains from trade
                     color = bubble_policy)) +                                  # The color of each line is given by the bubble policy
  geom_vline(xintercept = c(0.1, 0.3, 0.5)) +                                   # Add vertical lines at 10, 30, and 50%
  geom_line()                                                                   # Use lines to represent the data

