################################################################
###################         04_wdi_data       ##################
################################################################
# 
# This script downloads data from the World Bank, and produces
# a table with two key variables:
#  - per capita GDP
#  - total population size
#  
#  I also perform regional-level median imputation for a few
#  (n = 10) countries that are missing data on per capita GDP:
#  
#  - Curacao
#  - Gibraltar
#  - Democratic Peoples Republic of Korea
#  - St. Martin (French Part)
#  - New Caledonia
#  - French POlynesia
#  - Somalia
#  - Sint Maarten (Dutch part)
#  - Sysrian Arab Republic
#  - British Virgin Islands
# 
################################################################

## Set up ##############################################################################################################
# Load packages
library(startR)
library(here)
library(WDI)
library(countrycode)
library(tidyverse)

# Extract indicators from World Bank Database
wdi_indicators <- WDI(indicator = c("NY.GDP.PCAP.KD",                              # GDP per capita (constant 2010 US$)
                                    "SP.POP.TOTL"),                                # Total population size
                      start = 2010,                                                # Data for 2010 only
                      end = 2010) 

## Process ##############################################################################################################
# Clear the data --------------------------------------------------------------------------------------------------------
wdi_indicators_clean <- wdi_indicators %>% 
  rename(per_capita_gdp = NY.GDP.PCAP.KD,                                          # Rename columns
         population = SP.POP.TOTL) %>% 
  mutate(iso3 = countrycode(sourcevar = iso2c,                                     # Convert to iso3 countrycodes
                            origin = "iso2c",
                            destination = "iso3c"),
         region = countrycode(sourcevar = iso3,                                    # Convert to region
                              origin = "iso3c",
                              destination = "region")) %>% 
  drop_na(iso3) %>%                                                                # Remove non-matched regions
  group_by(region) %>% 
  mutate(regional_per_capita_gdp_median = median(per_capita_gdp, na.rm = T)) %>%   # Median for the region
  ungroup() %>% 
  mutate(gdp_impute = is.na(per_capita_gdp),                                       # Imputation dummy
         per_capita_gdp = ifelse(gdp_impute,                                       # Replace NAs
                                 regional_per_capita_gdp_median,
                                 per_capita_gdp)) %>% 
  mutate(per_capita_gdp = per_capita_gdp / 1e3,                                    # Use better units
         population = population / 1e3) %>% 
  select(iso3, country, region, gdp_impute, per_capita_gdp, population)            # Select relevant columns only

# Visualize post-imputation ----------------------------------------------------------------------------------------------
ggplot(data = wdi_indicators_clean,
       mapping = aes(x = population,
                     y = per_capita_gdp,
                     fill = region,
                     size = gdp_impute)) + 
  geom_point(shape = 21, color = "black") +
  scale_fill_brewer(palette = "Set1", direction = -1) +
  scale_x_continuous(trans = "log10") + 
  scale_y_continuous(trans = "log10") +
  ggtheme_plot() +
  labs(fill = "Region",
       size = "Imputed",
       x = "log10(Population)",
       y = "log10(GDP per capita (constant 2010 1000s US$))")

# Export -----------------------------------------------------------------------------------------------------------------
write.csv(x = wdi_indicators_clean,
          file = file.path(project_path, "processed_data", "wdi_indicators_by_country.csv"),
          row.names = F)


# END OF SCRIPT ##########################################################################################################




