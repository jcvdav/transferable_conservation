## Set up
# Load packages
library(WDI)
library(countrycode)
library(tidyverse)

# Extract indicators from World Bank Database
wdi_indicators <- WDI(indicator = c("NY.GDP.PCAP.KD",      # GDP per capita (constant 2010 US$)
                                    "SP.POP.TOTL"),        # Total population size
                      start = 2018,                        # Data for 2018 only
                      end = 2018) %>% 
  rename(per_capita_gdp = NY.GDP.PCAP.KD,
         population = SP.POP.TOTL) %>% 
  mutate(iso3 = countrycode(sourcevar = iso2c,             # Convert to iso3 countrycodes
                            origin = "iso2c",
                            destination = "iso3c"))

write.csv(x = wdi_indicators,
          file = here("data", "wdi_indicators.csv"))

a <- raster::raster(here("data", "cumulative_impact_2013.tif"))

### TO DO
# - Export WDI data
# - Get mean CHI for each country / meow intersection
# - Create the following allocations:
#     - 30% of what you have
#     - 30% of total meow / number of countries
#     - 30% of total meow, weighing by GDP
#     - 30% of total meow, weighing by population size
#     - 30% of total meow, weighing by CHI