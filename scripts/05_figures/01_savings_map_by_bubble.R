

library(startR)
library(rnaturalearth)
library(rmapshaper)
library(sf)
library(ggnewscale)
library(tidyverse)

coast <- ne_countries(returnclass = "sf")

r_target <- "0.30"

file <- list.files(path = file.path(project_path,
                                    "output_data",
                                    "trade_outcomes",
                                    "global"),
                   pattern = r_target,
                   full.names = T)

data <- read.csv(file) %>% 
  mutate(ratio = savings / bau_tc)


eez_meow <- st_read(file.path(project_path, "processed_data", "intersected_eez_and_meow.gpkg")) %>% 
  rmapshaper::ms_simplify(keep_shapes = T, sys = T) %>% 
  sf::st_make_valid() %>% 
  group_by(iso3) %>% 
  summarize(a = 1) %>% 
  ungroup() %>% 
  select(-a)

eez_with_results <- eez_meow %>% 
  left_join(data, by = c("iso3"))

sellers <- eez_with_results %>% 
  filter(transaction == "Sellers") %>% 
  mutate(Sellers = pmin(ratio, 1))

buyers <- eez_with_results %>% 
  filter(transaction == "Buyers") %>% 
  mutate(Buyers = ratio)

dont_participate <- eez_with_results %>%
  filter(transaction == "Doesn't participate")

my_scale <- function(x, accuracy = NULL, scale = 100, prefix = "", suffix = "%", 
                     big.mark = " ", decimal.mark = ".", trim = TRUE, ...){
  
  prefix <- character()
  prefix[!x == 1] <- ""
  prefix[x == 1] <- ">="
    
  scales::number(x = x, accuracy = accuracy, scale = scale, prefix = prefix, 
                 suffix = suffix, big.mark = big.mark, decimal.mark = decimal.mark, 
                 trim = trim, ...)
}

savings_map <- ggplot() +
  geom_sf(data = coast) +
  geom_sf(data = buyers, aes(fill = Buyers)) +
  scale_fill_gradient(low = "white", high = "steelblue", labels = my_scale) +
  guides(fill = guide_legend(title = "Costs avoided\n by buyers (%BAU)",
                             frame.colour = "black",
                             ticks.colour = "black")) +
  new_scale_fill() +
  geom_sf(data = sellers, aes(fill = Sellers)) +
  scale_fill_gradient(low = "white", high = "red", labels = my_scale) +
  guides(fill = guide_legend(title = "Gains from trade\n by sellers (%BAU)",
                             frame.colour = "black",
                             ticks.colour = "black"))  +
  geom_sf(data = dont_participate, fill = "gray") +
  ggtheme_map()


lazy_ggsave(plot = savings_map,
            filename = "30_by_segment/savings_map",
            width = 18.7, height = 9)

