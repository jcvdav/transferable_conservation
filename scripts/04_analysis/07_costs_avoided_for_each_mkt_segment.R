

library(startR)
library(purrr)
library(tidyverse)


files <- list.files(path = file.path(project_path, "output_data"),
                    pattern = "gains_from_trade*")

data <- file.path(file.path(project_path, "output_data", files)) %>% 
  map_dfr(readRDS) %>% 
  arrange(segments) %>% 
  mutate(market = paste0(str_to_sentence(market), " (", segments, ")")) %>% 
  mutate(market = fct_reorder(market, segments))


segment_gains <- 
  ggplot(data = data, 
         mapping = aes(x = market, y = 1 - percent_of_bau)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Market level (# of segments)",
       y = "% Costs avodided by market approach") +
  ggtheme_plot()

lazy_ggsave(plot = segment_gains,
            filename = "segment_gains",
            width = 10, height = 7.5)
