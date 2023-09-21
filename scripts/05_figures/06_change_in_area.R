######################################################
#title#
######################################################
#
# Purpose
#
######################################################

## SET UP ######################################################################
# Load packages
library(startR)
library(tidyverse)

# Load data


get_files <- function(segment) {
  filenames <-
    list.files(
      path = file.path(
        project_path, "output_data", "trade_outcomes", segment),
      pattern = "csv",
      full.names = T)
  
  data <- tibble(filename = filenames,
                 r = basename(filename)) %>% 
    mutate(data = map(filename, data.table::fread),
           r = as.numeric(str_extract(r, "[:digit:]\\.[:digit:]{2}"))) %>% 
    unnest(data) %>% 
    mutate(segment = segment)
  
  return(data)
}

data <- c("global", "hemisphere", "realm", "province", "ecoregion") %>% 
  map_dfr(get_files)


## PROCESSING ##################################################################


p <- data %>% 
  filter(r >= 0.1) %>% 
  group_by(r, segment) %>%
  summarize_if(is.numeric, sum, na.rm = T) %>% 
  ungroup() %>% 
  mutate(tb = bau_tb - mkt_tb,
         tc = bau_tc - mkt_tc,
         a = bau_area - mkt_area,
         a2 = a / bau_area) %>%
  select(r, segment, a, a2) %>% 
  # pivot_longer(cols = c(a, a2), names_to = "variable", values_to = "value") %>% 
  mutate(#variable = case_when(variable == "tc" ~ "BAU Cost - MKT Cost (M USD)",
                              # variable == "tc2" ~ "(BAU Cost - MKT Cost) / BAU Costs (%)",
                              # variable == "a" ~ "BAU Area - MKT Area (KM2^)",
                              # variable == "a2" ~ "(BAU Area - MKT Area) / BAU Area (%)"),
         segment = str_to_sentence(segment),
         segment = fct_relevel(segment, "Province", "Ecoregion", after = Inf)) %>% 
  ggplot(aes(x = r, y = a2, color = segment)) +
  geom_rect(xmin = 0, xmax = 0.1, ymin = 0, ymax = Inf, color = "transparent", fill = "gray", alpha = 0.1) +
  geom_vline(xintercept = c(0.1, 0.3, 0.5), linetype = "dashed") +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("#C13832", "#D28E00", "#9ECEEB", "#D4BF95", "#91B9A4")) +
  scale_x_continuous(limits = c(0, 1.01),
                     expand = c(0, 0),
                     labels = scales::percent) +
  scale_y_continuous(limits = c(0, 0.06),
                     expand = c(0, 0),
                     labels = scales::percent) +
  theme(legend.position = c(1, 1),
        legend.justification = c(1, 1),
        legend.background = element_blank()) +
  labs(x = "% Protected",
       y = "Difference",
       color = "Segment")

lazy_ggsave(plot = p,
            file = "change_in_area",
            width = 12,
            height = 7.5)
