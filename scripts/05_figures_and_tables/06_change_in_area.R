######################################################
#title#
######################################################
#
# Purpose
#
######################################################

## SET UP ######################################################################
# Load packages ----------------------------------------------------------------
pacman::p_load(
  startR,
  tidyverse
)

# Load data --------------------------------------------------------------------
get_files <- function(segment) {
  filenames <-
    list.files(
      path = here(
        "results", "output_data", "trade_outcomes", segment),
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
  map_dfr(get_files) %>% 
  filter(r >= 0.1) %>% 
  group_by(r, segment) %>%
  summarize_if(is.numeric, sum, na.rm = T) %>% 
  ungroup() %>% 
  mutate(a = bau_area - mkt_area, # Calculate difference
         a2 = a / bau_area, # Calculate relative difference
         ) %>%
  mutate(segment = str_to_sentence(segment),
    segment = fct_relevel(segment, "Province", "Ecoregion", after = Inf)) 


## PROCESSING ##################################################################


p1 <- ggplot(data = data,
            mapping = aes(x = r, y = a2, color = segment)) +
  geom_rect(xmin = 0, xmax = 0.1, ymin = 0, ymax = Inf, color = "transparent", fill = "gray", alpha = 0.1) +
  geom_vline(xintercept = c(0.1, 0.3, 0.5), linetype = "dashed") +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("#C13832", "#D28E00", "#9ECEEB", "#D4BF95", "#91B9A4")) +
  scale_x_continuous(limits = c(0, 1.01),
                     expand = c(0, 0),
                     labels = scales::percent) +
  scale_y_continuous(expand = c(0, 0),
                     labels = scales::percent) +
  theme(legend.position = c(0.99, 0.99),
        legend.justification = c(1, 1),
        legend.background = element_rect(color = "black", fill = "white", linewidth = 0.1)) +
  guides(color = guide_legend(keyheight = 0.5)) +
  labs(x = "Proteciton target (% of total)",
       y = "Difference (% of BAU)",
       title = "Relative difference [(BAU - MKT) / BAU]",
       color = "Bubble policy")


p2 <- ggplot(data = data,
            mapping = aes(x = r, y = a / 1e3, color = segment)) +
  geom_rect(xmin = 0, xmax = 0.1, ymin = 0, ymax = Inf, color = "transparent", fill = "gray", alpha = 0.1) +
  geom_vline(xintercept = c(0.1, 0.3, 0.5), linetype = "dashed") +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("#C13832", "#D28E00", "#9ECEEB", "#D4BF95", "#91B9A4")) +
  scale_x_continuous(limits = c(0, 1.01),
                     expand = c(0, 0),
                     labels = scales::percent) +
  theme(legend.position = "None") +
  labs(x = "Proteciton target (% of total)",
       y = expression("Absolute difference (thousands of km"^2~")"),
       title = "Difference (BAU - MKT)")

p <- cowplot::plot_grid(p1,
                        p2,
                        ncol = 1,
                        align = "hv")


lazy_ggsave(plot = p,
            file = "change_in_area",
            width = 8,
            height = 10)
