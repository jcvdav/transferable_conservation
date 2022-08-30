
library(cowplot)
library(tidyverse)

bau <-
  readRDS(
    file = file.path(
      project_path,
      "processed_data",
      "supply_curves",
      "with_mpas",
      "global_eez_supply_curves_with_mpas.rds"
    )) %>% 
  mutate(pixel_fraction = pmin(1 - ((tb - ((tb * 0.3) / pct)) / benefit), 1)) %>% 
  filter(pixel_fraction >= 0) %>% 
  select(lat, lon) %>% 
  mutate(bau = 1)

mkt <-
  readRDS(file = file.path(
    project_path,
    "processed_data",
    "supply_curves",
    "with_mpas",
    "global_supply_curve_with_mpas.rds"
  )) %>% 
  mutate(pixel_fraction = pmin(1 - ((tb - ((tb * 0.3) / pct)) / benefit), 1)) %>% 
  filter(pixel_fraction >= 0) %>% 
  select(lat, lon) %>% 
  mutate(mkt = 1)

base <- readRDS(
  file = file.path(
    project_path,
    "processed_data",
    "supply_curves",
    "with_mpas",
    "global_eez_supply_curves_with_mpas.rds"
  )) %>% 
  select(lat, lon, suitability, area, cost, benefit) %>% 
  left_join(bau, by = c("lat", "lon")) %>% 
  left_join(mkt, by = c("lat", "lon")) %>% 
  replace_na(list("bau" = 0,
                  "mkt" = 0)) %>% 
  mutate(both = mkt + bau,
         protected = case_when(mkt == 1 & bau == 1 ~ "Protected under both",
                               mkt == 0 & bau == 1 ~ "Protected only under BAU",
                               mkt == 1 & bau == 0 ~ "Protected only under MKT",
                               T ~ "Not protected")) %>% 
  mutate(cost = log10(cost)) %>% 
  filter(!protected == "Not protected")

b_bau <- base %>% mutate(a = bau * benefit) %>% pull(a) %>% sum()
b_mkt <- base %>% mutate(a = mkt * benefit) %>% pull(a) %>% sum()
A <- base %>% pull(area) %>% sum()

plot_measure <- function(data, variable, x_lab = "", y_lab, n = F){
  
  
  p <- data %>% 
    ggplot(aes(x = protected, y = {{variable}}, fill = protected)) +
    geom_violin() +
    # stat_summary(geom = "point", fun = "median", fill = "white") +
    # stat_summary(geom = "point", fun = "mean", fill = "white", shape = 24) +
    scale_fill_brewer(palette = "Set2") +
    ggtheme_plot() +
    labs(x = x_lab,
         y = y_lab) +
    theme(legend.position = "None")
  
  if(n){
    N <- data %>% 
      group_by(protected) %>% 
      summarize(n = paste0("N = ", n()),
                y = max({{variable}}))
    
    p <- p + 
      geom_text(data = N, aes(x = protected, y = max(y), label = n))
  }
  
  return(p)
}

cost <- plot_measure(
  data = base,
  variable = cost,
  x_lab = "",
  y_lab = expression(Costs~(log[10]~(M~USD))),
  n = T) +
  scale_x_discrete(labels = NULL)

area <- plot_measure(
  data = base,
  variable = area,
  x_lab = "",
  y_lab = expression(Area~(Km^2))) +
  scale_x_discrete(labels = NULL)

suitability <- plot_measure(
  data = base,
  variable = suitability,
  x_lab = "Pixel status",
  y_lab = "Habitat Suitability\nIndex")

p <- plot_grid(cost, area, suitability, ncol = 1, labels = "AUTO", label_x = 0.9)

lazy_ggsave(plot = p,
            filename = "30_by_segment/sources_of_efficiency",
            width = 15,
            height = 15)
