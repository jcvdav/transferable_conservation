# Load libraries
library(startR)
library(tidyverse)

# Set up a 10 by 10 matrix
# Each column represents a country

props <- seq(0.1, 1, by = 0.1)
gb_bau <- gb_ran_c <- gb_mkt <- matrix(nrow = 10, ncol = 100)

for (k in 1:100) {
  
  benefit <- t(seq(0.5, 5, by = 0.5) * t(matrix(runif(n = 100, min = 0, max = 10) ^ 2.5, ncol = 10)))
  colnames(benefit) <- LETTERS[1:10]
  
  j <- 1
  for (i in props){
    gb_bau[j, k] <- global_benefits(matrix = benefit, proportion = i, tactic = "bau")
    gb_ran_c[j, k] <- global_benefits(matrix = benefit, proportion = i, tactic = "random_c")
    gb_mkt[j, k] <- global_benefits(matrix = benefit, proportion = i, tactic = "mkt")
    j <- j + 1
  }
  
  
}

gb_bau_df <- tibble(prop = props) %>% 
  cbind(gb_bau) %>% 
  mutate(tactic = "bau")

gb_ran_c_df <- tibble(prop = props) %>% 
  cbind(gb_ran_c) %>% 
  mutate(tactic = "ran_c")

gb_mkt_df <- tibble(prop = props) %>% 
  cbind(gb_mkt) %>% 
  mutate(tactic = "mkt")

rbind(gb_bau_df, gb_ran_c_df, gb_mkt_df) %>% 
  gather(iteration, value, -c(prop, tactic)) %>% 
  ggplot(aes(x = prop, y = value, color = tactic, fill = tactic)) +
  # stat_summary(geom = "ribbon", fun.data = mean_sdl, alpha = 0.25, color = "transparent") +
  stat_summary(geom = "line", fun = "mean") +
  labs(x = "Proportion of habitat conserved",
       y = "Total benefit") +
  ggtheme_plot()


