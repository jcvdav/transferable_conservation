# Load libraries
library(tidyverse)

# Set up a 10 by 10 matrix
# Each column represents a country

props <- seq(0.1, 1, by = 0.1)
gb_bau <- gb_ran_c <- gb_ran_g <- gb_mkt <- matrix(nrow = 10, ncol = 100)

for (k in 1:100) {
  
  benefit <- matrix(runif(n = 100, min = 0, max = 10), ncol = 10)
  colnames(benefit) <- LETTERS[1:10]
  
  j <- 1
  for (i in props){
    gb_bau[j, k] <- global_benefits(matrix = benefit, proportion = i, tactic = "bau")
    gb_ran_c[j, k] <- global_benefits(matrix = benefit, proportion = i, tactic = "random_c")
    gb_ran_g[j, k] <- global_benefits(matrix = benefit, proportion = i, tactic = "random_g")
    gb_mkt[j, k] <- global_benefits(matrix = benefit, proportion = i, tactic = "mkt")
    j <- j + 1
  }
  
  
}

gb_bau_df <- tibble(prop = props) %>% 
  cbind(gb_bau) %>% 
  mutate(tactic = "bau")

gb_ran_c <- tibble(prop = props) %>% 
  cbind(gb_ran_c) %>% 
  mutate(tactic = "ran_c")

gb_ran_g <- tibble(prop = props) %>% 
  cbind(gb_ran_g) %>% 
  mutate(tactic = "ran_g")

gb_mkt <- tibble(prop = props) %>% 
  cbind(gb_mkt) %>% 
  mutate(tactic = "mkt")

rbind(gb_bau_df, gb_ran_c, gb_ran_g, gb_mkt) %>% 
  gather(iteration, value, -c(prop, tactic)) %>% 
  ggplot(aes(x = prop, y = value, color = tactic)) +
  geom_point() +
  stat_summary(geom = "ribbon", fun.data = mean_sdl, alpha = 0.5) +
  stat_summary(geom = "line", fun = "mean")


