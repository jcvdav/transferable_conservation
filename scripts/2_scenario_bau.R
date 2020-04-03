
# Set up a 10 by 10 matrix
# Each column represents a country

props <- seq(0.1, 1, by = 0.1)
gb <- matrix(nrow = 10, ncol = 100)

for (k in 1:100) {
  
  benefit <- matrix(runif(n = 100, min = 0, max = 10), ncol = 10)
  colnames(benefit) <- LETTERS[1:10]
  
  j <- 1
  for (i in props){
    gb[j, k] <- global_benefits(benefit, proportion = i)
    j <- j + 1
  }
  
  
}

colnames(gb) <- paste0("iter", 1:100)

tibble(prop = props) %>% 
  cbind(gb) %>% 
  gather(iteration, value, -prop) %>% 
  ggplot(aes(x = prop, y = value)) +
  geom_point() +
  stat_summary(geom = "ribbon", fun.data = mean_sdl, alpha = 0.5) +
  stat_summary(geom = "line", fun = "mean")


