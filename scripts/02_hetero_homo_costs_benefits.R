# Load libraries
library(startR)
library(tidyverse)

# Set up a 10 by 10 matrix
# Each column represents a country

props <- seq(0.1, 1, by = 0.1)
mkt_be_ce <- mkt_be_co <- mkt_bo_ce <- mkt_bo_co <- list()

ppc <- 100
n_countries <- 10

mat_mult <- matrix(seq(0.5, 5, length.out = n_countries), ncol = n_countries, nrow = ppc, byrow = T)

set.seed(43)
# Benefits
ben_het <- mat_mult * matrix(rnorm(n = ppc * n_countries, mean = 50,sd = 10) ^ 2.5, ncol = n_countries)
ben_hom <- matrix(mean(ben_het), ncol = n_countries, nrow = ppc)

image(ben_het)

# Costs
cost_het <- -1 * mat_mult * matrix(rnorm(n = ppc * n_countries, mean = 50, sd = 10) ^ 2.5, ncol = n_countries)
cost_hom <- matrix(mean(cost_het), ncol = n_countries, nrow = ppc)

image(cost_het)

world <- tibble(world = c("be_ce", "be_co", "bo_ce", "bo_co"),
                benefit_matrix = list(ben_het, ben_het, ben_hom, ben_hom),
                cost_matrix = list(cost_het, cost_hom, cost_het, cost_hom))

results <- expand_grid(prop = props, 
                       tactic = c("mkt", "bau", "random_c"),
                       type = c("max_benefits", "min_costs"),
                       world = c("be_ce", "be_co", "bo_ce")) %>%
  left_join(world, by = "world") %>% 
  mutate(results = pmap(.l = list(benefits = benefit_matrix,
                                  costs = cost_matrix,
                                  proportion = prop,
                                  tactic = tactic,
                                  type = type),
                        .f = global_benefits)) %>% 
  select(prop, world, tactic, type, results) %>% 
  unnest(results) %>% 
  mutate(costs = -1 * costs)

  # gather(variable, value, -c(prop, world, tactic, type)) %>% 
ggplot(data = results,
       aes(x = prop, color = tactic, linetype = type, group = paste(world, tactic, type))) +
  geom_line(aes(y = benefits)) +
  geom_line(aes(y = costs)) + 
  facet_wrap(~world, scales = "free_y", ncol = 2)

conserve(matrix = ben_het, proportion = 0.3, tactic = "mkt", dec = T) %>% image()
conserve(matrix = ben_het, proportion = 0.3, tactic = "bau", dec = T) %>% image()
conserve(matrix = ben_het, proportion = 0.3, tactic = "random_c", dec = T) %>% image()

sum(conserve(matrix = ben_het, proportion = 0.3, tactic = "mkt", dec = T) * ben_het)
sum(which_max_n(ben_het, n = 300, T) * ben_het)

apply(X = ben_het, MARGIN =  2, which_max_n, 30, T)



