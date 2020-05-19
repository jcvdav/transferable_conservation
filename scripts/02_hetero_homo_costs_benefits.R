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

results <- expand_grid(prop = props, 
                       tactic = c("mkt", "bau", "random_c"), type = c("max_benefits", "min_costs"),
                       benefit_matrix = list(ben_het, ben_hom),
                       cost_matrix = list(cost_het, cost_hom)) %>% 
  mutate(results = pmap(.l = list(benefits = benefit_matrix,
                                  costs = cost_matrix,
                                  proportion = prop,
                                  tactic = tactic,
                                  type = type),
                        .f = global_benefits))

results %>% 
  select(prop, tactic, type, results) %>% 
  unnest(results) %>% 
  ggplot(aes(x = prop, y = benefits, color = tactic)) +
  geom_point()

results %>% 
  select(prop, tactic, type, results) %>% 
  unnest(results) %>% 
  ggplot(aes(x = prop, y = costs, color = tactic)) +
  geom_point()
         





