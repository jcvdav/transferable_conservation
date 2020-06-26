# Load libraries
library(startR)
library(tidyverse)

# Set up a 10 by 10 matrix
# Each column represents a country

props <- seq(0.1, 1, by = 0.1)
mkt_be_ce <- mkt_be_co <- mkt_bo_ce <- mkt_bo_co <- list()

ppc <- 1000
n_countries <- 10
cols <- paste("country", 1:n_countries, sep = "_")

mat_mult <- matrix(seq(0.5, 5, length.out = n_countries), ncol = n_countries, nrow = ppc, byrow = T)
mat_mult2 <- matrix(seq(5, 0.5, length.out = n_countries), ncol = n_countries, nrow = ppc, byrow = T)

set.seed(43)
# Benefits
ben_het <- mat_mult * matrix(rnorm(n = ppc * n_countries, mean = 50,sd = 10) ^ 2.5, ncol = n_countries)
ben_hom <- matrix(mean(ben_het), ncol = n_countries, nrow = ppc)

colnames(ben_het) <- cols
colnames(ben_hom) <- cols

image(ben_het, main = "Heterogeneous benefits")

# Costs
cost_het <- mat_mult2 * matrix(rnorm(n = ppc * n_countries, mean = 50, sd = 10) ^ 2.5, ncol = n_countries)
cost_hom <- matrix(mean(cost_het), ncol = n_countries, nrow = ppc)

colnames(cost_het) <- cols
colnames(cost_hom) <- cols

image(cost_het, main = "Heterogeneous costs")

ben_het_df <- ben_het %>% 
  as_tibble() %>% 
  # mutate_all(sort, T) %>%
  mutate(pixel = 1:ppc) %>% 
  gather(country, mb, -pixel)

cost_het_df <- cost_het %>% 
  as_tibble() %>% 
  # mutate_all(sort) %>%
  mutate(pixel = 1:ppc) %>% 
  gather(country, mc, -pixel)

world <- left_join(ben_het_df, cost_het_df, by = c("pixel", "country")) %>% 
  mutate(cb = mb / mc) %>% 
  group_by(country) %>% 
  arrange(cb)%>%
  mutate(tb = cumsum(mb),
         tc = cumsum(mc),
         percent = (1:ppc) / ppc,
         tp = tb - tc) %>%
  ungroup() %>% 
  mutate(country = fct_relevel(country, cols)) 

target_percent <- 0.3

tb_targets <- world %>% 
  filter(percent == target_percent) %>% 
  select(country, tb, tc)

(target_benefit <- sum(tb_targets$tb))

ggplot(world, aes(x = percent, y = cb, color = country)) +
  geom_line() +
  geom_vline(xintercept = target_percent, linetype = "dashed") +
  # ggtheme_plot() +
  scale_x_continuous(labels = scales::percent) +
  labs(x = "Percent protected",
       y = "Marginal conservation")

ggplot(world, aes(x = percent, y = tb, color = country)) +
  geom_line() +
  geom_vline(xintercept = target_percent, linetype = "dashed") +
  geom_segment(data = tb_targets, aes(x = 0, xend = target_percent, y = tb, yend = tb, color = country), linetype = "dashed") +
  # ggtheme_plot() +
  scale_x_continuous(labels = scales::percent) +
  labs(x = "Percent protected",
       y = "Total conservation")

## Sum horizontally
agg <- left_join(ben_het_df, cost_het_df, by = c("pixel", "country")) %>% 
  mutate(cb = mb / mc) %>% 
  arrange(desc(mb)) %>% 
  mutate(tb = cumsum(mb),
         tc = cumsum(mc),
         percent = (1:nrow(.)) / nrow(.),
         tp = tb - tc)


target_benefit_g <- agg %>% 
  filter(percent == target_percent) %>% 
  pull(tb)

target_percent_g <- agg %>% 
  filter(tb <= target_benefit) %>% 
  tail(1) %>% 
  pull(percent)

target_mb <- agg %>% 
  filter(percent == target_percent) %>% 
  pull(mb)

world <- left_join(ben_het_df, cost_het_df, by = c("pixel", "country")) %>% 
  mutate(cb = mb / mc) %>% 
  group_by(country) %>% 
  arrange(mc)%>%
  mutate(tb = cumsum(mb),
         tc = cumsum(mc),
         percent = (1:ppc) / ppc,
         tp = tb - tc) %>%
  ungroup() %>% 
  mutate(country = fct_relevel(country, cols)) 

ggplot(agg, aes(x = percent, y = tb)) +
  geom_line() +
  geom_vline(xintercept = target_percent, linetype = "dashed") +
  geom_vline(xintercept = target_percent_g, linetype = "dashed") +
  # geom_hline(yintercept = target_benefit_g, linetype = "dashed") +
  geom_hline(yintercept = target_benefit, linetype = "dashed") +
  annotate(geom = "text", x = 0, y = 1.1 * target_benefit, label = bquote(bar(Q))) +
  ggtheme_plot() +
  labs(title = "Same conservation can be achieved with < 30%",
       x = "Percent protected",
       y = "Total conservation")

ggplot(agg, aes(x = percent, y = tc)) +
  geom_line() +
  geom_vline(xintercept = target_percent, linetype = "dashed") +
  geom_vline(xintercept = target_percent_g, linetype = "dashed") +
  geom_hline(yintercept = sum(tb_targets$tc), linetype = "dashed") +
  geom_hline(yintercept = 49133234.2, linetype = "dashed") +
  ggtheme_plot() +
  labs(title = "Same conservation can be achieved with < 30%",
       x = "Percent protected",
       y = "Total costs")

agg <- left_join(ben_het_df, cost_het_df, by = c("pixel", "country")) %>% 
  mutate(cb = mb / mc) %>% 
  arrange(mc) %>% 
  mutate(tb = cumsum(mb),
         tc = cumsum(mc),
         percent = (1:nrow(.)) / nrow(.),
         tp = tb - tc)

target_mc <- agg %>% 
  filter(tb <= target_benefit) %>% 
  tail(1) %>% 
  pull(mc)

target_tc <- agg %>% 
  filter(tb <= target_benefit) %>% 
  tail(1) %>% 
  pull(tc)

tb_i <- world %>% 
  filter(mc <= target_mc) %>% 
  group_by(country) %>% 
  summarize(tb = max(tb)) %>% 
  ungroup()

tc_i <- world %>% 
  filter(mc <= target_mc) %>% 
  group_by(country) %>% 
  summarize(tc = max(tc)) %>% 
  ungroup()

trade_targets <- world %>% 
  filter(mc <= target_mc) %>% 
  group_by(country) %>% 
  summarize(tb = max(tb),
            tc = max(tc),
            pct = max(percent)) %>% 
  ungroup()

trade_targets %>% 
  janitor::adorn_totals()

tb_targets %>% 
  janitor::adorn_totals()

ggplot(trade_targets, aes(x = country, y = pct)) +
  geom_col(fill = "steelblue", color = "black") + 
  geom_hline(yintercept = 0.3, linetype = "dashed") +
  scale_y_continuous(labels = scales::percent) +
  ggtheme_plot() +
  coord_flip()

ggplot(trade_targets, aes(x = country, y = pct)) +
  geom_col(fill = "steelblue", color = "black") +
  ggtheme_plot()

## Marginal costs
ggplot(mapping = aes(tb, mc)) +
  geom_line(data = world, aes(color = country)) +
  geom_line(data = agg) +
  geom_vline(xintercept = target_benefit, linetype = "dashed") +
  geom_hline(yintercept = target_mc, linetype = "dashed") +
  ggtheme_plot() +
  labs(title = "Marginal costs of conservation",
       x = "Total conservation",
       y = "Marginal costs")

# Marginal costs zoomed in
ggplot(mapping = aes(tb, mc)) +
  geom_line(data = world, aes(color = country)) +
  geom_segment(data = tb_i, aes(x = tb, xend = tb, y = 0, yend = target_cost, color = country), linetype = "dashed") +
  geom_hline(yintercept = target_cost, linetype = "dashed") +
  ggtheme_plot() +
  labs(title = "Marginal costs of conservation",
       x = "Total conservation",
       y = "Marginal costs") +
  scale_x_continuous(limits = c(0, 1.1e8), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 12.5e4), expand = c(0, 0))

## Total costs
ggplot(mapping = aes(tb, tc)) +
  geom_line(data = world, aes(color = country)) +
  geom_line(data = agg) +
  geom_vline(xintercept = target_benefit, linetype = "dashed") +
  geom_hline(yintercept = target_tc, linetype = "dashed") +
  ggtheme_plot() +
  labs(title = "Total costs of conservation",
       x = "Total conservation",
       y = "Total costs")

ggplot(mapping = aes(x = tb, y = tc)) +
  geom_line(data = world, aes(color = country)) +
  geom_segment(data = tc_i, aes(x = 0, xend = target_tc, y = tc, yend = tc, color = country), linetype = "dashed") +
  geom_hline(yintercept = target_tc, linetype = "dashed") +
  ggtheme_plot() +
  labs(title = "Total costs of conservation",
       x = "Total conservation",
       y = "Total costs") +
  scale_x_continuous(limits = c(0, 1.1e8), expand = c(0, 0))

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
  facet_wrap(~world, scales = "free_y", ncol = 2) +
  ggtheme_plot() +
  labs(title = "Benefits")

ggplot(data = results,
       aes(x = prop, color = tactic, linetype = type, group = paste(world, tactic, type))) +
  geom_line(aes(y = costs)) +
  facet_wrap(~world, scales = "free_y", ncol = 2) +
  ggtheme_plot() +
  labs(title = "Ceosts")

conserve(matrix = ben_het, proportion = 0.3, tactic = "mkt", dec = T) %>% image()
conserve(matrix = ben_het, proportion = 0.3, tactic = "bau", dec = T) %>% image()
conserve(matrix = ben_het, proportion = 0.3, tactic = "random_c", dec = T) %>% image()

sum(conserve(matrix = ben_het, proportion = 0.3, tactic = "mkt", dec = T) * ben_het)



