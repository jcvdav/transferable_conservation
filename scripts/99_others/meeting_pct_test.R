dt <- tibble(pixel = 1:4,
             b = c(10, 8, 1, 5),
             c = c(20, 40, 10, 5),
             mc = c/b) %>% 
  arrange(mc) %>% 
  mutate(tb = cumsum(b)) %>% 
  rbind(tibble(pixel = 0,
               b = 0,
               c = 0,
               mc = 0,
               tb = 0)) %>% 
  mutate(pct = tb/sum(b),
         id = "a")

r <- 0.3

mins <- dt %>% 
  filter(pct <= r) %>% 
  select(id, min_pct = pct) %>% 
  group_by(id) %>% 
  slice_max(min_pct)

nexts <- dt %>% 
  left_join(mins, by = "id") %>% 
  replace_na(replace = list(min_pct = 0)) %>% 
  filter(pct > min_pct) %>% 
  group_by(id) %>% 
  slice_min(pct) %>% 
  mutate(dif = pct - min_pct,
         missing = r - min_pct,
         p = missing / dif,
         pt = min_pct + (p * dif))

ggplot(dt, aes(x = tb, y = mc)) +
  geom_step(direction = "vh", size = 2, color = "steelblue") +
  geom_text(aes(x = tb - 1.5, y = mc + 0.5, label = paste("Pixel", pixel))) +
  lims(x = c(0, 25)) +
  theme_minimal() +
  labs(x = "Conservation Benefit",
       y = "Marginal cost of conservation")

dt <- dt %>% 
 filter(pixel > 0) %>%
  mutate(c = ifelse(pixel == 3, 0, c),
         mc = c / b) %>% 
  arrange(mc) %>% 
  mutate(tb = 1 + cumsum(b)) %>% 
  rbind(tibble(pixel = 0,
               b = 0,
               c = 0,
               mc = 0,
               tb = 0))

ggplot(dt, aes(x = tb, y = mc)) +
  geom_step(direction = "vh", size = 2, color = "steelblue") +
  geom_text(aes(x = tb - 1.5, y = mc + 0.5, label = paste("Pixel", pixel))) +
  lims(x = c(0, 25)) +
  theme_minimal() +
  labs(x = "Conservation Benefit",
       y = "Marginal cost of conservation")
