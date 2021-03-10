library(startR)
library(cowplot)
library(tidyverse)

data <- expand_grid(
  n = c("A", "B"),
  q = seq(0, 10, by = 0.1)
) %>% 
  mutate(p = ifelse(n == "A", 1 * q, 2 * q)) %>% 
  group_by(n) %>% 
  ungroup()

(p1 <- ggplot(data = data, aes(x = q, y = p, color = n)) +
  geom_line(size = 1, linetype = "dashed") +
  geom_line(data = filter(data, q <= 5), size = 1) +
  scale_color_brewer(palette = "Set1") +
  theme_half_open() +
  labs(x = "Conservation (q)", y = "Marginal Costs (p)") +
  geom_segment(x = 5, xend = 5, y = 0, yend = 25, color = "black", linetype = "dashed") +
  geom_segment(x = 0, xend = 5, y = 5, yend = 5, linetype = "dashed", color = "red") +
  geom_segment(x = 0, xend = 5, y = 10, yend = 10, linetype = "dashed", color = "steelblue") +
  scale_x_continuous(expand = c(0, 0), breaks = c(5), labels = c("q"), limits = c(0, 7.5)) +
  scale_y_continuous(expand = c(0, 0), breaks = c(5, 10), labels = c("p", expression(p + Delta)), limits = c(0, 15)) +
  theme(legend.justification = c(0, 1),
        legend.position = c(0.1, 1)) +
  guides(color = guide_legend(title = "Nation")))

(p2 <- ggplot(data = data, aes(x = q, y = p, color = n)) +
  geom_line(size = 1, linetype = "dashed") +
  geom_line(data = filter(data, p <= 6.7), size = 1) +
  scale_color_brewer(palette = "Set1") +
  theme_half_open() +
  labs(x = "Conservation (q)", y = "Marginal Costs (p)") +
  geom_segment(x = 3.35, xend = 3.35, y = 0, yend = 25, color = "steelblue", linetype = "dashed") +
  geom_segment(x = 6.7, xend = 6.7, y = 0, yend = 25, color = "red", linetype = "dashed") +
  geom_segment(x = 0, xend = 6.7, y = 6.7, yend = 6.7, linetype = "dashed", color = "black") +
  scale_x_continuous(expand = c(0, 0), breaks = c(3.35, 5, 6.7), labels = c("q'", "q", "2q - q'"), limits = c(0, 7.5)) +
  scale_y_continuous(expand = c(0, 0), breaks = c(5, 6.7, 10), labels = c("p", "p*", expression(p + Delta)), limits = c(0, 15)) +
  theme(legend.position = "none"))


cowplot::plot_grid(p1, p2, ncol = 2, labels = c(1, 2)) %>% 
  lazy_ggsave(filename = "dummy_supply_curves", width = 6, height = 2.5)
