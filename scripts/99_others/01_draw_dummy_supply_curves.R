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

pol1 <- tibble(q = c(0, 5, 5, 0, 0, 5, 5, 0),
              p = c(0, 10, 0, 0, 0, 5, 0, 0),
              n = c("A", "A", "A", "A",
                    "B", "B", "B", "B"))

pol2 <- tibble(q = c(3.35, 5, 5, 3.35, 5, 6.7, 5, 5),
               p = c(6.7, 10, 6.7, 6.7, 6.7, 6.7, 5, 6.7),
               n = c("A", "A", "A", "A",
                     "B", "B", "B", "B"))

p1 <- data %>% 
  filter(n == "B") %>% 
  ggplot(mapping = aes(x = q, y = p)) +
  geom_line(size = 1, color = "steelblue") +
  geom_text(x = 10, y = 20, label = expression(MC[1])) +
  theme_half_open() +
  labs(x = "Conservation", y = "Marginal Costs") +
  scale_x_continuous(expand = c(0, 0),
                     # breaks = c(5),
                     # labels = c("Q"),
                     limits = c(0, 15)) +
  scale_y_continuous(expand = c(0, 0),
                     # breaks = c(1, 5, 10, 15),
                     # labels = c(1, 5, 10, 15),
                     limits = c(0, 25))


p2 <- p1 +
  geom_segment(x = 5, xend = 5, y = 0, yend = 25, color = "black", linetype = "dashed") +
  geom_segment(x = 0, xend = 5, y = 10, yend = 10, linetype = "dashed", color = "steelblue")


(p1 <- ggplot(data = data, aes(x = q, y = p, color = n)) +
  geom_line(size = 1) +
  scale_color_brewer(palette = "Set1") +
  theme_half_open() +
  labs(x = "Conservation", y = "Marginal Costs") +
  geom_segment(x = 5, xend = 5, y = 0, yend = 25, color = "black", linetype = "dashed") +
  geom_segment(x = 0, xend = 5, y = 5, yend = 5, linetype = "dashed", color = "red") +
  geom_segment(x = 0, xend = 5, y = 10, yend = 10, linetype = "dashed", color = "steelblue") +
  scale_x_continuous(expand = c(0, 0),
                     breaks = c(5),
                     labels = c("Q"),
                     limits = c(0, 15)) +
  scale_y_continuous(expand = c(0, 0),
                     breaks = c(5, 10),
                     labels = c("P", expression(P + Delta)),
                     limits = c(0, 20)) +
  theme(legend.justification = c(0, 1),
        legend.position = c(0.01, 1)) +
  guides(color = guide_legend(title = "Nation")))

p2 <- p1 +
  geom_polygon(data = pol1 %>% filter(n == "A"), fill = "steelblue", alpha = 0.5, color = "transparent")

p3 <- p1 +
  geom_polygon(data = pol1 %>% filter(n == "B"), fill = "red", alpha = 0.5, color = "transparent")
  

(p4 <- ggplot(data = data, aes(x = q, y = p, color = n)) +
  geom_line(size = 1) +
  geom_abline(intercept = 0, slope = 0.67, size = 1) +
  scale_color_brewer(palette = "Set1") +
  theme_half_open() +
  labs(x = "Conservation (q)", y = "Marginal Costs (p)") +
  geom_segment(x = 5, xend = 5, y = 0, yend = 25, color = "gray") +
  geom_segment(x = 10, xend = 10, y = 0, yend = 6.7, linetype = "dashed", color = "black") +
  geom_segment(x = 3.35, xend = 3.35, y = 0, yend = 25, color = "steelblue", linetype = "dashed") +
  geom_segment(x = 6.7, xend = 6.7, y = 0, yend = 25, color = "red", linetype = "dashed") +
  geom_segment(x = 0, xend = 10, y = 6.7, yend = 6.7, linetype = "dashed", color = "black") +
  scale_x_continuous(expand = c(0, 0),
                     breaks = c(3.35, 5, 6.7, 10),
                     labels = c(expression(Q[b]), "Q", expression(Q[a]), expression("2Q="~Q[a]+Q[b])),
                     limits = c(0, 15)) +
  scale_y_continuous(expand = c(0, 0),
                     breaks = c(5, 6.7, 10),
                     labels = c("P", "P*", expression(P + Delta)),
                     limits = c(0, 20)) +
    theme(legend.justification = c(0, 1),
          legend.position = c(0.01, 1)) +
    guides(color = guide_legend(title = "Nation")))

p5 <- p4 +
  geom_polygon(data = pol2 %>% filter(n == "A"), fill = "steelblue", alpha = 0.5, color = "transparent") +
  geom_polygon(data = pol2 %>% filter(n == "B"), fill = "red", alpha = 0.5, color = "transparent")


# Export figures
lazy_ggsave(plot = p1, filename = "dummy_plots/supply_curves1", width = 9, height = 6)
lazy_ggsave(plot = p2, filename = "dummy_plots/supply_curves2", width = 9, height = 6)
lazy_ggsave(plot = p3, filename = "dummy_plots/supply_curves3", width = 9, height = 6)
lazy_ggsave(plot = p4, filename = "dummy_plots/supply_curves4", width = 9, height = 6)
lazy_ggsave(plot = p5, filename = "dummy_plots/supply_curves5", width = 9, height = 6)


sc_dat <- tibble(pixel = 1:4,
                 p = c(0.5, 0.8, 1, 1),
                 a = c(20, 10, 1, 5),
                 b = c(10, 8, 1, 5),
                 c = c(20, 40, 10, 5),
                 mc = c/b) %>% 
  arrange(mc) %>% 
  rbind(tibble(pixel = 0,
               p = 0,
               a = 0,
               b = 0,
               c = 0,
               mc = 0), .) %>% 
  mutate(tb = cumsum(b))

p6 <- ggplot(data = sc_dat, aes(x = tb, y = mc)) +
  geom_step(direction = "vh", size = 1, color = "steelblue") +
  geom_text(data = sc_dat %>% filter(pixel > 0), aes(label = paste("Pixel", pixel)), nudge_x = -2, nudge_y = 0.5) +
  theme_half_open() +
  labs(x = "Conservation (q)", y = "Marginal Costs (p)")

lazy_ggsave(plot = p6, filename = "dummy_plots/supply_curves6", width = 9, height = 6)
