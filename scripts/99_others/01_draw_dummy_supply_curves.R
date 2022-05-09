######################################################
#title#
######################################################
# 
# Purpose
#
######################################################

## SET UP ######################################################################
# Load packages
library(here)
library(startR)
library(cowplot)
library(ggpattern)
library(tidyverse)

data <- expand_grid(
  n = c("A", "B"),
  q = seq(0, 10, by = 1)
) %>% 
  mutate(p = ifelse(n == "A", 1 * q, 2 * q)) %>% 
  group_by(n) %>% 
  ungroup()

pol1 <- tibble(q = c(0, 5, 5, 0, 0, 5, 5, 0),
              p = c(0, 10, 0, 0, 0, 5, 0, 0),
              n = c("A", "A", "A", "A",
                    "B", "B", "B", "B"))

pol2 <- tibble(q = c(0.3, 0.3, 0.4, 0.3, 0.2, 0.3, 0.3, 0.2),
               p = c(3, 4, 4, 3, 4, 4, 6, 4),
               n = c("A", "A", "A", "A",
                     "B", "B", "B", "B"))

bau_text <- tibble(x = c(0.25, 0.25, 0.5, 0.55),
                   y = c(4, 1.5, 11, 6),
                   label = c("TC[1]", "TC[2]", "MC[1]", "MC[2]"))

mkt_text <- tibble(x = c(0.12, 0.25, 0.5, 0.55, 0.275, 0.325),
               y = c(2, 1.5, 11, 6, 5, 3.5),
               label = c("TC[1]", "TC[2]", "MC[1]", "MC[2]", "S[1]", "S[2]"))


p1 <- data %>% 
  filter(n == "B") %>% 
  ggplot() +
  geom_line(mapping = aes(x = q / 10, y = p), size = 1, color = "steelblue") +
  geom_text(x = .95, y = 20.5, label = expression(MC[1])) +
  theme_half_open() +
  labs(x = "Conservation", y = "Marginal Costs") +
  scale_x_continuous(expand = c(0, 0),
                     breaks = (1:10)/10,
                     labels = scales::percent,
                     limits = c(0, 0.6)) +
  scale_y_continuous(expand = c(0, 0),
                     # breaks = c(5, 10, 15),
                     # labels = c(5, 10, 15),
                     limits = c(0, 20))


p2 <- p1 +
  geom_vline(xintercept = 0.1, color = "black", linetype = "dashed") +
  geom_segment(x = 0, xend = 0.1, y = 2, yend = 2, linetype = "dashed", color = "steelblue") +
  geom_polygon(data = tibble(x = c(0, 0.1, 0.1, 0),
                             y = c(0, 2, 0, 0)),
               mapping = aes(x = x ,y = y),
               fill = "steelblue",
               alpha = 0.5)

p3 <- p1 +
  geom_vline(xintercept = 0.3, color = "black", linetype = "dashed") +
  geom_segment(x = 0, xend = 0.3, y = 6, yend = 6, linetype = "dashed", color = "steelblue") + 
  geom_polygon(data = tibble(x = c(0, 0.3, 0.3, 0),
                             y = c(0, 6, 0, 0)),
               mapping = aes(x = x ,y = y),
               fill = "steelblue",
               alpha = 0.5)

p_full <- ggplot() +
  geom_line(data = data,
            mapping = aes(x = q / 10, y = p, color = n),
            size = 1) +
  geom_text(x = .95, y = 20.5, label = expression(MC[1])) +
  geom_text(x = .95, y = 10.5, label = expression(MC[2])) +
  theme_half_open() +
  labs(x = "Conservation", y = "Marginal Costs") +
  theme(legend.position = "None") +
  scale_x_continuous(expand = c(0, 0),
                     breaks = (1:10)/10,
                     labels = scales::percent,
                     limits = c(0, 1.1)) +
  scale_y_continuous(expand = c(0, 0),
                     # breaks = c(5, 10, 15),
                     # labels = c(5, 10, 15),
                     limits = c(0, 25)) +
  scale_color_manual(values = c("red", "steelblue"))

p4 <- p_full +
  geom_vline(xintercept = 0.3, color = "black", linetype = "dashed") +
  geom_segment(x = 0, xend = 0.3, y = 6, yend = 6, linetype = "dashed", color = "steelblue") +
  geom_segment(x = 0, xend = 0.3, y = 3, yend = 3, linetype = "dashed", color = "red")

p4_area <- p4 +
  geom_polygon(data = tibble(x = c(0, 0.3, 0.3, 0),
                             y = c(0, 6, 0, 0)),
               mapping = aes(x = x ,y = y),
               fill = "steelblue",
               alpha = 0.5) +
  geom_polygon(data = tibble(x = c(0, 0.3, 0.3, 0),
                             y = c(0, 3, 0, 0)),
               mapping = aes(x = x ,y = y),
               fill = "red",
               alpha = 0.5) +
  scale_x_continuous(breaks = c(0, 0.3, 0.6),
                     labels = c("0", "Q", "2Q"),
                     expand = c(0, 0),
                     limits = c(0, 0.6)) +
  scale_y_continuous(breaks = c(0, 3, 6),
                     labels = c("0", expression(P[low]), expression(P[high])),
                     limits = c(0, 12),
                     expand = c(0, 0)) +
  labs(x = "Conservation (Q)", "Marginal Costs (P)") +
  geom_text(data = bau_text,
            aes(x = x, y = y, label = label),
            parse = T)

p5 <- p4 +
  geom_hline(yintercept = 4, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 0.3, color = "gray", linetype = "dashed")

p6 <- p_full  +
  geom_hline(yintercept = 4, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 0.3, color = "gray", linetype = "dashed") +
  geom_segment(x = 0.2, xend = 0.2, y = 0, yend = 4, color = "steelblue", linetype = "dashed") +
  geom_segment(x = 0.4, xend = 0.4, y = 0, yend = 4, color = "red", linetype = "dashed")

p6_area <- p6 +
  geom_polygon(data = tibble(x = c(0, 0.2, 0.2, 0),
                             y = c(0, 4, 0, 0)),
               mapping = aes(x = x ,y = y),
               fill = "steelblue",
               alpha = 0.5) +
  geom_polygon(data = tibble(x = c(0, 0.3, 0.3, 0),
                             y = c(0, 3, 0, 0)),
               mapping = aes(x = x ,y = y),
               fill = "red",
               alpha = 0.5) +
  geom_polygon_pattern(data = tibble(x = c(0.3, 0.3, 0.4, 0.4, 0.3),
                             y = c(0, 4, 4, 0, 0)),
               mapping = aes(x = x ,y = y),
               fill = "red",
               pattern_fill = "steelblue",
               color = "transparent",
               alpha = 0.5) +
  geom_polygon(data = pol2 %>% filter(n == "B"), aes(x = q, y = p), fill = "steelblue", color = "black") +
  geom_polygon(data = pol2 %>% filter(n == "A"), aes(x = q, y = p), fill = "red", color = "black", alpha = 0.5) +
  scale_x_continuous(breaks = c(0, 0.2, 0.3, 0.4, 0.6),
                     labels = c("0", "2/3Q", "Q", "4/3Q", "2Q"),
                     expand = c(0, 0),
                     limits = c(0, 0.6)) +
  scale_y_continuous(breaks = c(0, 3, 4, 6),
                     labels = c("0", expression(P[low]), expression(P[mkt]), expression(P[high])),
                     limits = c(0, 12), expand = c(0, 0)) +
  labs(x = "Conservation (Q)", "Marginal Costs (P)") +
  geom_text(data = mkt_text, aes(x = x, y = y, label = label), parse = T)




paper_figure <- plot_grid(p4_area, p6_area,
                          ncol = 2,
                          labels = "AUTO")

lazy_ggsave(plot = paper_figure,
            filename = "dummy_plots/stialized_supply_curves",
            width = 18,
            height = 8)

  
p7 <- p6 +
  geom_polygon(data = pol2 %>% filter(n == "B"), aes(x = q, y = p), fill = "steelblue", alpha = 0.5, color = "transparent") +
  geom_polygon(data = pol2 %>% filter(n == "A"), aes(x = q, y = p), fill = "red", alpha = 0.5, color = "transparent")


# Export figures
lazy_ggsave(plot = p1, filename = "dummy_plots/supply_curves1", width = 18, height = 12)
lazy_ggsave(plot = p2, filename = "dummy_plots/supply_curves2", width = 18, height = 12)
lazy_ggsave(plot = p3, filename = "dummy_plots/supply_curves3", width = 18, height = 12)
lazy_ggsave(plot = p4, filename = "dummy_plots/supply_curves4", width = 18, height = 12)
lazy_ggsave(plot = p5, filename = "dummy_plots/supply_curves5", width = 18, height = 12)
lazy_ggsave(plot = p6, filename = "dummy_plots/supply_curves6", width = 18, height = 12)
lazy_ggsave(plot = p7, filename = "dummy_plots/supply_curves7", width = 18, height = 12)


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

p8 <- ggplot(data = sc_dat, aes(x = tb, y = mc)) +
  geom_step(direction = "vh", size = 1, color = "steelblue") +
  geom_text(data = sc_dat %>% filter(pixel > 0), aes(label = paste("Pixel", pixel)), nudge_x = -3, nudge_y = 0.5) +
  theme_half_open() +
  labs(x = "Conservation (Q)", y = "Marginal Costs (C)")

lazy_ggsave(plot = p8, filename = "dummy_plots/supply_curves8", width = 9, height = 6)
