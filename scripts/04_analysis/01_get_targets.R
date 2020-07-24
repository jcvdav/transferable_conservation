#######################################################
# There are many ways the 30% can be interpreted. Each
# country may decide to protect their 30% in a
# different way. For example, by protecting the 30% with
# the highest biodiversity, or the 30% with the least cost.
# 
# To avoid abcriguities, I calculate the country-level
# biodiversity benefit of conserving 30% with the
# the folloeing rules:
# 
# - Most efficient (benefit / cost)
# - Most conservtion (benefit)
# - Least cosnervation (-benefit)
# - Most costly (cost)
# - Cheapest (-cost)
# 
# I then calculate the global biodiversity benefit by
# summing across all nations. These 5 nubcrers represent
# plausible global biodiversity targets.
#######################################################



# SET UP ################################################################################################
# Load packages
library(startR)
library(tidyverse)

# Load data
eez_cb <- readRDS(file = file.path(project_path, "processed_data", "eez_costs_and_benefits.rds")) %>% 
  mutate(type = "efficient") %>% 
  select(iso3, everything())

eez_h_sum_cb <- readRDS(file = file.path(project_path, "processed_data", "eez_h_sum_costs_and_benefits.rds")) %>% 
  mutate(type = "efficient") %>% 
  select(iso3, everything())

# PROCESSING #############################################################################################
# The procesing part sorts the dataset (ascending and descending) by the relevant variable (cost or benefit)
# It then calculates the total benefit (using the "get_country_patwhays" function),
# as well as the percentage contribution

# Maximize conservation
max_con <- eez_cb %>% 
  group_by(iso3) %>% 
  arrange(desc(benefit)) %>% 
  nest() %>% 
  mutate(data = map(data, get_country_pathways)) %>% 
  ungroup() %>% 
  mutate(type = "maximize conservation") %>% 
  unnest(data)

# Minimize conservation
min_con <- eez_cb %>% 
  group_by(iso3) %>% 
  arrange(benefit)  %>% 
  nest() %>% 
  mutate(data = map(data, get_country_pathways)) %>% 
  ungroup() %>% 
  mutate(type = "minimize conservation") %>% 
  unnest(data)

# Maximize costs
max_cos <- eez_cb %>% 
  group_by(iso3) %>% 
  arrange(desc(cost))  %>% 
  nest() %>% 
  mutate(data = map(data, get_country_pathways)) %>% 
  ungroup() %>% 
  mutate(type = "maximize costs") %>% 
  unnest(data)

# Minimize costs
min_cos <- eez_cb %>% 
  group_by(iso3) %>% 
  arrange(cost) %>% 
  nest() %>% 
  mutate(data = map(data, get_country_pathways)) %>% 
  ungroup() %>% 
  mutate(type = "minimize costs") %>% 
  unnest(data)


# We now combine all curves together
all_curves <- rbind(
  eez_cb,
  max_con,
  min_con,
  max_cos,
  min_cos) %>% 
  group_by(type)

# Now that we know the country-level conservation,
# we can derive the global by summing across all of those.
conservation_targets <- all_curves %>%
  group_by(type) %>%
  nest() %>%
  mutate(tb = map_dbl(data, benefit, 0.3)) %>% 
  select(-data)

# Plot all the country-level protection curves. The curvature is different,
# based on the approach taken. But the end value is always the same.
all_supply_curves <- 
  ggplot() +
  geom_line(data = all_curves, mapping = aes(x = pct, y = tb, group = iso3), size = 0.1) +
  geom_text(data = conservation_targets, aes(label = round(tb)), x = 0.1, y = 2000, size = 3) +
  facet_wrap(~type) +
  geom_vline(xintercept = 0.3, linetype = "dashed") +
  ggtheme_plot() +
  scale_x_continuous(labels = scales::percent) +
  labs(x = "%EEZ protected",
       y = "Biodiversity")

# Export stuff

lazy_ggsave(all_supply_curves,
            "eez_supply_curves_approaches",
            width = 14,
            height = 9)

saveRDS(object = conservation_targets,
        file = file.path(project_path, "output_data", "conservation_targets.rds"))



















