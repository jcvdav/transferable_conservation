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
# The processing part sorts the dataset (ascending and descending) by the relevant variable (cost or benefit)
# It then calculates the total benefit (using the "get_country_pathways" function),
# as well as the percentage contribution. The process is done globally and for each country.

## Global
# Most efficient
agg_cb <- eez_cb %>% 
  arrange(desc(bcr)) %>% 
  get_country_pathways() %>% 
  mutate(type = "efficient")

# Maximize conservation
max_con_agg <- eez_cb %>% 
  arrange(desc(benefit)) %>% 
  get_country_pathways() %>% 
  mutate(type = "maximize conservation")

# Minimize conservation
min_con_agg <- eez_cb %>% 
  arrange(benefit)  %>% 
  get_country_pathways() %>% 
  mutate(type = "minimize conservation")

# Maximize costs
max_cos_agg <- eez_cb %>% 
  arrange(desc(cost)) %>% 
  get_country_pathways() %>% 
  mutate(type = "maximize costs")

# Minimize costs
min_cos_agg <- eez_cb %>% 
  arrange(cost) %>% 
  get_country_pathways() %>% 
  mutate(type = "minimize costs")

# We now combine all curves together
all_curves_agg <- rbind(
  agg_cb,
  max_con_agg,
  min_con_agg,
  max_cos_agg,
  min_cos_agg)

## National
# Maximize conservation
max_con_eez <- eez_cb %>% 
  group_by(iso3) %>% 
  arrange(desc(benefit)) %>% 
  nest() %>% 
  mutate(data = map(data, get_country_pathways)) %>% 
  ungroup() %>% 
  mutate(type = "maximize conservation") %>% 
  unnest(data)

# Minimize conservation
min_con_eez <- eez_cb %>% 
  group_by(iso3) %>% 
  arrange(benefit)  %>% 
  nest() %>% 
  mutate(data = map(data, get_country_pathways)) %>% 
  ungroup() %>% 
  mutate(type = "minimize conservation") %>% 
  unnest(data)

# Maximize costs
max_cos_eez <- eez_cb %>% 
  group_by(iso3) %>% 
  arrange(desc(cost))  %>% 
  nest() %>% 
  mutate(data = map(data, get_country_pathways)) %>% 
  ungroup() %>% 
  mutate(type = "maximize costs") %>% 
  unnest(data)

# Minimize costs
min_cos_eez <- eez_cb %>% 
  group_by(iso3) %>% 
  arrange(cost) %>% 
  nest() %>% 
  mutate(data = map(data, get_country_pathways)) %>% 
  ungroup() %>% 
  mutate(type = "minimize costs") %>% 
  unnest(data)


# We now combine all curves together
all_curves_eez <- rbind(
  eez_cb,
  max_con_eez,
  min_con_eez,
  max_cos_eez,
  min_cos_eez)

# Now that we know the country-level conservation,
# we can derive the global by summing across all of those.
conservation_targets <- all_curves_eez %>%
  group_by(type) %>%
  nest() %>%
  mutate(tb = map_dbl(data, benefit, 0.3)) %>% 
  select(-data)

# Plot all the country-level protection curves. The curvature is different,
# based on the approach taken. But the end value is always the same.
all_supply_curves_eez <- 
  ggplot() +
  geom_line(data = all_curves_eez, mapping = aes(x = pct, y = tb, group = iso3), size = 0.1) +
  geom_text(data = conservation_targets, aes(label = round(tb)), x = 0.1, y = 9, size = 3) +
  facet_wrap(~type) +
  geom_vline(xintercept = 0.3, linetype = "dashed") +
  ggtheme_plot() +
  scale_x_continuous(labels = scales::percent) +
  labs(x = "% EEZ Protected",
       y = "Biodiversity")

all_supply_curves_agg <- 
  ggplot() +
  geom_line(data = all_curves_agg, mapping = aes(x = pct, y = tb, color = type), size = 1) +
  geom_text(data = conservation_targets, aes(label = round(tb), y = round(tb)), x = 0.25, size = 3) +
  geom_vline(xintercept = 0.3, linetype = "dashed") +
  ggtheme_plot() +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(labels = scales::percent) +
  labs(x = "%Oceans protected (within EEZ only)",
       y = "Biodiversity") + 
  guides(color = guide_legend(title = "Approach"))

# Export stuff

lazy_ggsave(all_supply_curves_eez,
            "eez_supply_curves_approaches",
            width = 14,
            height = 9)

lazy_ggsave(all_supply_curves_agg,
            "agg_supply_curves_approaches",
            width = 14,
            height = 9)

saveRDS(object = conservation_targets,
        file = file.path(project_path, "output_data", "conservation_targets.rds"))



















