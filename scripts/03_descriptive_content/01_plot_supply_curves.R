# Load packages

# Load data

global_cb <- readRDS(file = file.path(project_path, "processed_data", "global_costs_and_benefits.rds"))
eez_cb <- readRDS(file = file.path(project_path, "processed_data", "eez_costs_and_benefits.rds"))
rlm_eez_cb <- readRDS(file = file.path(project_path, "processed_data", "rlm_eez_costs_and_benefits.rds"))
pro_eez_cb <- readRDS(file = file.path(project_path, "processed_data", "pro_eez_costs_and_benefits.rds"))
eco_eez_cb <- readRDS(file = file.path(project_path, "processed_data", "eco_eez_costs_and_benefits.rds"))

# FIGURES

# Global curve
ggplot(mapping = aes(x = tb, y = cost)) +
  geom_line(data = global_data)

# Country-level supply curves
ggplot(mapping = aes(x = tb, y = cost)) +
  geom_line(data = country_data, aes(color = iso3)) +
  guides(color = F)

# Country realm
ggplot(mapping = aes(x = tb, y = cost)) +
  geom_line(data = rlm_eez_cb, aes(color = iso3)) +
  guides(color = F) +
  facet_wrap(~realm, scales = "free")

ggplot(mapping = aes(x = tb, y = cost)) +
  geom_line(data = pro_eez_cb, aes(color = iso3)) +
  guides(color = F) +
  facet_wrap(~province, scales = "free")
