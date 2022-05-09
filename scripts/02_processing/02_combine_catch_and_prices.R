######################################################
# 02_combine_catch_and_prices
######################################################
# 
# Builds up a taxonomy-based table used to match
# price data to catch data.
#
######################################################

# Load packages
library(rfishbase)
library(corrplot)
library(tidyverse)


# Set up taxonomy baseline
## Get taxonomy from SeaLifeBase
sealifebase_names <- load_taxa(server = "sealifebase") %>%
  as_tibble() %>%
  select(Species, Genus, Family, Order, Class)

## Get taxonomy from FishBase
fishbase_names <- load_taxa(server = "fishbase") %>%
  as_tibble() %>%
  select(Species, Genus, Family, Order, Class)

# Combine taxonomy
taxonomy <- rbind(sealifebase_names, fishbase_names) %>% 
  select(Genus, Family, Order, Class) %>% 
  distinct() %>% 
  janitor::clean_names() %>% 
  mutate(genus = ifelse(genus == "Not assigned", NA, genus),
         family = ifelse(family == "Not assigned", NA, family),
         order = ifelse(order == "Not assigned", NA, order),
         class = ifelse(class == "Not assigned", NA, class)) %>% 
  filter(!(genus == "Mytilus" & class == "Bivalva"),
         !(genus == "Pharus" & is.na(order))) %>% 
  mutate_all(~str_remove_all(., "<a0>")) %>% 
  distinct()

# Get unique lists of each taxon
families <- unique(taxonomy$family)
orders <- unique(taxonomy$order)
classes <- unique(taxonomy$class)

# We'll need to use stepped matching, because the "species" columns
# in price and catch data contain different taxons. We'll have three
# tables, and we'll join them hierarchichally after moving the "species"
# to its correct taxon.
## Match genus and family
taxonomy_genus_family <- taxonomy %>% 
  select(genus, family) %>% 
  distinct() %>% 
  drop_na(genus) %>% 
  group_by(genus) %>% 
  slice(n = 1) %>% 
  ungroup()

## Match family and order
taxonomy_family_order <- taxonomy %>% 
  select(family, order) %>% 
  distinct() %>% 
  drop_na(family)%>% 
  group_by(family) %>% 
  slice(n = 1) %>% 
  ungroup()

## Match order and class
taxonomy_order_class <- taxonomy %>% 
  select(order, class) %>% 
  distinct() %>% 
  drop_na(order)%>% 
  group_by(order) %>% 
  slice(n = 1) %>% 
  ungroup()

# Read-in price data and perform high-level data cleaning
price <- read.csv(
  file.path(
    clean_seafod_path,
    "exvessel_price_database_1976_2017.csv"
  ),
  stringsAsFactors = F) %>% 
  as_tibble() %>% 
  janitor::clean_names() %>% 
  filter(between(year, 2005, 2015)) %>% 
  filter_all(all_vars(!str_detect(., "[fF]reshwater|FRESHWATER"))) %>% 
  select(-x) %>% 
  drop_na(exvessel) %>% 
  mutate(scientific_name = str_remove_all(scientific_name, " spp"),
         scientific_name = str_remove_all(scientific_name, "\\(.*\\)"),
         scientific_name = str_remove_all(scientific_name, "[:punct:]"),
         scientific_name = str_squish(scientific_name),
         scientific_name = str_trim(scientific_name),
         isscaap_division = str_to_sentence(isscaap_division),
         asfis_species = str_remove_all(asfis_species, "\\(.*\\)"),
         asfis_species = str_remove_all(asfis_species, "[:punct:]"),
         asfis_species = str_squish(asfis_species),
         asfis_species = str_trim(asfis_species)) 

# Take only the species data, and validate the scientific names and all taxonomy
validated_taxonomy <- price %>% 
  select(scientific_name) %>% 
  distinct() %>% 
  mutate(species = my_validate(scientific_name)) %>% 
  mutate(genus = str_extract(species, pattern = "^[:alpha:]+")) %>% 
  left_join(taxonomy_genus_family, by = "genus") %>% 
  mutate(family = ifelse(is.na(family) & genus %in% families, genus, family)) %>% 
  left_join(taxonomy_family_order, by = "family") %>% 
  mutate(order = ifelse(is.na(order) & genus %in% orders, genus, order)) %>% 
  left_join(taxonomy_order_class, by = "order") %>% 
  mutate(class = ifelse(is.na(class) & genus %in% classes, genus, class)) %>% 
  mutate(genus = ifelse(genus == family, NA_character_, genus),
         genus = ifelse(genus == order, NA_character_, genus),
         genus = ifelse(genus == class, NA_character_, genus))
  
# Now we need to create a list of cases where "species" is not actually "species", but a higher taxon.
# Leaving them in would be as double-counting.

# Find cases where species is actually a genus
redundant_prices_species_genus <- validated_taxonomy %>%
  group_by(genus) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(species == genus, n > 1) %>% 
  pull(species)

# Find cases where species is actually a family
redundant_prices_species_family <- validated_taxonomy %>%
  group_by(family) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(species == family, n > 1) %>% 
  pull(species)

# Find cases where species is actually an order
redundant_prices_species_order <- validated_taxonomy%>%
  group_by(order) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(species == order, n > 1) %>% 
  pull(species)

# Find cases where species is actually a class
redundant_prices_species_class <- validated_taxonomy %>%
  group_by(class) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(species == class, n > 1) %>% 
  pull(species)

# Now we remove all those from our list
validated_taxonomy <- validated_taxonomy %>% 
  filter(!species %in% c(redundant_prices_species_genus,
                         redundant_prices_species_family,
                         redundant_prices_species_order,
                         redundant_prices_species_class))


updated_price <- price %>% 
  right_join(validated_taxonomy, by = "scientific_name")

# Now we create a fucntion that calculates median for any desired taxa

summarize_group <- function(x, group) {
  varname <- paste0(group, c("_median"))
  x %>% 
    group_by_at(vars(one_of(group, "year"))) %>% 
    summarize(median = median(exvessel, na.rm = T)) %>% 
    magrittr::set_colnames(c(group, "year", varname)) %>% 
    drop_na()
}

# Get species-level median
price_species <- updated_price %>% 
  summarize_group("species")

# Get genus-level median
price_genus <- updated_price %>% 
  summarize_group("genus")

# Get family-level median
price_family <- updated_price %>% 
  summarize_group("family")

# Get order-level median
price_order <- updated_price %>% 
  summarize_group("order")

# Get class-level median
price_class <- updated_price %>% 
  summarize_group("class")

# Get family-level median
price_isscaap_group <- updated_price %>% 
  summarize_group("isscaap_group")

# We also know that the Watson dataset contains a "marine animals" category, whic in this case we'll
# impute using the overall median value, for lack of a better value
marine_animals_median <- updated_price %>% 
  select(species, year, exvessel) %>% 
  distinct() %>% 
  group_by(year) %>% 
  summarize(marine_animals_median = median(exvessel, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(species = "Marine animals")

# Check correlation between these
all_prices <- updated_price %>% 
  left_join(price_species, by = c("species", "year")) %>% 
  left_join(price_genus, by = c("genus", "year")) %>% 
  left_join(price_family, by = c("family", "year")) %>% 
  left_join(price_order, by = c("order", "year")) %>% 
  left_join(price_class, by = c("class", "year")) %>% 
  left_join(price_isscaap_group, by = c("isscaap_group", "year")) %>%
  select(contains("median")) %>% 
  magrittr::set_colnames(value = str_replace_all(str_remove_all(colnames(.), "_median"), "_", " "))

correlogram <- all_prices %>%
  magrittr::set_colnames(value = str_to_sentence(colnames(.))) %>%
  cor(use = "pairwise.complete.obs") %>%
  ggcorrplot(type = "lower",
             lab = TRUE,
             colors = c("red", "white", "steelblue"),
             outline.color = "black") +
  startR::ggtheme_plot() +
  labs(x = "Taxon or group", y = "Taxon or group") +
  guides(fill = guide_colorbar(title = "Pearson's\nR",
                               frame.colour = "black",
                               ticks.colour = "black"))

startR::lazy_ggsave(correlogram,
                    filename = "correlogram_exvessel_price",
                    width = 12, 
                    height = 12)

# Read taxa codes from the reg watson dataset and perform some cleaning
taxa_codes <- readxl::read_excel(file.path(rw_path, "Codes.xlsx"), sheet = 3L) %>% 
  rename(species = TaxonName) %>% 
  janitor::clean_names() %>% 
  mutate(tmp_spp = species,
         species = my_validate(species)) %>% 
  mutate(species = case_when(species == "Clupeoids" ~ "Clupeidae",
                             species == "Scombroids" ~ "Scombridae",
                             species == "Macroramphosidae" ~ "Centriscidae",
                             species == "Inermiidae" ~ "Haemulidae",
                             species == "Starfish and other echinoderms" ~ "Asteroidea",
                             T ~ species)) %>% 
  mutate(genus = str_extract(species, pattern = "^[:alpha:]+")) %>% 
  left_join(taxonomy_genus_family, by = "genus") %>% 
  mutate(family = ifelse(is.na(family) & genus %in% families, genus, family)) %>% 
  left_join(taxonomy_family_order, by = "family") %>% 
  mutate(order = ifelse(is.na(order) & genus %in% orders, genus, order)) %>% 
  left_join(taxonomy_order_class, by = "order") %>% 
  mutate(class = ifelse(is.na(class) & genus %in% classes, genus, class)) %>% 
  mutate(species = ifelse(species == genus, NA_character_, species),
         species = ifelse(species == family, NA_character_, species),
         species = ifelse(species == order, NA_character_, species),
         species = ifelse(species == class, NA_character_, species)) %>% 
  mutate(genus = ifelse(genus == family, NA_character_, genus),
         genus = ifelse(genus == order, NA_character_, genus),
         genus = ifelse(genus == class, NA_character_, genus),
         species = ifelse(is.na(species) & is.na(genus) & is.na(family) & is.na(order) & is.na(class), tmp_spp, species)) %>% 
  select(-tmp_spp)

# Combine datasets
combined <- taxa_codes %>%
  expand_grid(year = c(2005:2015)) %>% 
  left_join(price_species, by = c("species", "year")) %>% 
  left_join(price_genus, by = c("genus", "year")) %>%
  left_join(price_family, by = c("family", "year")) %>%
  left_join(price_order, by = c("order", "year")) %>%
  left_join(price_class, by = c("class", "year")) %>%
  left_join(price_isscaap_group, by = c("species" = "isscaap_group", "year")) %>%
  left_join(price_isscaap_group, by = c("common_name" = "isscaap_group", "year")) %>%
  left_join(marine_animals_median, by = c("species", "year")) %>%
  mutate(price = species_median,
         price = ifelse(is.na(price), genus_median, price),
         price = ifelse(is.na(price), family_median, price),
         price = ifelse(is.na(price), order_median, price),
         price = ifelse(is.na(price), class_median, price),
         price = ifelse(is.na(price), isscaap_group_median.x, price),
         price = ifelse(is.na(price), isscaap_group_median.y, price),
         price = ifelse(is.na(price), marine_animals_median, price)) %>% 
  select(year, taxon_key, class, order, family, genus, species, common_name, price)


# Save results to disk
saveRDS(combined, file.path(project_path, "processed_data", "taxa_codes_and_prices.rds"))
