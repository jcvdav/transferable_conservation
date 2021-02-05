
# Load packages
library(tidyverse)
library(rfishbase)

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


price <- read.csv(
  file.path(
    clean_seafod_path,
    "exvessel_price_database_1976_2017.csv"
  ),
  stringsAsFactors = F) %>% 
  as_tibble() %>% 
  janitor::clean_names() %>% 
  filter(year == 2017) %>% 
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
         asfis_species = str_trim(asfis_species)) %>% 
  rename(species = scientific_name) %>% 
  mutate(species = my_validate(species)) %>% 
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

redundant_prices_species_genus <- price %>%
  group_by(genus) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(species == genus, n > 1) %>% 
  pull(species)

redundant_prices_species_family <- price %>%
  group_by(family) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(species == family, n > 1) %>% 
  pull(species)

redundant_prices_species_order <- price %>%
  group_by(order) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(species == order, n > 1) %>% 
  pull(species)

redundant_prices_species_class <- price %>%
  group_by(class) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(species == class, n > 1) %>% 
  pull(species)

price <- price %>% 
  filter(!species %in% c(redundant_prices_species_genus,
                         redundant_prices_species_family,
                         redundant_prices_species_order,
                         redundant_prices_species_class))



# Checks
# filter(price, is.na(class)) %>% select(species, genus, family, order, class) %>% View("price_missing_class")

# if family is NA AND genus IN family, genus is family


summarize_group <- function(x, group) {
  varname <- paste0(group, c("_median"))
  x %>% 
    group_by_at(vars(one_of(group))) %>% 
    summarize(median = median(exvessel, na.rm = T)) %>% 
    magrittr::set_colnames(c(group, varname)) %>% 
    drop_na()
}

price_species <- price %>% 
  summarize_group("species")

price_genus <- price %>% 
  summarize_group("genus")

price_family <- price %>% 
  summarize_group("family")

price_order <- price %>% 
  summarize_group("order")

price_class <- price %>% 
  summarize_group("class")

price_isscaap_group <- price %>% 
  summarize_group("isscaap_group")

marine_animals_median <- price %>% 
  select(species, exvessel) %>% 
  distinct() %>% 
  pull(exvessel) %>% 
  median(na.rm = T)


# Check correlation between these
all_prices <- price %>% 
  left_join(price_species, by = "species") %>% 
  left_join(price_genus, by = "genus") %>% 
  left_join(price_family, by = "family") %>% 
  left_join(price_order, by = "order") %>% 
  left_join(price_class, by = "class") %>% 
  left_join(price_isscaap_group, by = "isscaap_group") %>%
  select(exvessel, contains("median")) %>% 
  magrittr::set_colnames(value = str_replace_all(str_remove_all(colnames(.), "_median"), "_", " "))

corrplot::corrplot(corr = cor(all_prices, use = "pairwise.complete.obs"),
                   method = "ellipse",
                   type = "upper",
                   outline = T,
                   addCoef.col = "black",
                   diag = F,
                   number.cex = 0.75)

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

combined <- taxa_codes %>%
  left_join(price_species, by = "species") %>% 
  left_join(price_genus, by = "genus") %>% 
  left_join(price_family, by = "family") %>% 
  left_join(price_order, by = "order") %>% 
  left_join(price_class, by = "class") %>% 
  left_join(price_isscaap_group, by = c("species" = "isscaap_group")) %>%
  left_join(price_isscaap_group, by = c("common_name" = "isscaap_group")) %>%
  mutate(price = species_median,
         price = ifelse(is.na(price), genus_median, price),
         price = ifelse(is.na(price), family_median, price),
         price = ifelse(is.na(price), order_median, price),
         price = ifelse(is.na(price), class_median, price),
         price = ifelse(is.na(price), isscaap_group_median.x, price),
         price = ifelse(is.na(price), isscaap_group_median.y, price),
         price = ifelse(species == "Marine animals" & is.na(price), marine_animals_median, price)) %>% 
  select(taxon_key, class, order, family, genus, species, common_name, price)


# Save results to disk
saveRDS(combined, file.path(project_path, "processed_data", "taxa_codes_and_prices.rds"))
