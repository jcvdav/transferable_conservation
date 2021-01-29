

# Load packages
library(raster)
library(data.table)
library(tidyverse)
library(rfishbase)

# Create common pointer to RegWatson data
rw_path <- file.path(data_path, "reg-watson-global-marine-capture", "global_fisheries_landing_v4")

# Read in catch data
## List the files
ind <- list.files(path = file.path(rw_path, "industrial"),
                          pattern = "*.csv",
                          full.names = T) %>% 
  tail(2)

Nind <- list.files(path = file.path(rw_path, "nonindustrial"),
                    pattern = "*.csv",
                    full.names = T) %>% 
  tail(2)


catch_files <- c(ind, Nind)

# Read all
dt <- map_dfr(catch_files, fread, sep = ",")
setkey(dt , Cell, ID)

# Read in the codes
cell_codes <- readxl::read_excel(file.path(rw_path, "Codes.xlsx"), sheet = 1L) %>% 
  as.data.table(key = "Cell")

taxa_codes <- readxl::read_excel(file.path(rw_path, "Codes.xlsx"), sheet = 3L)

# Read indices
index <- list.files(rw_path, "*.csv", full.names = T) %>% 
  map_dfr(fread, key = "ID") %>% 
  .[, ":=" (Year = IYear, TaxonKey = Taxonkey)] %>%
  .[Year >= 2010, .(ID, Year, TaxonKey)]
  

merged <- dt %>% 
  merge(index, all.x = TRUE, by = "ID") %>% 
  merge(taxa_codes, all.x = TRUE, by = "TaxonKey") %>% 
  merge(cell_codes, all.x = TRUE, by = "Cell")

setkey(merged, Year, Cell)

summary <- merged %>% 
  .[!is.na(price)] %>% 
  .[, .(Reported = sum(Reported, na.rm = T),
        Revenue = sum(Reported * price, na.rm = T)),
    by = .(Year, Cell, LatCentre, LonCentre)] %>%
  .[,.(Reported = mean(Reported, na.rm = T),
       Revenue = mean(Revenue, na.rm = T)),
    by = .(Cell, LatCentre, LonCentre)]

benefits_raster <-
  raster(
    file.path(
      project_path,
      "processed_data",
      "suitability.tif"
    )
  )

crs(benefits_raster) <- proj_moll
  
watson_raster <- select(summary, LonCentre, LatCentre, Reported) %>%
  raster::rasterFromXYZ(crs = proj_longlat) %>% 
  raster::projectRaster(benefits_raster)

sum(raster::values(watson_raster), na.rm = T)

raster::plot(watson_raster)

writeRaster(watson_raster,
            filename = file.path(project_path, "processed_data", "catch_raster.tif"),
            overwrite = TRUE)

summary %>% 
  sample_n(5e4) %>% 
  ggplot(aes(x = LonCentre, y = LatCentre, fill = Reported)) +
  geom_tile() +
  scale_fill_viridis_c(trans = "log10") +
  theme(text = element_text(family = "Times", face = "bold", color = "red", size = 24), axis.text.x = element_text(color = "blue"))





######## Tyler's price dataset
clean_seafod_path <- file.path(sys_path, "Shared drives/emlab/projects/current-projects/clean-seafood/project-materials/track-3-olivier/raw-data/reconstructed-global-prices/price-db-results/exvessel_price_database_1976_2017.csv")

sealifebase_names <- load_taxa(server = "sealifebase") %>%
  as_tibble() %>%
  select(Species, Genus, Family, Order, Class)

fishbase_names <- load_taxa(server = "fishbase") %>%
  as_tibble() %>%
  select(Species, Genus, Family, Order, Class)

taxonomy <- rbind(sealifebase_names, fishbase_names) %>% 
  select(Genus, Family, Order, Class) %>% 
  distinct() %>% 
  janitor::clean_names() %>% 
  mutate(genus = ifelse(genus == "Not assigned", NA, genus),
         family = ifelse(family == "Not assigned", NA, family),
         order = ifelse(order == "Not assigned", NA, order),
         class = ifelse(class == "Not assigned", NA, class)) %>% 
  filter(!(genus == "Mytilus" & class == "Bivalva"),
         !(genus == "Pharus" & is.na(order))) 

families <- unique(taxonomy$family)
orders <- unique(taxonomy$order)
classes <- unique(taxonomy$class)

taxonomy_genus_family <- taxonomy %>% 
  select(genus, family) %>% 
  distinct() %>% 
  drop_na(genus)

taxonomy_family_order <- taxonomy %>% 
  select(family, order) %>% 
  distinct() %>% 
  drop_na(family)

taxonomy_order_class <- taxonomy %>% 
  select(order, class) %>% 
  distinct() %>% 
  drop_na(order)

my_validate <- function(species_list){
  # browser()
  tmp <- data.frame(input = species_list, stringsAsFactors = FALSE)

  results_fishbase <- synonyms(species_list, server = "fishbase") %>%
    select(input = synonym, Status, Species) %>%
    distinct()
  
  results_sealifebase <- synonyms(species_list, server = "sealifebase") %>%
    select(input = synonym, Status, Species) %>%
    distinct()
  
  results <- rbind(results_fishbase, results_sealifebase) %>% 
    drop_na(Species)
  
  accepted <- results %>% 
    filter(Status == "accepted name") %>% 
    select(input, accepted_name = Species)
  
  synonyms <- results %>% 
    filter(Status == "synonym") %>% 
    select(input, synonym = Species)
  
  final <- tmp %>% 
    left_join(accepted, by = "input") %>% 
    left_join(synonyms, by = "input") %>% 
    mutate(Species = ifelse(is.na(accepted_name), synonym, accepted_name),
           Species = ifelse(is.na(Species), input, Species))
  
  return(final$Species)

}


price <- read.csv(clean_seafod_path, stringsAsFactors = F) %>% 
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
  # left_join(price_asfis, by = "ASFIS_species") %>% 
  # left_join(price_pool_com, by = "pooled_commodity") %>% 
  # left_join(price_isscaap_division, by = "ISSCAAP_division") %>% 
  select(exvessel, contains("median")) %>% 
  magrittr::set_colnames(value = str_replace_all(str_remove_all(colnames(.), "_median"), "_", " "))

corrplot::corrplot(corr = cor(all_prices, use = "pairwise.complete.obs"),
                   method = "ellipse",
                   type = "lower",
                   outline = T,
                   addCoef.col = "black",
                   diag = F,
                   number.cex = 0.75)

# Why does spp acanthuridae in taxa codes has no family?

taxa_codes <- readxl::read_excel(file.path(rw_path, "Codes.xlsx"), sheet = 3L) %>% 
  rename(species = TaxonName) %>% 
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
  # filter(!species == "Inermiidae") %>% 
  left_join(price_species, by = "species") %>% 
  left_join(price_genus, by = "genus") %>% 
  left_join(price_family, by = "family") %>% 
  left_join(price_order, by = "order") %>% 
  left_join(price_class, by = "class") %>% 
  left_join(price_isscaap_group, by = c("species" = "isscaap_group")) %>%
  left_join(price_isscaap_group, by = c("CommonName" = "isscaap_group")) %>%
  mutate(price = species_median,
         price = ifelse(is.na(price), genus_median, price),
         price = ifelse(is.na(price), family_median, price),
         price = ifelse(is.na(price), order_median, price),
         price = ifelse(is.na(price), class_median, price),
         price = ifelse(is.na(price), isscaap_group_median.x, price),
         price = ifelse(is.na(price), isscaap_group_median.y, price),
         price = ifelse(species == "Marine animals" & is.na(price), marine_animals_median, price))


# Testing

asfis <- unique(price$asfis_species)
pooled <- unique(price$pooled_commodity)
isscaap <- unique(price$isscaap_division)
isscaap2 <- unique(price$isscaap_group)

all <- c(asfis, pooled, isscaap, isscaap2) %>% unique()

filter(combined, is.na(price)) %>% 
  mutate(n_matches = 1 * (species %in% all)) %>% 
  View("unmatched")

  # left_join(price_asfis, by = c("TaxonName" = "ASFIS_species")) %>% 
  # left_join(price_asfis, by = c("CommonName" = "ASFIS_species")) %>% 
  # # left_join(price_pool_com) %>% 
  # left_join(price_isscaap_group, by = c("TaxonName" = "ISSCAAP_group")) %>%
  # left_join(price_isscaap_group, by = c("CommonName" = "ISSCAAP_group")) %>%
  # left_join(price_isscaap_division, by = c("CommonName" = "ISSCAAP_division")) 

# Extra stuff
# 
# 
# my_validate <- function(species_list, server){
#   browser()
#   tmp <- data.frame(input = species_list, stringsAsFactors = FALSE)
#   
#   synonyms(species_list, server = server) %>% 
#     select(input = synonym, Status, Species) %>% 
#     distinct() %>% 
#     filter(Status %in% c("accepted name", "synonym")) %>% 
#     spread(Status, Species) %>% 
#     rename(accepted_name = `accepted name`) %>% 
#     mutate(Species = ifelse(is.na(accepted_name), synonym, accepted_name)) %>% 
#     right_join(tmp, by = "input") %>% 
#     pull(Species)
#   
# }
# 
# my_fun <- function(sci_name1, sci_name2, sci_name3){
#   sci_name <- sci_name3
#   sci_name[is.na(sci_name)] <- sci_name2[is.na(sci_name)]
#   sci_name[is.na(sci_name)] <- sci_name1[is.na(sci_name)]
#   return(sci_name)
# }

