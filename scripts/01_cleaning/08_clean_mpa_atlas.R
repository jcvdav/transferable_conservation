################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
#
# Keep category Ia, Ib, and II. These are the only ones
# that have a focus on ecosystem processes:
# 
# https://www.iucn.org/theme/protected-areas/about/protected-areas-categories/category-ii-national-park
# 
# Category Ia protected areas are strictly protected areas, generally with only limited human visitation. 
# 
# Category Ib and II protected areas are often similar in size and in their aim to protect functioning ecosystems.
# 
# Category III protected areas are generally centred on a particular natural feature, so that the primary focus of management is on maintaining this feature, whereas objectives of Ia are generally aimed at a whole ecosystem and ecosystem processes.
# 
# Category IV protected areas protect fragments of ecosystems or habitats, which often require continual management intervention to maintain.
# 
# Category V protected areas are generally cultural landscapes or seascapes that have been altered by humans over hundreds or even thousands of years and that rely on continuing intervention to maintain their qualities including biodiversity.
# 
# Category VI protected areas contain natural areas where biodiversity conservation is linked with sustainable use of natural resources, which is incompatible with category Ia. 
#
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacmann::p_load(
  janitor,
  sf,
  tidyverse
)

sf_use_s2(F)

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
mpas <-
  st_read(
    file.path(data_path, "mpa-atlas/mpatlas_20201223_clean"),
    "mpatlas_20201223_clean"
  ) %>%
  clean_names() %>%
  filter(iucn_categ %in% c("Ia", "Ib", "II"),
         implemente == 1) %>%
  select(mpa_id, wdpa_id, iso3 = sovereign)

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
st_write(mpas,
         here("clean_data", "clean_mpa_atlas.gpkg"))



