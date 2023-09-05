##########################
## Paths to directories ##
##########################
# Check for OS
sys_path <- ifelse(Sys.info()["sysname"]=="Windows", "~/G:/","/Users/juancarlosvillasenorderbez/Library/CloudStorage/GoogleDrive-juancarlos@ucsb.edu")
# Nat geo ocean conservation priorities path
ng_data_path <- file.path(sys_path,"Shared drives/emlab/projects/current-projects/ocean-conservation-priorities/data")
# Path to our emLab's data folder
data_path <- file.path(sys_path,"Shared drives/emlab/data")
# Path to this projects directory
project_path <- file.path(sys_path,"Shared drives/emlab/projects/current-projects/transferable-conservation")
# Create common pointer to RegWatson data
rw_path <- file.path(data_path, "reg-watson-global-marine-capture", "global_fisheries_landing_v4")
# Pointer to cleanseafood path
clean_seafod_path <- file.path(sys_path, "Shared drives/emlab/projects/current-projects/clean-seafood/project-materials/track-3-olivier/raw-data/reconstructed-global-prices/price-db-results")


# Source project functions
source(here::here("scripts", "00_setup","02_functions.R"))

# Turn off dplyr's anoying messages
options(dplyr.summarise.inform = FALSE)


###########################
## Useful general values ##
###########################
# proj4strings
proj_moll <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs "             # Mollweide - EPSG code ESRI:54009
proj_longlat <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "                         # Unprojected coordinates - EPSG code 4326

# EPSG codes
epsg_moll <- "ESRI:54009"
epsg_longlat <- 4326

# Turn off the use of espherical geometries
sf::sf_use_s2(FALSE)

##########################
## Plot-related things ##
##########################
# Default geometry colors, fills, and shapes

ggplot2::update_geom_defaults(geom = "col", list(color = "black",
                                        fill = "#173c66"))

ggplot2::update_geom_defaults(geom = "point", list(color = "black",
                                          fill = "#173c66",
                                          shape = 21,
                                          size = 3))

ggplot2::update_geom_defaults(geom = "line", list(color = "#173c66",
                                         size = 0.2))

ggplot2::update_geom_defaults(geom = "sf", list(color = "black",
                                       fill = "gray50",
                                       size = 0.1))


ggplot2::theme_set(
  ggplot2::theme_bw()
)
ggplot2::theme_update(
  panel.grid.major = ggplot2::element_blank(),
  line = ggplot2::element_line(color = "black",
                               linewidth = 0.176389),
  panel.grid.minor = ggplot2::element_blank(),
  legend.background = ggplot2::element_blank(),
  legend.key = ggplot2::element_blank(),
  strip.background = ggplot2::element_blank(),
  strip.text = ggplot2::element_text(hjust = 0),
  text = ggplot2::element_text(size = 6),
)
