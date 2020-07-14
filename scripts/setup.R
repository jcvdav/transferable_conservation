##########################
## Paths to directories ##
##########################
# Check for OS
sys_path <- ifelse(Sys.info()["sysname"]=="Windows", "~/G:/","~/Google Drive File Stream")
# Nat geo ocean conservation priorities path
ng_data_path <- paste0(sys_path,"Shared drives/emlab/projects/current-projects/ocean-conservation-priorities/data")
# Path to our emLab's data folder
data_path <- paste0(sys_path,"Shared drives/emlab/data/")
# Path to this projects directory
project_path <- "~/Google Drive File Stream/Shared drives/emlab/projects/current-projects/transferable-conservation"


###########################
## Useful general values ##
###########################
# proj4strings
proj_moll <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs " # Mollweide - EPSG code ESRI:54009
proj_longlat <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "              # Unprojected coordinates - EPSG code 4326

# A color palete
zis_pal <- wesanderson::wes_palette("Zissou1", 100, type = "continuous")
mat_pal <- colorRamps::matlablike(100)
