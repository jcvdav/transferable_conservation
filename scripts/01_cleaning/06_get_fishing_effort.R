# Download fishing effort

# Load packages
library(raster)
library(connections)
library(bigrquery)
library(tidyverse)

# Establish a connection to BigQuery
con <- connection_open(
  bigquery(),
  project = "world-fishing-827",
  dataset = "gfw_research",
  billing = "emlab-gcp",
  use_legacy_sql = FALSE,
  allowLargeResults = TRUE
)

# Generate a table of vessel info
vessel_info <- tbl(con, "vi_ssvid_v20190227") %>%
  filter(
    # best$best_vessel_class %in% c("tuna_purse_seines", "drifting_longlines"),     # Keep only this gears
    on_fishing_list_best,                                                         # Keep vessels that are on the fishing list
    activity$overlap_hours < 24 * 3,                                              # Keep vessels with few overlaps
    activity$active_hours > 24,                                                   # Keep active vessels only
    !activity$offsetting                                                          # Exclude any vessels that offset activity
  ) %>%
  mutate(
    length_m = best$best_length_m,                                                # Get the vessel length
    length_factor = case_when(length_m < 50 ~ 0.5,                                # PNA charges half a day to small vessels
                              length_m > 80 ~ 1.5,                                # PNA charges 1.5 X for large vessels
                              T ~ 1),                                             # PNA charges a day for normal vessels
    gear = best$best_vessel_class,                                                # Get the gear
  ) %>%
  select(ssvid, gear, length_m, length_factor)

# Generate a table of eez info (not used right now)
# eez_info <- tbl(con, "eez_info") %>%
# filter(territory1_iso3 %in% pna_iso3)%>%
# pull(eez_id) %>%
# as.character()

# Query the main vessel activity table and create a gridded version
effort_query <- tbl(con, "pipe_v20190502_fishing") %>% 
  filter(nnet_score == 1,
         distance_from_shore_m > 12 * 1854) %>%
  inner_join(vessel_info, by = "ssvid") %>%                   # Keep only purse seiners and longliners
  mutate(
    year = sql("EXTRACT(YEAR FROM date)"),                    # Extract the year from the date
    lat = (floor(lat / 0.1) * 0.1 + 0.05),                    # Grid latitude
    lon = (floor(lon / 0.1) * 0.1 + 0.05),                    # Grid longitude
    vds_hours = hours * length_factor                         # Adjust hours for length-based multipliers
  ) %>%
  filter(year >= 2015,
         year < 2020) %>% 
  group_by(year, lon, lat) %>%                                # Aggregate by space, time, and gear
  summarize(days = sum(hours, na.rm = T) / 24) %>% 
  ungroup() %>% 
  group_by(lon, lat) %>% 
  summarize(days = mean(days, na.rm = T)) %>%                             # Calculate long-term mean
  ungroup()

# Collect the query
effort <- effort_query %>%
  collect()                                                   # Force computation of the query (actually querying now)

# Target raster
benefits <- raster(file.path(ng_data_path, "/03_output/14_upgrade_weak_MPAs/ranking_raster.tif"))

effort_raster <- rasterFromXYZ(effort, crs = proj_longlat) %>% 
  projectRaster(benefits, method = "bilinear")
  
names(effort_raster) <- "costs"

# Export the raster
writeRaster(x = effort_raster,
            filename = file.path(project_path, "data", "fishing_days_as_costs.tif"),
            overwrite = TRUE)

## END OF SCRIPT ##



