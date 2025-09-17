# Load required libraries
library(readr)
library(terra)
library(sf)
library(whitebox)
library(exactextractr)
library(tigris)
library(dplyr)
library(tidyr)
library(ggplot2)

options(tigris_use_cache = TRUE)

# ---------------------------
# Load base spatial data
# ---------------------------

# set working directory
setwd("C:/Users/matth/Desktop/Undergraduate-Research/GWU-Bootcamp/Final Project")

# Load DC census tracts
dc_tracts <- tracts(state = "DC", year = 2020, class = "sf")

# Load floodplain shapefile
flood_sf <- st_read("Data/Floodplains/Floodplains.shp")

# Load tidal shoreline flood hazard buffer
tidal_sf <- st_read("Data/Tidal_Shoreline_Flood_Hazard_Area/Tidal_Shoreline_Flood_Hazard_Area.shp")

# Load original DTM (Hydro-Flattened)
dem_orig <- rast("Data/OpenDataDC_LiDAR_DTM_2020/DTM.tif")
writeRaster(dem_orig, "dc_dem_26985.tif", overwrite = TRUE)

# Fill depressions
wbt_init()
wbt_fill_depressions(
  dem = "dc_dem_26985.tif",
  output = "dc_filled_26985.tif"
)

# Compute bluespots
dem_filled <- rast("dc_filled_26985.tif")
bluespots <- dem_filled - dem_orig
bluespots[bluespots < 0.1] <- NA
writeRaster(bluespots, "dc_bluespots_26985.tif", overwrite = TRUE)

# ---------------------------
# Ensure consistent CRS
# ---------------------------

target_crs <- st_crs(dc_tracts)
flood_sf <- st_transform(flood_sf, target_crs)
tidal_sf <- st_transform(tidal_sf, target_crs)

# ---------------------------
# Floodplain Analysis (100-year and 500-year)
# ---------------------------
library(sf)
library(dplyr)
library(tidyr)

# Ensure flood polygons are valid
flood_sf <- st_make_valid(flood_sf)

# Reproject flood data to match tract CRS (if needed)
flood_sf <- st_transform(flood_sf, st_crs(dc_tracts))

# Recalculate total tract area
dc_tracts <- dc_tracts %>%
  mutate(total_area = st_area(.))

# Drop any previous flood-related columns (optional cleanup)
dc_tracts <- dc_tracts %>%
  select(-any_of(c(
    "flood_area_100", "percent_flood_100",
    "flood_area_500", "percent_flood_500"
  )))

# --- 100-year flood zone (A, AE) ---
flood_100 <- flood_sf %>%
  filter(FLD_ZONE %in% c("A", "AE"))

intersection_100 <- st_intersection(dc_tracts, flood_100) %>%
  mutate(flood_area_100 = st_area(.)) %>%
  st_drop_geometry() %>%
  group_by(GEOID) %>%
  summarise(flood_area_100 = sum(as.numeric(flood_area_100)), .groups = "drop")

# --- 500-year flood zone (X500) ---
flood_500 <- flood_sf %>%
  filter(ZONE %in% c("X500"))

intersection_500 <- st_intersection(dc_tracts, flood_500) %>%
  mutate(flood_area_500 = st_area(.)) %>%
  st_drop_geometry() %>%
  group_by(GEOID) %>%
  summarise(flood_area_500 = sum(as.numeric(flood_area_500)), .groups = "drop")

# --- Join intersections and compute percentages ---
dc_tracts <- dc_tracts %>%
  left_join(intersection_100, by = "GEOID") %>%
  left_join(intersection_500, by = "GEOID") %>%
  mutate(
    flood_area_100 = replace_na(flood_area_100, 0),
    percent_flood_100 = 100 * flood_area_100 / as.numeric(total_area),
    flood_area_500 = replace_na(flood_area_500, 0),
    percent_flood_500 = 100 * flood_area_500 / as.numeric(total_area)
  )

# ---------------------------
# Tidal Shoreline Buffer Analysis
# ---------------------------

# Attempt geometry repair if needed
tidal_sf <- st_make_valid(tidal_sf)

intersection_tidal <- st_intersection(dc_tracts, tidal_sf) %>%
  mutate(tidal_area = st_area(.)) %>%
  st_drop_geometry() %>%
  group_by(GEOID) %>%
  summarise(tidal_area = sum(as.numeric(tidal_area)), .groups = "drop")

# Join and calculate percent
dc_tracts <- dc_tracts %>%
  left_join(intersection_tidal, by = "GEOID") %>%
  mutate(
    tidal_area = replace_na(tidal_area, 0),
    percent_tidal = 100 * tidal_area / as.numeric(total_area)
  )

# ---------------------------
# Bluespot Analysis
# ---------------------------

dc_tracts$blue_percent <- exact_extract(bluespots, dc_tracts, fun = function(vals, cov) {
  if (all(is.na(vals))) return(0)
  total_area <- sum(cov)
  flooded_area <- sum(cov[!is.na(vals) & vals > 0])
  100 * flooded_area / total_area
})

dc_tracts$blue_percent[is.na(dc_tracts$blue_percent)] <- 0

# ---------------------------
# Summary Table
# ---------------------------

# Update percent_flood_500 to include 100-year + 500-year coverage
dc_tracts <- dc_tracts %>%
  mutate(percent_flood_500 = percent_flood_100 + percent_flood_500)
# Then create summary table
dc_summary <- dc_tracts %>%
  st_drop_geometry() %>%
  select(GEOID, percent_flood_100, percent_flood_500, percent_tidal, blue_percent)
print(dc_summary)
write.csv(dc_summary, "dc_flood_summary.csv", row.names = FALSE)
