# ---------------------------
# BlueSpot Flood Risk Analysis in DC (Full Pipeline, NA → 0)
# ---------------------------

# Install and load required packages
required_pkgs <- c("tigris", "sf", "terra", "whitebox", "elevatr", "exactextractr", "tmap")
new_pkgs <- required_pkgs[!(required_pkgs %in% installed.packages()[, "Package"])]
if (length(new_pkgs)) install.packages(new_pkgs)

library(tigris)
library(sf)
library(terra)
library(whitebox)
library(elevatr)
library(exactextractr)
library(tmap)

#  Load census tracts for DC
options(tigris_use_cache = TRUE)
dc_tracts <- tracts(state = "DC", year = 2020, class = "sf")

# Download DEM (elevation) for DC area
dc_dem <- get_elev_raster(locations = dc_tracts, z = 13, clip = "locations")
writeRaster(dc_dem, "dc_dem.tif", overwrite = TRUE)

#  Fill terrain depressions using WhiteboxTools
wbt_init()
wbt_fill_depressions(
  dem = "dc_dem.tif",
  output = "dc_filled.tif"
)

#  Compute BlueSpots (depth = filled - original DEM)
dem_orig <- rast("dc_dem.tif")
dem_filled <- rast("dc_filled.tif")
bluespots <- dem_filled - dem_orig

#  Remove shallow depressions (depth < 0.1 m)
bluespots[bluespots < 0.1] <- NA
writeRaster(bluespots, "dc_bluespots.tif", overwrite = TRUE)

#  Reproject tracts to match raster CRS
dc_tracts_proj <- st_transform(dc_tracts, crs(bluespots))

#  Calculate flood metrics for each census tract
flood_stats <- exact_extract(bluespots, dc_tracts_proj,
                             fun = function(vals, cov) {
                               data.frame(
                                 mean_depth = mean(vals, na.rm = TRUE),
                                 max_depth = max(vals, na.rm = TRUE),
                                 percent_flooded = 100 * sum(!is.na(vals) & vals > 0) / length(vals)
                               )
                             })

#  Replace NA values with 0 if tract has no significant depressions
flood_stats$mean_depth[is.na(flood_stats$mean_depth)] <- 0
flood_stats$max_depth[is.na(flood_stats$max_depth)] <- 0
flood_stats$percent_flooded[is.na(flood_stats$percent_flooded)] <- 0

# Join metrics to spatial data
dc_tracts_flood <- cbind(dc_tracts_proj, flood_stats)

#  Log-transform mean depth
dc_tracts_flood$log_mean_depth <- log(dc_tracts_flood$mean_depth + 0.01)

#  Plot log-transformed BlueSpot flood depth
tmap_mode("plot")
tm_shape(dc_tracts_flood) +
  tm_polygons(
    fill = "log_mean_depth",
    fill.scale = tm_scale(values = "brewer.blues"),
    fill.legend = tm_legend(title = "log(Mean Depth + 0.01)")
  ) +
  tm_title("Log-Transformed BlueSpot Depth per Census Tract in DC")

