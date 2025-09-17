library(ggplot2)
library(plotly)
library(dplyr)
library(readr)
library(tigris)
library(sf)

# ---------------------------
# Summary Table
# ---------------------------
setwd("C:/Users/matth/Desktop/Undergraduate-Research/GWU-Bootcamp/Final Project")

flood_summary <- read_csv("Data/dc_flood_summary.csv", 
                          col_types = cols(GEOID = col_character()))
head(flood_summary)

# Load DC census tracts shapefile from tigris
dc_tracts <- tracts(state = "DC", year = 2020, class = "sf")

# Merge flood data with spatial data
flood_map <- dc_tracts %>%
  left_join(flood_summary, by = "GEOID")

# 100-Year Floodplain plot
flood_plot_100 <- ggplot(flood_map) +
  geom_sf(aes(fill = percent_flood_100,
              text = paste0("GEOID: ", GEOID, "<br>",
                            "100-Year Flood %: ", round(percent_flood_100, 1))),
          color = NA) +
  scale_fill_viridis_c(option = "mako", name = "100-Year Flood %", direction = -1) +
  labs(title = "100-Year Flood Plain in Washington, D.C.",
       subtitle = "Census Tracts (2020)",
       caption = "Source: Open Data DC") +
  theme_minimal()

# Convert to interactive plot
ggplotly(flood_plot_100, tooltip = "text")

# 100-Year Floodplain plot
flood_plot_500 <- ggplot(flood_map) +
  geom_sf(aes(fill = percent_flood_500,
              text = paste0("GEOID: ", GEOID, "<br>",
                            "500-Year Flood %: ", round(percent_flood_500, 1))),
          color = NA) +
  scale_fill_viridis_c(option = "mako", name = "100-Year Flood %", direction = -1) +
  labs(title = "500-Year Flood Plain in Washington, D.C.",
       subtitle = "Census Tracts (2020)",
       caption = "Source: Open Data DC") +
  theme_minimal()

# Convert to interactive plot
ggplotly(flood_plot_500, tooltip = "text")

# Percent Tidal plot
tidal_plot <- ggplot(flood_map) +
  geom_sf(aes(fill = percent_tidal,
              text = paste0("GEOID: ", GEOID, "<br>",
                            "Tidal Zone %: ", round(percent_tidal, 1))),
          color = NA) +
  scale_fill_viridis_c(option = "mako", name = "Tidal Flood %", direction = -1) +
  labs(title = "Tidal Flood Plain in Washington, D.C.",
       subtitle = "Census Tracts (2020)",
       caption = "Source: Open Data DC") +
  theme_minimal()

# Convert to interactive plot
ggplotly(tidal_plot, tooltip = "text")

# Percent Bluespot plot
blue_zones <- ggplot(flood_map) +
  geom_sf(aes(fill = blue_percent,
              text = paste0("GEOID: ", GEOID, "<br>",
                            "Blue Zone %: ", round(blue_percent, 1))),
          color = NA) +
  scale_fill_viridis_c(option = "mako", name = "Blue Zone %", direction = -1) +
  labs(title = "Blue Zones in Washington, D.C.",
       subtitle = "Census Tracts (2020)",
       caption = "Source: Open Data DC") +
  theme_minimal()

# Convert to interactive plot
ggplotly(blue_zones, tooltip = "text")
