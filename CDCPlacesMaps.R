library(tidyverse)
library(sf)
library(tigris)
library(plotly)

options(tigris_use_cache = TRUE)

# Load CDC PLACES 2024 dataset
cdc_data <- read_csv("Data//cdc_places.csv", 
                     col_types = cols(LocationID = col_character()))

# Load DC census tracts shapefile from tigris
dc_tracts <- tracts(state = "DC", year = 2020, class = "sf")

# Separate datasets
asthma_df <- cdc_data %>%
  filter(Measure == "Current asthma among adults")

# Merge asthma and diabetes data
dc_asthma_map <- dc_tracts %>%
  left_join(asthma_df, by = c("GEOID" = "LocationID"))

# ------------------------------------------
# Interactive Plotly Asthma Map
# ------------------------------------------
asthma_plot <- ggplot(dc_asthma_map) +
  geom_sf(aes(
    fill = Data_Value,
    text = paste0("GEOID: ", GEOID, "<br>",
                  "Asthma Rate: ", round(Data_Value, 1), "%")
  ), color = NA) +
  scale_fill_viridis_c(option = "magma", name = "Asthma %", direction = -1) +
  labs(title = "Asthma Prevalence Among Adults (CDC PLACES 2024)",
       subtitle = "Washington, D.C. Census Tracts",
       caption = "Source: CDC PLACES 2024") +
  theme_minimal()

ggplotly(asthma_plot, tooltip = "text")


diabetes_df <- cdc_data %>% 
  filter(Measure == "Diagnosed diabetes among adults")

dc_diabetes_map <- dc_tracts %>%
  left_join(diabetes_df, by = c("GEOID" = "LocationID"))

# ------------------------------------------
# Interactive Plotly Diabetes Map
# ------------------------------------------
diabetes_plot <- ggplot(dc_diabetes_map) +
  geom_sf(aes(
    fill = Data_Value,
    text = paste0("GEOID: ", GEOID, "<br>",
                  "Diabetes Rate: ", round(Data_Value, 1), "%")
  ), color = NA) +
  scale_fill_viridis_c(option = "magma", name = "Diabetes %", direction = -1) +
  labs(title = "Diabetes Prevalence Among Adults (CDC PLACES 2024)",
       subtitle = "Washington, D.C. Census Tracts",
       caption = "Source: CDC PLACES 2024") +
  theme_minimal()

ggplotly(diabetes_plot, tooltip = "text")
