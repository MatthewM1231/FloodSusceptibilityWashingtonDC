library(tidyverse)
library(sf)
library(tigris)
library(plotly)
library(dplyr)

options(tigris_use_cache = TRUE)
setwd("C:/Users/matth/Desktop/Undergraduate-Research/GWU-Bootcamp/Final Project")

# Load CDC PLACES 2024 dataset
dp02 <- read_csv("Data//dp02_cleaned.csv", 
                 col_types = cols(geoid = col_character()))
dp03 <- read_csv("Data//dp03_cleaned.csv", 
                 col_types = cols(geoid = col_character()))
dp04 <- read_csv("Data//dp04_cleaned.csv", 
                 col_types = cols(geoid = col_character()))
dp05 <- read_csv("Data//dp05_cleaned.csv", 
                 col_types = cols(geoid = col_character()))

head(dp02)
head(dp03)
head(dp04)
head(dp05)

# Load DC census tracts shapefile from tigris
dc_tracts <- tracts(state = "DC", year = 2020, class = "sf")

# Separate datasets for clarity
race_df <- dp05 %>%
  rename("total_pop" = `race_total population`,
         "total_white" = `race_total population_one race_white`) %>%
  dplyr::select(geoid, total_pop, total_white) %>%
  mutate(minority_rate = 100 * (1 - (total_white / total_pop)))

race_map <- dc_tracts %>%
  left_join(race_df, by = c("GEOID" = "geoid"))

# ------------------------------------------
# Interactive Plotly Race Map
# ------------------------------------------

race_plot <- ggplot(race_map) +
  geom_sf(aes(
    fill = minority_rate,
    text = paste0("GEOID: ", GEOID, "<br>",
                  "% of Minority Race: ", round(minority_rate,1), "%")
  ), color = NA) +
  scale_fill_viridis_c(option = "rocket", name = "Percent of Minority Race",
                       direction = -1) +
  labs(title = "Minority (non-White) Population Rates in Washington D.C. by Census Tracts",
       subtitle = "Washington, D.C. Census Tracts",
       caption = "Source: American Community Survey") +
  theme_minimal()

ggplotly(race_plot, tooltip = "text")


age_df <- dp05 %>%
  rename("total_pop" = `sex and age_total population`,
         "under_5" = `sex and age_total population_under 5 years`,
         "between_65_and_74" = `sex and age_total population_65 to 74 years`,
         "between_75_and_84" = `sex and age_total population_75 to 84 years`,
         "over_85" = `sex and age_total population_85 years and over`) %>%
  dplyr::select(geoid, total_pop, under_5, between_65_and_74, between_75_and_84, over_85) %>%
  mutate(vulnerable_age = 100 * ((under_5 + between_65_and_74 + between_75_and_84 + over_85) / total_pop))

age_map <- dc_tracts %>%
  left_join(age_df, by = c("GEOID" = "geoid"))

# ------------------------------------------
# Interactive Plotly Age Map
# ------------------------------------------

age_plot <- ggplot(age_map) +
  geom_sf(aes(
    fill = vulnerable_age,
    text = paste0("GEOID: ", GEOID, "<br>",
                  "% in Vuln Age Grp: ", round(vulnerable_age,1), "%")
  ), color = NA) +
  scale_fill_viridis_c(option = "rocket", name = "Percent in Vulnerable Age Group: ", direction = -1) +
  labs(title = "Percent of Individuals <5 years old or >65 years old in Washington D.C. by Census Tracts",
       subtitle = "Washington, D.C. Census Tracts",
       caption = "Source: American Community Survey") +
  theme_minimal()

ggplotly(age_plot, tooltip = "text")


# Separate datasets for clarity
housing_df <- dp04 %>%
  rename("total_units" = `year structure built_total housing units`,
         "built_2020_later" = `year structure built_total housing units_built 2020 or later`,
         "built_2010_to_2019" = `year structure built_total housing units_built 2010 to 2019`,
         "built_2000_to_2009" = `year structure built_total housing units_built 2000 to 2009`,
         "built_1990_to_1999" = `year structure built_total housing units_built 1990 to 1999`,
         "built_1980_to_1989" = `year structure built_total housing units_built 1980 to 1989`) %>%
  dplyr::select(geoid, total_units, built_2020_later, built_2010_to_2019, built_2000_to_2009, built_1990_to_1999, built_1980_to_1989) %>%
  mutate(old_home_rate = 100 * (1 - (built_2020_later + built_2010_to_2019 + built_2000_to_2009 + built_1990_to_1999 + built_1980_to_1989) / total_units))

housing_map <- dc_tracts %>%
  left_join(housing_df, by = c("GEOID" = "geoid"))

# ------------------------------------------
# Interactive Plotly Housing Map
# ------------------------------------------

housing_plot <- ggplot(housing_map) +
  geom_sf(aes(
    fill = old_home_rate,
    text = paste0("GEOID: ", GEOID, "<br>",
                  "% of Old Housing Units: ", round(old_home_rate,1), "%")
  ), color = NA) +
  scale_fill_viridis_c(option = "rocket", name = "Percent of Old Housing Units",
                       direction = -1) +
  labs(title = "Percent of Old Housing Units (built before 1980) in Washington D.C. by Census Tracts",
       subtitle = "Washington, D.C. Census Tracts",
       caption = "Source: American Community Survey") +
  theme_minimal()

ggplotly(housing_plot, tooltip = "text")


# Separate datasets for clarity
income_df <- dp03 %>%
  rename("total_pop" = `income and benefits (in 2023 inflation-adjusted dollars)_total households`,
         "less_10000" = `income and benefits (in 2023 inflation-adjusted dollars)_total households_less than $10,000`,
         "between_10000_and_14999" = `income and benefits (in 2023 inflation-adjusted dollars)_total households_$10,000 to $14,999`,
         "between_15000_and_24999" = `income and benefits (in 2023 inflation-adjusted dollars)_total households_$15,000 to $24,999`,
         "between_25000_and_34999" = `income and benefits (in 2023 inflation-adjusted dollars)_total households_$25,000 to $34,999`,
         "between_35000_and_49999" = `income and benefits (in 2023 inflation-adjusted dollars)_total households_$35,000 to $49,999`) %>%
  dplyr::select(geoid, total_pop, less_10000, between_10000_and_14999, between_15000_and_24999, between_25000_and_34999, between_35000_and_49999) %>%
  mutate(low_income_rate = 100 * ((less_10000 + between_10000_and_14999 + between_15000_and_24999 + between_25000_and_34999 + between_35000_and_49999) / total_pop))

income_map <- dc_tracts %>%
  left_join(income_df, by = c("GEOID" = "geoid"))

# ------------------------------------------
# Interactive Plotly Income Map
# ------------------------------------------

income_plot <- ggplot(income_map) +
  geom_sf(aes(
    fill = low_income_rate,
    text = paste0("GEOID: ", GEOID, "<br>",
                  "Low Income Rate: ", round(low_income_rate,1), "%")
  ), color = NA) +
  scale_fill_viridis_c(option = "rocket", name = "Low Income Rate (<$50,000)",
                       direction = -1) +
  labs(title = "Low Income Rate (less than $50,000/year, inflation-adjusted dollars) in Washington D.C. by Census Tracts",
       subtitle = "Washington, D.C. Census Tracts",
       caption = "Source: American Community Survey") +
  theme_minimal()

ggplotly(income_plot, tooltip = "text")


# Separate datasets for clarity
employment_df <- dp03 %>%
  rename("employed_pop" = `employment status_population 16 years and over_in labor force`, 
         "unemployed_pop" = `employment status_population 16 years and over_not in labor force`) %>%
  dplyr::select(geoid, employed_pop, unemployed_pop) %>%
  mutate(unemployment_rate = 100 * (unemployed_pop / (employed_pop + unemployed_pop))) %>%
  filter(!(unemployment_rate > 50 & employed_pop < 200))

employment_map <- dc_tracts %>%
  left_join(employment_df, by = c("GEOID" = "geoid"))

# ------------------------------------------
# Interactive Plotly Employment Map
# ------------------------------------------

employment_plot <- ggplot(employment_map) +
  geom_sf(aes(
    fill = unemployment_rate,
    text = paste0("GEOID: ", GEOID, "<br>",
                  "Unemployment Rate: ", round(unemployment_rate,1), "%")
  ), color = NA) +
  scale_fill_viridis_c(option = "rocket", name = "Unemployment Rate",
                       direction = -1) +
  labs(title = "Unemployment Rate in Washington D.C. by Census Tracts",
       subtitle = "Washington, D.C. Census Tracts",
       caption = "Source: American Community Survey") +
  theme_minimal()

ggplotly(employment_plot, tooltip = "text")