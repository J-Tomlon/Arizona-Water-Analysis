library(tidyverse)
library(dplyr)
library(readr)
library(janitor)
library(ggplot2)

setwd("../Arizona-Water-Analysis")
USGS <- read_csv("raw-data/USGS Water Use Data for All of Arizona.csv")

USGS <- USGS %>%
  mutate(across(-c(`State Code`, `State Name`, `County Code`, `County Name`, `Year`, 
                   `Total Population total population of area, in thousands`), as.character)) %>%
  rename(Population = 'Total Population total population of area, in thousands')

public.deliveries <- USGS %>%
  select("County Name", "Year", "Population",
         contains("Public Supply Deliveries")
  )

public.deliveries <- public.deliveries %>%
  mutate(across(where(is.character), ~ifelse(. == "-", NA, .))) %>%
  mutate(across(where(is.character), ~as.numeric(.)))

colnames(public.deliveries) <- gsub("Public Supply deliveries to ", "", colnames(public.deliveries))
colnames(public.deliveries) <- gsub(", in Mgal/d", "", colnames(public.deliveries))


yearly_data <- public.deliveries %>%
  group_by(Year) %>%
  summarize(
    domestic = mean(domestic, na.rm = TRUE),
    commercial = mean(commercial, na.rm = TRUE),
    industrial = mean(industrial, na.rm = TRUE),
    thermoelectric = mean(thermoelectric, na.rm = TRUE)
  )


long_data <- yearly_data %>%
  pivot_longer(
    cols = c("domestic", "commercial", "industrial", "thermoelectric"),
    names_to = "Sector",
    values_to = "Delivery"
  )

ggplot(long_data, aes(x = Year, y = Delivery, color = Sector, group = Sector)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Public Supply Deliveries to Different Sectors Over Time",
    subtitle = "Mean deliveries across all counties (in Mgal/d)",
    x = "Year",
    y = "Delivery (Mgal/d)",
    color = "Sector"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom"
  )
