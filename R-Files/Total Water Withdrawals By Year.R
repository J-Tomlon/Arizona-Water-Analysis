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


# totals graph

totals <- USGS %>%
  select(Year,
         Public.Supply = `Public Supply total self-supplied withdrawals, total, in Mgal/d`,
         Domestic = `Domestic total self-supplied withdrawals, fresh, in Mgal/d`,
         Irrigation = `Irrigation, Total total self-supplied withdrawals, fresh, in Mgal/d`,
         Thermoelectric = `Total Thermoelectric Power total self-supplied withdrawals, total, in Mgal/d`,
         Industrial = `Industrial total self-supplied withdrawals, in Mgal/d`,
         Mining = `Mining total self-supplied withdrawals, in Mgal/d`,
         Livestock = `Livestock total self-supplied withdrawals, fresh, in Mgal/d`,
         Aquaculture = `Aquaculture total self-supplied withdrawals, in Mgal/d`) 

for(i in 2:ncol(totals)) {
  totals[[i]] <- as.numeric(as.character(totals[[i]]))
}

yearly_totals <- totals %>%
  group_by(Year) %>%
  summarize(
    Public.Supply = sum(Public.Supply, na.rm = TRUE),
    Domestic = sum(Domestic, na.rm = TRUE),
    Irrigation = sum(Irrigation, na.rm = TRUE),
    Thermoelectric = sum(Thermoelectric, na.rm = TRUE),
    Industrial = sum(Industrial, na.rm = TRUE),
    Mining = sum(Mining, na.rm = TRUE),
    Livestock = sum(Livestock, na.rm = TRUE),
    Aquaculture = sum(Aquaculture, na.rm = TRUE)
  )

yearly_totals$Total <- rowSums(yearly_totals[, 2:9], na.rm = TRUE)


total_usage_plot <- ggplot(yearly_totals, aes(x = Year, y = Total)) +
  geom_line(color = "navy", size = 1.2) +  # Professional blue
  geom_point(color = "gold", size = 3, alpha = 0.8) +  # Contrasting red points
  labs(
    title = "Total Arizona Water Withdrawals by Year",
    subtitle = "(Million Gallons per Day)",
    x = "Year",
    y = "Total Withdrawals (Mgal/d)",
    caption = "Source: USGS Water Usage Dataset"
  ) + 
  theme_minimal(base_size = 14) +  # Slightly larger base text
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),  # Centered title
    plot.subtitle = element_text(size = 14, hjust = 0.5, color = "gray40"),  
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    panel.grid.major = element_line(color = "gray85"),
    panel.grid.minor = element_blank(),  # Remove minor grid lines for clarity
    plot.caption = element_text(size = 10, hjust = 1, color = "gray50")  # Align caption right
  )

total_usage_plot
