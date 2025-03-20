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
  
#Withdrawals
water_withdrawals <- USGS %>%
  select(
    `County Name`, `Year`, `Population`,
    contains("total self-supplied withdrawals")) %>%
  select(!contains("saline")) %>%
  select(!contains("deliveries")) 
  

#Population Table 
Population <- water_withdrawals %>%
  group_by(Year) %>%
  summarise(Total_Population = sum(Population, na.rm = TRUE))

# Public Supply

public <- USGS %>%
  select(
    `County Name`, `Year`, `Population`,
    contains("self-supplied withdrawals")) %>%
  select(`County Name`, `Year`, `Population`,
         contains("public supply")) %>%
  select(`County Name`, `Year`, `Population`,
         !contains("saline"))

# Domestic

domestic <- USGS %>%
  select(
    `County Name`, `Year`, `Population`,
    contains("self-supplied withdrawals")) %>%
  select(
    `County Name`, `Year`, `Population`,
    contains("Domestic")) %>%
  select(
    `County Name`, `Year`, `Population`,
    !contains("deliveries")) %>%
  select(`County Name`, `Year`, `Population`,
         !contains("saline"))

# Irrigation

irrigation <- USGS %>%
  select(
    `County Name`, `Year`, `Population`,
    contains("self-supplied withdrawals")) %>%
  select(
      `County Name`, `Year`, `Population`,
      contains("irrigation")) %>%
  select(`County Name`, `Year`, `Population`,
         !contains("saline"))

# Thermoelectric Power

thermo <- USGS %>%
  select(
    `County Name`, `Year`, `Population`,
    contains("self-supplied withdrawals")) %>%
  select(
    `County Name`, `Year`, `Population`,
    contains("total thermo")) %>%
  select(`County Name`, `Year`, `Population`,
         !contains("saline"))

# Industrial

industiral <- USGS %>%
  select(
    `County Name`, `Year`, `Population`,
    contains("self-supplied withdrawals")) %>%
  select(
    `County Name`, `Year`, `Population`,
    contains("Industrial")) %>%
  select(
    `County Name`, `Year`, `Population`,
    !contains("deliveries"))%>%
  select(`County Name`, `Year`, `Population`,
         !contains("saline"))

# Mining

mining <- USGS %>%
  select(
    `County Name`, `Year`, `Population`,
    contains("self-supplied withdrawals")) %>%
  select(
    `County Name`, `Year`, `Population`,
    contains("mining")) %>%
  select(`County Name`, `Year`, `Population`,
         !contains("saline"))

# Livestock

livestock <- USGS %>%
  select(
    `County Name`, `Year`, `Population`,
    contains("self-supplied withdrawals")) %>%
  select(
    `County Name`, `Year`, `Population`,
    contains("livestock")) %>%
  select(`County Name`, `Year`, `Population`,
         !contains("saline"))

# Aquaculture

aquaculture <- USGS %>%
  select(
    `County Name`, `Year`, `Population`,
    contains("self-supplied withdrawals")) %>%
  select(
    `County Name`, `Year`, `Population`,
    contains("aquaculture")) %>%
  select(`County Name`, `Year`, `Population`,
         !contains("saline"))


#Population Table 

Population <- water_withdrawals %>%
  group_by(Year) %>%
  summarise(Total_Population = sum(Population, na.rm = TRUE))

plot1 <- ggplot(data= Population, aes(x= Year, y= Total_Population )) +
  geom_line(color = "navy", size = 1) +
  geom_point(color = "gold", size = 2) + 
  scale_x_continuous(breaks = seq(min(Population$Year), max(Population$Year), by = 5)) +
  labs(title = "Total Population Over Time", x = "Year", y = "Total Population") 

plot1



















