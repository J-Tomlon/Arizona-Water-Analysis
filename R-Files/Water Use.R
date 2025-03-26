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


water.use <- USGS %>%
  select(
    contains("use"), 
    -contains("saline")
    
  )

public.use <- USGS %>%
  select(`State Code`, `State Name`, `County Code`, `County Name`, `Year`, 
    contains("public"),
    -contains("saline")
  )


model <- lm("Public Supply per capita use, in gallon/person/day" ~ "County Name")
