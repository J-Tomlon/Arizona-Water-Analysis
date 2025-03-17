library(tidyverse)
library(readr)
library(ggplot2)

setwd("../Arizona-Water-Analysis")
USGS <- read_csv("raw-data/USGS Water Use Data for All of Arizona.csv")

USGS <- USGS %>%
  mutate(across(-c(`State Code`, `State Name`, `County Code`, `County Name`, `Year`, 
                   `Total Population total population of area, in thousands`), as.character)) %>%
  rename(Population = 'Total Population total population of area, in thousands')
  

water_withdrawals <- USGS %>%
  select(
    `County Name`, `Year`, `Population`,
    contains("total self-supplied withdrawals") & contains("total, in Mgal/d")
  )

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





















