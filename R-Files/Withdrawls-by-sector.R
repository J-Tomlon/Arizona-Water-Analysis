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

withdrawals <- USGS %>%
  select(`County Code`, `County Name`, `Year`, `Population`, 
    contains("withdrawal"), 
    -contains("saline")
  ) %>%
select(
  `County Name`, `Year`, `Population`,
  Public.Supply = `Public Supply total self-supplied withdrawals, total, in Mgal/d`,
  Domestic = `Domestic total self-supplied withdrawals, fresh, in Mgal/d`,
  Irrigation = `Irrigation, Total total self-supplied withdrawals, fresh, in Mgal/d`,
  Thermoelectric = `Total Thermoelectric Power total self-supplied withdrawals, total, in Mgal/d`,
  Industrial = `Industrial total self-supplied withdrawals, in Mgal/d`,
  Mining = `Mining total self-supplied withdrawals, in Mgal/d`,
  Livestock = `Livestock total self-supplied withdrawals, fresh, in Mgal/d`,
  Aquaculture = `Aquaculture total self-supplied withdrawals, in Mgal/d`) 

withdrawals[withdrawals == "-"] <- NA


withdrawal_columns <- c("Public.Supply", "Domestic", "Irrigation", "Thermoelectric", 
                        "Industrial", "Mining", "Livestock", "Aquaculture")
withdrawals[withdrawal_columns] <- lapply(withdrawals[withdrawal_columns], function(x) as.numeric(as.character(x)))

yearly_withdrawals <- withdrawals %>%
  group_by(Year) %>%
  summarize(across(all_of(withdrawal_columns), ~sum(.x, na.rm = TRUE)))


long_data <- yearly_withdrawals %>%
  pivot_longer(cols = all_of(withdrawal_columns),
               names_to = "Sector",
               values_to = "Withdrawal")

ggplot(long_data, aes(x = factor(Year), y = Withdrawal, fill = Sector)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Water Withdrawals by Sector Over Time",
       x = "Year",
       y = "Withdrawal (Mgal/d)",
       fill = "Sector",
       caption = "Source: USGS") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),  #
    plot.subtitle = element_text(size = 14, hjust = 0.5, color = "gray40"),  
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    panel.grid.major = element_line(color = "gray85"),
    panel.grid.minor = element_blank(),  
    plot.caption = element_text(size = 10, hjust = 1, color = "gray50")  
  )

ggsave("withdrawals_by_sector.png", width = 10, height = 6)


############
# Withdrawals per sector 
############

withdrawals.by.sector <- ggplot(long_data, aes(x = Year, y = Withdrawal, color = Sector, group = Sector)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 3) +
  labs(title = "Trends in Self-Supplied Water Withdrawals by Sector",
       x = "Year",
       y = "Withdrawal (Mgal/d)",
       color = "Sector", 
       caption = "Figure 2: Self-supplied water withdrawals for each sector in Arizona. Note how Public and Irrigation vastly out weigh other sectors.") +
  theme_minimal() +
  scale_x_continuous(breaks = unique(long_data$Year)) +
  scale_y_continuous(breaks = seq(0, 5000, by= 1000)) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),  
    plot.subtitle = element_text(size = 14, hjust = 0.5, color = "gray40"),  
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    panel.grid.major = element_line(color = "gray85"),
    panel.grid.minor = element_blank(),  
    plot.caption = element_text(size = 10, hjust = 1, color = "gray50") 
  ) +
  scale_color_manual(values = c("Public.Supply" = "#f29f05",
                                "Domestic" = '#f25c05',
                                "Irrigation" = '#001eba',
                                "Thermoelectric" = '#d6568c',
                                "Industrial" = '#400d01',
                                "Mining" = '#a62f03',
                                "Livestock" = '#31029c',
                                "Aquaculture" = '#4d8584'
                                
  ))

withdrawals.by.sector
ggsave("withdrawal_trends_by_sector.tiff",
       plot = withdrawals.by.sector, dpi = 600,
       width = 10, height = 8, 
       units = "in", compression = "lzw")


############
# Withdrawals per sector minus Irrigation and Public Supply
############
long_data_filtered <- long_data %>%
  filter(!Sector %in% c("Public.Supply", "Irrigation"))

Withdrawls.by.sector.blow.up <- ggplot(long_data_filtered, aes(x = Year, y = Withdrawal, color = Sector, group = Sector)) +
  geom_line(linewidth = 1.5,) +
  geom_point(size = 3) +
  labs(
      x = NULL,
      y = NULL,
      color = "Sector") +
  theme_minimal() +
  scale_x_continuous(breaks = unique(long_data$Year)) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),  
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    panel.grid.major = element_line(color = "gray85"),
    panel.grid.minor = element_blank(),
    legend.position = "none", 
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  ) +
  scale_color_manual(values = c("Public.Supply" = "#f29f05",
                                "Domestic" = '#f25c05',
                                "Irrigation" = '#001eba',
                                "Thermoelectric" = '#d6568c',
                                "Industrial" = '#400d01',
                                "Mining" = '#a62f03',
                                "Livestock" = '#31029c',
                                "Aquaculture" = '#4d8584'
                                
  ))

Withdrawls.by.sector.blow.up

ggsave("withdrawal_trends_by_sector_blow_up.tiff",
       plot = Withdrawls.by.sector.blow.up, dpi = 600,
       width = 10, height = 6, 
       units = "in", compression = "lzw")

ggsave("withdrawal_trends_by_sector_blow_up.jpeg",
       plot = Withdrawls.by.sector.blow.up, dpi = 600,
       width = 10, height = 6)

############################################################################
