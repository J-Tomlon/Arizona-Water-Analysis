library(tidyverse)
library(dplyr)

setwd("C:/Users/delil/Desktop/Spring 2025/STA 486C/FinalProject/Arizona-Water-Analysis")


water <- read.csv("AMA Demand Supply from DW .csv")

water2 <- read.csv("raw-data/ID Summary Source Data.csv")


waterfiltered <- water2 %>% group_by(AMA) %>% 
  summarise(Avg_Lost = mean(Lost.and.Unaccounted),
            Avg_Delivered = mean(Delivered.Water.AF),
            Avg_Received = mean(Received.Water.AF), 
            Avg_Withdrawn = mean(Withdrawn.Water.AF), 
            Avg_Lost_Perc = mean(Lost.and.Unaccounted..))

# filtering by AMA and irrigation district to see the differences in water use 
waterfiltered <- water2 %>% 
  group_by(AMA) %>% 
  summarise(Avg_Lost = mean(Lost.and.Unaccounted, na.rm = TRUE),
            Avg_Delivered = mean(Delivered.Water.AF, na.rm = TRUE),
            Avg_Received = mean(Received.Water.AF, na.rm = TRUE),  # Fixed spelling
            Avg_Withdrawn = mean(Withdrawn.Water.AF, na.rm = TRUE), 
            Avg_Lost_Perc = mean(Lost.and.Unaccounted.., na.rm = TRUE))


# visual idea for this set- spatially plotting the water use/ amount of lost water by the sector 
waterfiltered2 <- water2 %>% 
  group_by(Irrigation.District) %>% 
  summarise(Avg_Lost = mean(Lost.and.Unaccounted, na.rm = TRUE),
            Avg_Delivered = mean(Delivered.Water.AF, na.rm = TRUE),
            Avg_Received = mean(Received.Water.AF, na.rm = TRUE),  # Fixed spelling
            Avg_Withdrawn = mean(Withdrawn.Water.AF, na.rm = TRUE), 
            Avg_Lost_Perc = mean(Lost.and.Unaccounted.., na.rm = TRUE))


# loading in the USGS data 
usgswater <- read.csv("USGS Water Use Data for All of Arizona.csv")

usgswaterfiltered <- usgswater %>% select(ends_with("water.withdrawals..fresh..in.Mgal.d"))

# code generated from claude for ideas on reshaping

# Load necessary libraries
library(tidyverse)
library(readr)

# Read the CSV file
water_data <- read_csv("USGS Water Use Data for All of Arizona.csv")
water_data <- read_csv("../raw-data/USGS Water Use Data for All of Arizona.csv")

# First, convert all columns (except the identifier columns) to character type
water_data_char <- water_data %>%
  mutate(across(-c(`State Code`, `State Name`, `County Code`, `County Name`, `Year`, 
                   `Total Population total population of area, in thousands`), as.character))

# Now pivot the data to long format
water_data_long <- water_data_char %>%
  pivot_longer(
    cols = -c(`State Code`, `State Name`, `County Code`, `County Name`, `Year`, 
              `Total Population total population of area, in thousands`),
    names_to = "metric",
    values_to = "value"
  )

# Extract category from metric names
water_data_long <- water_data_long %>%
  mutate(
    category = case_when(
      str_detect(metric, "^Public Supply") ~ "Public Supply",
      str_detect(metric, "^Domestic") ~ "Domestic",
      str_detect(metric, "^Industrial") ~ "Industrial",
      str_detect(metric, "^Total Thermoelectric") ~ "Thermoelectric",
      str_detect(metric, "^Mining") ~ "Mining",
      str_detect(metric, "^Irrigation") ~ "Irrigation",
      str_detect(metric, "^Livestock") ~ "Livestock",
      str_detect(metric, "^Aquaculture") ~ "Aquaculture",
      str_detect(metric, "^Hydroelectric") ~ "Hydroelectric",
      str_detect(metric, "^Wastewater") ~ "Wastewater",
      TRUE ~ "Other"
    ),
    # Extract measurement type (groundwater, surface water, etc.)
    measurement_type = case_when(
      str_detect(metric, "groundwater") ~ "groundwater",
      str_detect(metric, "surface") ~ "surface water",
      str_detect(metric, "population") ~ "population",
      str_detect(metric, "acres") ~ "area",
      str_detect(metric, "consumptive") ~ "consumption",
      TRUE ~ "other"
    )
  )

# Convert values to numeric (handling any non-numeric values)
water_data_long <- water_data_long %>%
  mutate(value = as.numeric(value))

# Create a summary dataset by category and year
water_summary <- water_data_long %>%
  filter(!is.na(value)) %>%
  group_by(`County Name`, `Year`, category, measurement_type) %>%
  summarize(
    avg_value = mean(value, na.rm = TRUE),
    sum_value = sum(value, na.rm = TRUE),
    count = n()
  ) %>%
  ungroup()

# Example: Extract just the main water withdrawal data
water_withdrawals <- water_data %>%
  select(
    `County Name`, `Year`, `Total Population total population of area, in thousands`,
    contains("total self-supplied withdrawals") & contains("total, in Mgal/d")
  )

# Create a time series analysis dataset for trend analysis
time_series_data <- water_data %>%
  group_by(`Year`) %>%
  summarize(
    total_population = sum(`Total Population total population of area, in thousands`, na.rm = TRUE),
    public_supply = sum(`Public Supply total self-supplied withdrawals, total, in Mgal/d`, na.rm = TRUE),
    irrigation = sum(`Irrigation, Total total self-supplied withdrawals, fresh, in Mgal/d`, na.rm = TRUE),
    industrial = sum(`Industrial total self-supplied withdrawals, in Mgal/d`, na.rm = TRUE),
    mining = sum(`Mining total self-supplied withdrawals, in Mgal/d`, na.rm = TRUE)
  )


######
water_use_by_sector <- water_data %>%
  select(
    `County Name`, `Year`, 
    `Total Population total population of area, in thousands`,
    # Public supply
    `Public Supply total self-supplied withdrawals, total, in Mgal/d`,
    # Irrigation (typically largest user in AZ)
    `Irrigation, Total total self-supplied withdrawals, fresh, in Mgal/d`, 
    # Mining (important in AZ)
    `Mining total self-supplied withdrawals, in Mgal/d`,
    # Industrial
    `Industrial total self-supplied withdrawals, in Mgal/d`,
    # Thermoelectric power
    `Total Thermoelectric Power total self-supplied withdrawals, total, in Mgal/d`,
    # Domestic
    `Domestic total self-supplied withdrawals plus deliveries, in Mgal/d`
  ) %>%
  mutate(across(-c(`County Name`, `Year`), ~as.numeric(.x)))


# Find a better set for reclaimed use? or look into possibly changing the units before downloading 


# Look at reclaimed water use opportunities
reclaimed_water <- water_data %>%
  select(
    `County Name`, `Year`,
    # Available reclaimed water
    `Wastewater Treatment reclaimed wastewater released by public wastewater facilities, in Mgal/d`,
    # Current reuse in different sectors
    contains("reclaimed wastewater") & !contains("released")
  ) %>%
  mutate(across(-c(`County Name`, `Year`), ~as.numeric(.x)))

# Calculate water use efficiency metrics
water_efficiency <- water_use_by_sector %>%
  mutate(
    # Calculate per capita metrics
    Population = as.numeric(`Total Population total population of area, in thousands`),
    Public_Supply_Per_Capita = as.numeric(`Public Supply total self-supplied withdrawals, total, in Mgal/d`) / Population,
    Domestic_Per_Capita = as.numeric(`Domestic total self-supplied withdrawals plus deliveries, in Mgal/d`) / Population,
    
    # Calculate relative sector usage (as percentage of total)
    Total_Water_Use = rowSums(across(c(
      `Public Supply total self-supplied withdrawals, total, in Mgal/d`,
      `Irrigation, Total total self-supplied withdrawals, fresh, in Mgal/d`,
      `Mining total self-supplied withdrawals, in Mgal/d`,
      `Industrial total self-supplied withdrawals, in Mgal/d`,
      `Total Thermoelectric Power total self-supplied withdrawals, total, in Mgal/d`,
      `Domestic total self-supplied withdrawals plus deliveries, in Mgal/d`
    )), na.rm = TRUE),
    
    Irrigation_Pct = as.numeric(`Irrigation, Total total self-supplied withdrawals, fresh, in Mgal/d`) / Total_Water_Use * 100,
    Public_Supply_Pct = as.numeric(`Public Supply total self-supplied withdrawals, total, in Mgal/d`) / Total_Water_Use * 100,
    Mining_Pct = as.numeric(`Mining total self-supplied withdrawals, in Mgal/d`) / Total_Water_Use * 100,
    Industrial_Pct = as.numeric(`Industrial total self-supplied withdrawals, in Mgal/d`) / Total_Water_Use * 100,
    Thermoelectric_Pct = as.numeric(`Total Thermoelectric Power total self-supplied withdrawals, total, in Mgal/d`) / Total_Water_Use * 100,
    Domestic_Pct = as.numeric(`Domestic total self-supplied withdrawals plus deliveries, in Mgal/d`) / Total_Water_Use * 100
  )

# Identify counties and years with highest water use 
high_water_use <- water_efficiency %>%
  group_by(`Year`) %>%
  arrange(desc(Total_Water_Use)) %>%
  slice_head(n = 5) %>%
  ungroup()

# Look at efficiency of irrigation (since it's typically the largest user in AZ)
irrigation_efficiency <- water_data %>%
  select(
    `County Name`, `Year`,
    `Irrigation, Total total self-supplied withdrawals, fresh, in Mgal/d`,
    `Irrigation, Total total irrigation, in thousand acres`,
    contains("microirrigation") & contains("thousand acres"),
    contains("sprinkler irrigation") & contains("thousand acres"),
    contains("surface irrigation") & contains("thousand acres")
  ) %>%
  mutate(across(-c(`County Name`, `Year`), ~as.numeric(.x))) %>%
  mutate(
    Water_Use_Per_Acre = `Irrigation, Total total self-supplied withdrawals, fresh, in Mgal/d` / 
      `Irrigation, Total total irrigation, in thousand acres`,
    Microirrigation_Pct = `Irrigation, Total microirrigation, in thousand acres` / 
      `Irrigation, Total total irrigation, in thousand acres` * 100,
    Sprinkler_Pct = `Irrigation, Total sprinkler irrigation, in thousand acres` / 
      `Irrigation, Total total irrigation, in thousand acres` * 100,
    Surface_Pct = `Irrigation, Total surface irrigation, in thousand acres` / 
      `Irrigation, Total total irrigation, in thousand acres` * 100
  )

# Look at groundwater vs surface water use (important for sustainability in AZ)
water_sources <- water_data %>%
  select(
    `County Name`, `Year`,
    # Public supply
    `Public Supply self-supplied groundwater withdrawals, fresh, in Mgal/d`,
    `Public Supply self-supplied surface-water withdrawals, fresh, in Mgal/d`,
    # Irrigation 
    `Irrigation, Total self-supplied groundwater withdrawals, fresh, in Mgal/d`,
    `Irrigation, Total self-supplied surface-water withdrawals, fresh, in Mgal/d`
  ) %>%
  mutate(across(-c(`County Name`, `Year`), ~as.numeric(.x))) %>%
  mutate(
    Public_Supply_GW = `Public Supply self-supplied groundwater withdrawals, fresh, in Mgal/d`,
    Public_Supply_SW = `Public Supply self-supplied surface-water withdrawals, fresh, in Mgal/d`,
    Irrigation_GW = `Irrigation, Total self-supplied groundwater withdrawals, fresh, in Mgal/d`,
    Irrigation_SW = `Irrigation, Total self-supplied surface-water withdrawals, fresh, in Mgal/d`,
    
    Public_Supply_GW_Pct = Public_Supply_GW / (Public_Supply_GW + Public_Supply_SW) * 100,
    Irrigation_GW_Pct = Irrigation_GW / (Irrigation_GW + Irrigation_SW) * 100,
    
    # Overall groundwater dependence
    Total_GW = Public_Supply_GW + Irrigation_GW,
    Total_SW = Public_Supply_SW + Irrigation_SW,
    GW_Dependence_Pct = Total_GW / (Total_GW + Total_SW) * 100
  )

# Connect current reclaimed water use to potential opportunities
reuse_potential <- water_data %>%
  select(
    `County Name`, `Year`,
    `Wastewater Treatment reclaimed wastewater released by public wastewater facilities, in Mgal/d`,
    `Public Supply total self-supplied withdrawals, total, in Mgal/d`,
    `Irrigation, Total total self-supplied withdrawals, fresh, in Mgal/d`
  ) %>%
  mutate(across(-c(`County Name`, `Year`), ~as.numeric(.x))) %>%
  mutate(
    Reclaimed_Available = `Wastewater Treatment reclaimed wastewater released by public wastewater facilities, in Mgal/d`,
    # Calculate potential reuse percentage (what % of water could be offset)
    Potential_Public_Supply_Offset_Pct = Reclaimed_Available / 
      `Public Supply total self-supplied withdrawals, total, in Mgal/d` * 100,
    Potential_Irrigation_Offset_Pct = Reclaimed_Available / 
      `Irrigation, Total total self-supplied withdrawals, fresh, in Mgal/d` * 100
  )

# Time series to analyze trends in water use
water_trends <- water_data %>%
  group_by(`Year`) %>%
  summarize(
    Total_Population = sum(as.numeric(`Total Population total population of area, in thousands`), na.rm = TRUE),
    Public_Supply = sum(as.numeric(`Public Supply total self-supplied withdrawals, total, in Mgal/d`), na.rm = TRUE),
    Irrigation = sum(as.numeric(`Irrigation, Total total self-supplied withdrawals, fresh, in Mgal/d`), na.rm = TRUE),
    Industrial = sum(as.numeric(`Industrial total self-supplied withdrawals, in Mgal/d`), na.rm = TRUE),
    Mining = sum(as.numeric(`Mining total self-supplied withdrawals, in Mgal/d`), na.rm = TRUE),
    Total_GW = sum(as.numeric(`Public Supply self-supplied groundwater withdrawals, fresh, in Mgal/d`), na.rm = TRUE) +
      sum(as.numeric(`Irrigation, Total self-supplied groundwater withdrawals, fresh, in Mgal/d`), na.rm = TRUE),
    Reclaimed_Available = sum(as.numeric(`Wastewater Treatment reclaimed wastewater released by public wastewater facilities, in Mgal/d`), na.rm = TRUE)
  ) %>%
  mutate(
    Public_Supply_Per_Capita = Public_Supply / Total_Population,
    Irrigation_Per_Capita = Irrigation / Total_Population,
    Total_Water_Use = Public_Supply + Irrigation + Industrial + Mining,
    Reclaimed_Potential_Pct = Reclaimed_Available / Total_Water_Use * 100
  )