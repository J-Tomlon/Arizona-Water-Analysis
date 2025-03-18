library(tidyverse)
library(dplyr)
library(viridis)
library(forcats)
library(readr)
library(sf)
library(tmap)
library(tigris)

###### lots of code generated from claude.ai

# loading in the USGS data 
water_data <- read.csv("USGS Water Use Data for All of Arizona.csv")

#################################################################
# initial attempt at reformatting from 300ish columns to a long format 
#################################################################

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


####################################
# focus on reclaimed water use
################################
reclaimed_water <- water_data %>% select(c("County Name", "Year", 
                                           "Mining reclaimed wastewater, in Mgal/d",                                                       
                                           "Wastewater Treatment reclaimed wastewater released by public wastewater facilities, in Mgal/d",
                                           "Irrigation, Total reclaimed wastewater, in Mgal/d",                                            
                                           "Irrigation, Crop reclaimed wastewater for crops, in Mgal/d",                                   
                                           "Irrigation, Golf Courses reclaimed wastewater for golf courses, in Mgal/d",                    
                                           "Wastewater Treatment reclaimed wastewater released by wastewater facilities, in Mgal/d"))

# Reshape the data for time series analysis
reclaimed_long <- reclaimed_water %>%
  # Get total reclaimed water usage across sectors
  mutate(
    Total_Reclaimed = rowSums(across(c(
      `Mining reclaimed wastewater, in Mgal/d`,
      `Irrigation, Total reclaimed wastewater, in Mgal/d`, 
      `Irrigation, Golf Courses reclaimed wastewater for golf courses, in Mgal/d`
    )), na.rm = TRUE)
  ) %>%
  select(`County Name`, Year, Total_Reclaimed) %>%
  # Filter out counties with zero usage throughout
  group_by(`County Name`) %>%
  filter(sum(Total_Reclaimed, na.rm = TRUE) > 0) %>%
  ungroup()

# Create line graph with county as color
ggplot(reclaimed_long, aes(x = Year, y = Total_Reclaimed, color = `County Name`, group = `County Name`)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_viridis_d() +
  theme_minimal() +
  labs(
    title = "Total Reclaimed Water Usage Over Time by County",
    subtitle = "Counties with non-zero reclaimed water usage",
    y = "Total Reclaimed Water (Mgal/d)",
    x = "Year"
  ) +
  theme(legend.position = "right")

# Create a dataset showing potential vs actual reuse
reclaimed_potential <- reclaimed_water %>%
  # Calculate total reclaimed water used vs available
  mutate(
    Reclaimed_Used = rowSums(across(c(
      `Mining reclaimed wastewater, in Mgal/d`,
      `Irrigation, Total reclaimed wastewater, in Mgal/d`, 
      `Irrigation, Golf Courses reclaimed wastewater for golf courses, in Mgal/d`
    )), na.rm = TRUE),
    Reclaimed_Available = `Wastewater Treatment reclaimed wastewater released by public wastewater facilities, in Mgal/d`,
    Utilization_Rate = ifelse(is.na(Reclaimed_Available) | Reclaimed_Available == 0, 
                              NA, 
                              Reclaimed_Used/Reclaimed_Available * 100)
  ) %>%
  filter(!is.na(Reclaimed_Available) & Reclaimed_Available > 0) %>%
  select(`County Name`, Year, Reclaimed_Used, Reclaimed_Available, Utilization_Rate)

# Create a stacked bar chart
reclaimed_potential_long <- reclaimed_potential %>%
  mutate(
    Unused_Potential = Reclaimed_Available - Reclaimed_Used
  ) %>%
  pivot_longer(
    cols = c(Reclaimed_Used, Unused_Potential),
    names_to = "Category",
    values_to = "Value"
  ) %>%
  filter(Year == max(Year))  # Most recent year

# Order counties by available reclaimed water
county_order <- reclaimed_potential %>%
  filter(Year == max(Year)) %>%
  arrange(desc(Reclaimed_Available)) %>%
  pull(`County Name`)

# code for plot: ReusePotentialPlot in the plots-in-progress folder
ggplot(reclaimed_potential_long, 
       aes(x = factor(`County Name`, levels = county_order), y = Value, fill = Category)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d(
    labels = c("Reclaimed Water Used", "Unused Potential"),
    option = "plasma"
  ) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Reclaimed Water: Actual Use vs Unused Potential",
    subtitle = paste("Data for", max(reclaimed_potential$Year)),
    y = "Reclaimed Water (Mgal/d)",
    x = "County",
    fill = ""
  )

# Join reclaimed water data with overall water use data
# Assuming you have the main water dataset loaded as water_data
water_combined <- water_data %>%
  select(
    `County Name`, Year,
    Total_Water_Use = `Public Supply total self-supplied withdrawals, total, in Mgal/d`,
    Population = `Total Population total population of area, in thousands`
  ) %>%
  left_join(
    reclaimed_water %>%
    select(
      `County Name`, Year,
      Reclaimed_Used = `Irrigation, Total reclaimed wastewater, in Mgal/d`,
      Reclaimed_Available = `Wastewater Treatment reclaimed wastewater released by public wastewater facilities, in Mgal/d`
    ),
    by = c("County Name", "Year")
  ) %>%
  mutate(
    Water_Per_Capita = Total_Water_Use / Population,
    Reclaimed_Pct = ifelse(is.na(Reclaimed_Used) | is.na(Total_Water_Use) | Total_Water_Use == 0,
                          NA,
                          (Reclaimed_Used / Total_Water_Use) * 100)
  ) %>%
  filter(!is.na(Water_Per_Capita) & !is.na(Reclaimed_Pct))

# Create a scatterplot to see correlation
ggplot(water_combined, aes(x = Water_Per_Capita, y = Reclaimed_Pct, color = `County Name`)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "grey50", linetype = "dashed") +
  scale_color_viridis_d() +
  theme_minimal() +
  labs(
    title = "Relationship Between Water Scarcity and Reclaimed Water Use",
    subtitle = "Do water-scarce counties use more reclaimed water?",
    x = "Water Use Per Capita (Mgal/d per 1000 people)",
    y = "Reclaimed Water as % of Total Water Use"
  )

#####################################
# focus on find improvements in efficiency by sector, 
## visualizing differences in irrigation methods
#########################################

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
      `Irrigation, Total total irrigation, in thousand acres` * 100, 
    Total = Microirrigation_Pct + Sprinkler_Pct + Surface_Pct
  )

# First, let's reshape the data to long format for easier plotting
irrigation_long <- irrigation_efficiency %>%
  select(`County Name`, Year, Microirrigation_Pct, Sprinkler_Pct, Surface_Pct) %>%
  pivot_longer(
    cols = c(Microirrigation_Pct, Sprinkler_Pct, Surface_Pct),
    names_to = "Irrigation_Type",
    values_to = "Percentage"
  ) %>%
  # Clean up the irrigation type names
  mutate(
    Irrigation_Type = case_when(
      Irrigation_Type == "Microirrigation_Pct" ~ "Microirrigation",
      Irrigation_Type == "Sprinkler_Pct" ~ "Sprinkler",
      Irrigation_Type == "Surface_Pct" ~ "Surface"
    )
  ) %>%
  # Remove rows with NA percentages
  filter(!is.na(Percentage))

# Aggregate data for statewide trends
statewide_trends <- irrigation_long %>%
  group_by(Year, Irrigation_Type) %>%
  summarize(
    Avg_Percentage = mean(Percentage, na.rm = TRUE),
    .groups = "drop"
  )

# Create the statewide trends visualization
ggplot(statewide_trends, aes(x = Year, y = Avg_Percentage, color = Irrigation_Type, group = Irrigation_Type)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_viridis_d(option = "plasma") +
  theme_minimal() +
  labs(
    title = "Changes in Irrigation Methods Over Time in Arizona",
    subtitle = "Average percentage of irrigation type across all counties",
    y = "Percentage of Total Irrigated Area (%)",
    x = "Year",
    color = "Irrigation Type"
  ) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )

# Create a county-by-county comparison using facets
# Filter to counties with sufficient data
counties_with_data <- irrigation_long %>%
  group_by(`County Name`) %>%
  summarize(data_points = n()) %>%
  filter(data_points >= 9) %>%  # At least 3 years of data (3 types × 3 years)
  pull(`County Name`)

# kind of an interesting looking plot ?
# Create faceted plot for selected counties
ggplot(irrigation_long %>% filter(`County Name` %in% counties_with_data), 
       aes(x = Year, y = Percentage, color = Irrigation_Type, group = Irrigation_Type)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_viridis_d(option = "plasma") +
  facet_wrap(~ `County Name`) +
  theme_minimal() +
  labs(
    title = "Changes in Irrigation Methods by County",
    y = "Percentage of Total Irrigated Area (%)",
    x = "Year",
    color = "Irrigation Type"
  ) +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "lightgray"),
    strip.text = element_text(face = "bold")
  )

# Create a stacked area chart to show composition changes over time
statewide_composition <- statewide_trends %>%
  pivot_wider(
    names_from = Irrigation_Type,
    values_from = Avg_Percentage
  ) %>%
  pivot_longer(
    cols = c(Microirrigation, Sprinkler, Surface),
    names_to = "Irrigation_Type",
    values_to = "Percentage"
  )

# Order the irrigation types for better visualization
statewide_composition$Irrigation_Type <- factor(
  statewide_composition$Irrigation_Type,
  levels = c("Surface", "Sprinkler", "Microirrigation")
)

# Create the stacked area chart
ggplot(statewide_composition, aes(x = Year, y = Percentage, fill = Irrigation_Type)) +
  geom_area(alpha = 0.8) +
  scale_fill_viridis_d(option = "plasma") +
  theme_minimal() +
  labs(
    title = "Composition of Irrigation Methods Over Time",
    subtitle = "Showing the relative proportion of each irrigation type",
    y = "Percentage of Total Irrigated Area (%)",
    x = "Year",
    fill = "Irrigation Type"
  ) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )

# Create a small multiples chart comparing irrigation efficiency with method
efficiency_by_method <- irrigation_efficiency %>%
  select(`County Name`, Year, Water_Use_Per_Acre, Microirrigation_Pct, Sprinkler_Pct, Surface_Pct) %>%
  pivot_longer(
    cols = c(Microirrigation_Pct, Sprinkler_Pct, Surface_Pct),
    names_to = "Irrigation_Type",
    values_to = "Percentage"
  ) %>%
  mutate(
    Irrigation_Type = case_when(
      Irrigation_Type == "Microirrigation_Pct" ~ "Microirrigation",
      Irrigation_Type == "Sprinkler_Pct" ~ "Sprinkler",
      Irrigation_Type == "Surface_Pct" ~ "Surface"
    )
  ) %>%
  filter(!is.na(Percentage) & !is.na(Water_Use_Per_Acre) & Percentage > 0)

# Create a scatter plot showing the relationship between irrigation method and water efficiency
ggplot(efficiency_by_method, 
       aes(x = Percentage, y = Water_Use_Per_Acre, color = Irrigation_Type)) +
  geom_point(alpha = 0.7, size = 3) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1, alpha = 0.7) +
  scale_color_viridis_d(option = "plasma") +
  facet_wrap(~ Irrigation_Type, scales = "free_x") +
  theme_minimal() +
  labs(
    title = "Relationship Between Irrigation Method and Water Efficiency",
    subtitle = "Higher water use per acre indicates lower efficiency",
    y = "Water Use Per Acre (Mgal/d per thousand acres)",
    x = "Percentage of Irrigation Type (%)",
    color = "Irrigation Type"
  ) +
  theme(legend.position = "none")



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

####################################
# Focus on spatial visualization
################################

# Get Arizona county shapefile
az_counties <- counties(state = "AZ", cb = TRUE)

# Prepare most recent water data with key metrics
latest_year <- max(water_data$Year, na.rm = TRUE)
water_metrics <- water_data %>%
  filter(Year == latest_year) %>%
  mutate(
    # Total water use
    Total_Water_Use = rowSums(across(c(
      `Public Supply total self-supplied withdrawals, total, in Mgal/d`,
      `Irrigation, Total total self-supplied withdrawals, fresh, in Mgal/d`,
      `Mining total self-supplied withdrawals, in Mgal/d`,
      `Industrial total self-supplied withdrawals, in Mgal/d`
    )), na.rm = TRUE),
    
    # Key sectors
    Irrigation = `Irrigation, Total total self-supplied withdrawals, fresh, in Mgal/d`,
    Public_Supply = `Public Supply total self-supplied withdrawals, total, in Mgal/d`,
    Mining = `Mining total self-supplied withdrawals, in Mgal/d`,
    
    # Per capita and water source metrics
    Population = `Total Population total population of area, in thousands`,
    Total_Per_Capita = Total_Water_Use / Population,
    Groundwater_Pct = `Public Supply self-supplied groundwater withdrawals, fresh, in Mgal/d` / 
      `Public Supply total self-supplied withdrawals, total, in Mgal/d` * 100
  ) %>%
  select(`County Name`, Population, Total_Water_Use, Total_Per_Capita, 
         Irrigation, Public_Supply, Mining, Groundwater_Pct) %>%
  mutate(`County Name` = str_replace(`County Name`, " COUNTY", ""))

# Join with shapefile (ensure county names match)
az_counties$NAME <- toupper(az_counties$NAME)
az_water_map <- az_counties %>%
  left_join(water_metrics, by = c("NAMELSAD" = "County Name"))

# Create a series of maps with viridis color scales
tmap_mode("plot")

# Load required libraries
library(tidyverse)
library(sf)
library(viridis)
library(tigris)

# Read water data and convert columns
water_data <- read_csv("USGS Water Use Data for All of Arizona.csv") %>%
  mutate(across(-c(`State Code`, `County Code`, `Year`), as.character),
         across(-c(`State Name`, `County Name`), as.numeric))

# Get Arizona county shapefile
az_counties <- counties(state = "AZ", cb = TRUE)

# Prepare most recent water data with key metrics
latest_year <- max(water_data$Year, na.rm = TRUE)
water_metrics <- water_data %>%
  filter(Year == latest_year) %>%
  mutate(
    # Total water use
    Total_Water_Use = rowSums(across(c(
      `Public Supply total self-supplied withdrawals, total, in Mgal/d`,
      `Irrigation, Total total self-supplied withdrawals, fresh, in Mgal/d`,
      `Mining total self-supplied withdrawals, in Mgal/d`,
      `Industrial total self-supplied withdrawals, in Mgal/d`
    )), na.rm = TRUE),
    
    # Key sectors
    Irrigation = `Irrigation, Total total self-supplied withdrawals, fresh, in Mgal/d`,
    Public_Supply = `Public Supply total self-supplied withdrawals, total, in Mgal/d`,
    Mining = `Mining total self-supplied withdrawals, in Mgal/d`,
    
    # Per capita and water source metrics
    Population = `Total Population total population of area, in thousands`,
    Total_Per_Capita = Total_Water_Use / Population,
    Groundwater_Pct = `Public Supply self-supplied groundwater withdrawals, fresh, in Mgal/d` / 
      `Public Supply total self-supplied withdrawals, total, in Mgal/d` * 100
  ) %>%
  select(`County Name`, Population, Total_Water_Use, Total_Per_Capita, 
         Irrigation, Public_Supply, Mining, Groundwater_Pct) %>%
  mutate(`County Name` = str_replace(`County Name`, " COUNTY", ""))

# Join with shapefile (ensure county names match)
az_counties$NAME <- toupper(az_counties$NAME)
az_water_map <- az_counties %>%
  left_join(water_metrics, by = c("NAME" = "County Name"))

# Example 1: Total Water Use Map
ggplot(data = az_water_map) +
  geom_sf(aes(fill = Total_Water_Use)) +
  scale_fill_viridis_c(option = "viridis", name = "Total Water Use\n(Mgal/d)") +
  theme_minimal() +
  labs(title = "Total Water Use by Arizona County",
       subtitle = paste("Data from USGS,", latest_year)) +
  theme(legend.position = "right")

# Example 2: Irrigation Water Use Map
ggplot(data = az_water_map) +
  geom_sf(aes(fill = Irrigation)) +
  scale_fill_viridis_c(option = "inferno", name = "Irrigation\n(Mgal/d)") +
  theme_minimal() +
  labs(title = "Irrigation Water Use by Arizona County",
       subtitle = paste("Data from USGS,", latest_year)) +
  theme(legend.position = "right")

# Example 3: Water Use Per Capita
ggplot(data = az_water_map) +
  geom_sf(aes(fill = Total_Per_Capita)) +
  scale_fill_viridis_c(option = "plasma", name = "Water Use Per Capita\n(Mgal/d per 1000 people)") +
  theme_minimal() +
  labs(title = "Water Use Per Capita by Arizona County",
       subtitle = paste("Data from USGS,", latest_year)) +
  theme(legend.position = "right")

# Example 4: Groundwater Percentage
ggplot(data = az_water_map) +
  geom_sf(aes(fill = Groundwater_Pct)) +
  scale_fill_viridis_c(option = "cividis", name = "Groundwater\nPercentage (%)") +
  theme_minimal() +
  labs(title = "Groundwater Dependence by Arizona County",
       subtitle = paste("Data from USGS,", latest_year)) +
  theme(legend.position = "right")

# Create a multi-panel comparison of water use by sector
# Gather the data for faceting
az_water_sectors <- az_water_map %>%
  select(NAME, geometry, Public_Supply, Irrigation, Mining) %>%
  pivot_longer(cols = c(Public_Supply, Irrigation, Mining),
               names_to = "Sector",
               values_to = "Water_Use")

# Create faceted map
ggplot(data = az_water_sectors) +
  geom_sf(aes(fill = Water_Use)) +
  scale_fill_viridis_c(option = "viridis", name = "Water Use\n(Mgal/d)") +
  facet_wrap(~Sector, ncol = 3) +
  theme_minimal() +
  labs(title = "Water Use by Sector across Arizona Counties",
       subtitle = paste("Data from USGS,", latest_year)) +
  theme(legend.position = "right",
        axis.text.x = element_blank(),
        axis.text.y = element_blank())

# For time series analysis showing changes over time
water_time <- water_data %>%
  group_by(`County Name`, Year) %>%
  summarize(
    Total_Water_Use = sum(`Public Supply total self-supplied withdrawals, total, in Mgal/d`, na.rm = TRUE) +
      sum(`Irrigation, Total total self-supplied withdrawals, fresh, in Mgal/d`, na.rm = TRUE) +
      sum(`Mining total self-supplied withdrawals, in Mgal/d`, na.rm = TRUE) +
      sum(`Industrial total self-supplied withdrawals, in Mgal/d`, na.rm = TRUE),
    Population = sum(`Total Population total population of area, in thousands`, na.rm = TRUE),
    .groups = "drop"
  )

# County-level water use trends over time
ggplot(water_time, aes(x = Year, y = Total_Water_Use, color = `County Name`)) +
  geom_line(linewidth = 1) +
  scale_color_viridis_d() +
  theme_minimal() +
  labs(title = "Water Use Trends by County",
       y = "Total Water Use (Mgal/d)",
       x = "Year") +
  theme(legend.position = "bottom")
