library(readxl)
library(stringr)
library(dplyr)
library(tidyverse)
library(tidyr)
library(gganimate)
library(leaflet)
library(htmlwidgets)
library(viridis)
library(webshot2)
library(magick)
library(mgcv)
library(tidygeocoder)
library(lubridate)
library(zoo)
library(geosphere)
library(glmnet)
library(treemapify)
library(ggrepel)

# INLA not available in CRAN
# see https://www.r-inla.org/download-install 
if (!requireNamespace("INLA", quietly = TRUE)) {
  stop("INLA package not installed. See https://www.r-inla.org/download-install to install the package and run the script.")
} else {
  library(INLA)
}
library(ggplot2)
library(car)
library(sf)   
library(ggmap)  
library(tmaptools)  
library(gridExtra)
library(lattice)

# options(tidygeocoder.google_api_key = "AIzaSyB13Mx7EXxARNCoYEH2ICJ40WdQbfcl7QI")
# Sys.setenv(GOOGLEGEOCODE_API_KEY = "AIzaSyB13Mx7EXxARNCoYEH2ICJ40WdQbfcl7QI")

###### EXPLORING DATA ##################################################

## Visualizations based on deal specific data ##

df <- readRDS("Data.rds")

## Overall Deal Volume ##

deal_volume_over_year <- df %>%
  group_by(Year) %>%
  summarize(Deal_Volume = n(), .groups = "drop")

# Plot Histogram of Overall Deal Volume Over Time (Figure 29)
fig29 <- ggplot(deal_volume_over_year, aes(x = factor(Year), y = Deal_Volume)) +
  geom_col(fill = "#2C7FB8") +
  labs(
    title = "Overall Deal Volume Over Time",
    x = "Year",
    y = "Number of Deals"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

# Save Figure 29
ggsave(filename = "figures/fig37.png", plot = fig29, width = 12, height = 6, dpi = 300)

# Identify Top 10 Cities by Total Deal Volume
top_10_cities <- df %>%
  filter(!is.na(Town)) %>%
  group_by(Town) %>%
  summarize(Total_Deal_Volume = n(), .groups = "drop") %>%
  arrange(desc(Total_Deal_Volume)) %>%
  slice_head(n = 10) %>%
  pull(Town)

# Filter data for Top 10 Cities and Aggregate Deal Volume Over Year
deal_volume_top_cities <- df %>%
  filter(Town %in% top_10_cities) %>%
  group_by(Year, Town) %>%
  summarize(Deal_Volume = n(), .groups = "drop")

# Plot Faceted Histogram of Deal Volume Over Time for Top 10 Cities (Figure 30)
fig30 <- ggplot(deal_volume_top_cities, aes(x = factor(Year), y = Deal_Volume)) +
  geom_col(fill = "#2C7FB8") +
  labs(
    title = "Deal Volume Over Time Across Top 10 Cities",
    x = "Year",
    y = "Number of Deals"
  ) +
  facet_wrap(~ Town, ncol = 2) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    strip.text = element_text(face = "bold")
  )

# Save Figure 30
ggsave(filename = "figures/fig38.png", plot = fig30, width = 14, height = 10, dpi = 300)


## Treemap Plot of Asset Class ###


df_treemap <- df %>%
  filter(!is.na(Primary.Asset.Type)) %>%
  group_by(Primary.Asset.Type) %>%
  summarize(Deal_Volume = n(), .groups = "drop")

ggplot(df_treemap, aes(area = Deal_Volume, fill = Primary.Asset.Type,
                       label = paste(Primary.Asset.Type))) +
  geom_treemap() +
  geom_treemap_text(colour = "white", place = "centre", size = 8) +
  scale_fill_viridis_d() +
  labs(title = "Deal Volume by Town and Asset Class") +
  theme_minimal()
ggsave(filename = "figures/fig1.png", plot = last_plot(), width = 10, height = 6, dpi = 300)


## Los Angeles Deal Gifs ##
## Syntax aided by Chat-GPT ##

# Filter data for Los Angeles and arrange by time
la_data <- df %>%
  filter(Town == "Los Angeles") %>%
  arrange(Year_Month) %>%
  mutate(Year = as.numeric(substr(Year_Month, 1, 4)))

# color palette based on years
year_colors <- colorFactor(palette = "viridis", domain = la_data$Year)

# create directory to save Leaflet map snapshots
output_dir <- "leaflet_frames"
dir.create(output_dir, showWarnings = FALSE)

unique_dates <- unique(la_data$Year_Month)
for (i in seq_along(unique_dates)) {
  current_date <- unique_dates[i]
  current_data <- la_data %>% filter(Year_Month <= current_date)
  
  m <- leaflet(current_data) %>%
    addTiles() %>%
    addCircleMarkers(
      ~longitude,
      ~latitude,
      color = ~year_colors(Year),
      radius = 5,
      fillOpacity = 0.8,
      popup = ~paste0(
        "<b>Deal Type:</b> ", Deal.Type, "<br>",
        "<b>Deal Size (USD Mn):</b> ", Deal.Size.Usd.Mn, "<br>",
        "<b>Date:</b> ", Year_Month
      )
    ) %>%
    addLegend(
      position = "bottomright",
      pal = year_colors,
      values = ~Year,
      title = "Year"
    )
  
  # Save the map as an HTML widget and capture a PNG
  map_file <- file.path(output_dir, paste0("frame_", sprintf("%03d", i), ".html"))
  png_file <- file.path(output_dir, paste0("frame_", sprintf("%03d", i), ".png"))
  
  saveWidget(m, file = map_file, selfcontained = TRUE)
  webshot(map_file, file = png_file, cliprect = "viewport")
}

# combine PNGs into a GIF
png_files <- list.files(output_dir, pattern = "*.png", full.names = TRUE)
gif <- image_read(png_files)
gif <- image_animate(gif, fps = 5)
image_write(gif, "los_angeles_deals.gif")

##### Modeling ######

## first, we have to do some data processing to get it into the form
## that INLA can work with

d <- readRDS("Data.rds")
d$Unemployment_Rate <- as.numeric(d$Unemployment_Rate)

unique_towns <- d %>%
  distinct(Town) %>%
  filter(!is.na(Town)) %>%
  mutate(full_address = paste0(Town, ", California"))

unique_towns$full_address <- as.character(unique_towns$full_address)

# Geocode towns to get latitude and longitude
town_coords <- unique_towns %>%
  tidygeocoder::geocode(address = "full_address", method = "osm") %>%
  rename(lat.census.town = lat, lon.census.town = long)

d <- d %>%
  left_join(town_coords, by = "Town")
colnames(d)


d_new <- d

predictor_data <- readRDS("Data_Predictor.rds")
predictor_data$Unemployment_Rate <- as.numeric(predictor_data$Unemployment_Rate)

d_new <- d_new %>%
  mutate(across(starts_with("Estimate "), ~ as.numeric(.), .names = "{.col}"))  # Convert non-numeric to NA

# create 6 month periods
# anything less is too sparse data
d_new <- d_new %>%
  mutate(
    Year_Month = as.Date(paste0(Year_Month, "-01")),
    Period = floor_date(Year_Month, unit = "6 months")
  )

predictor_data <- predictor_data %>%
  mutate(
    Year_Month = as.Date(paste0(Year_Month, "-01")),
    Period = floor_date(Year_Month, unit = "6 months")
  ) %>%
  mutate(across(
    starts_with("Estimate ") | matches("Opportunity_Zone"),
    ~ as.numeric(.),
    .names = "{.col}"
  ))  # Convert non-numeric to NA

d_aggregated <- d_new %>%
  mutate(
    Asset_Class = case_when(
      Primary.Asset.Type %in% c("Residential", "Industrial", "Office", "Retail") ~ Primary.Asset.Type,
      Primary.Asset.Type %in% c("Niche", "Mixed Use", "Land", "Hotel") ~ "Other",
      TRUE ~ "Unknown"
    )
  ) %>%
  group_by(Town, Period) %>%
  summarise(
    Deal_Volume = n(),
    Avg_Deal_Size = ifelse(Deal_Volume > 0, mean(Deal.Size.Usd.Mn, na.rm = TRUE), NA),
    Total_Size_Sq_Ft = ifelse(Deal_Volume > 0, mean(Total.Size.Sq.Ft, na.rm = TRUE), NA),
    Fed_Rate = mean(Fed_Rate, na.rm = TRUE),
    Unemployment_Rate = mean(Unemployment_Rate, na.rm = TRUE),
    HomeValue = mean(HomeValue, na.rm = TRUE),
    across(starts_with("Estimate "), ~ mean(., na.rm = TRUE), .names = "{.col}"),
    Opportunity_Zone = first(Opportunity_Zone),
    lat.census.town = first(lat.census.town),
    lon.census.town = first(lon.census.town),
    Residential_Percentage = sum(Asset_Class == "Residential") / Deal_Volume * 100,
    Industrial_Percentage = sum(Asset_Class == "Industrial") / Deal_Volume * 100,
    Office_Percentage = sum(Asset_Class == "Office") / Deal_Volume * 100,
    Retail_Percentage = sum(Asset_Class == "Retail") / Deal_Volume * 100,
    Other_Percentage = sum(Asset_Class == "Other") / Deal_Volume * 100,
    .groups = "drop"
  )

# fill in missing periods for each town
full_grid <- expand.grid(
  Town = unique(d_new$Town),
  Period = seq(min(d_new$Period), max(d_new$Period), by = "6 months")
)

d_aggregated <- full_grid %>%
  left_join(d_aggregated, by = c("Town", "Period"))



d_aggregated <- d_aggregated %>%
  left_join(select(town_coords, Town, lat.census.town, lon.census.town), by = "Town") %>%
  select(-lat.census.town.x, -lon.census.town.x) %>%
  rename(lat.census.town = lat.census.town.y, lon.census.town = lon.census.town.y)


# percentages and metrics to 0 for periods with no deals
d_aggregated <- d_aggregated %>%
  mutate(
    Deal_Volume = ifelse(is.na(Deal_Volume), 0, Deal_Volume),
    Avg_Deal_Size = ifelse(Deal_Volume == 0, 0, Avg_Deal_Size),
    Total_Size_Sq_Ft = ifelse(Deal_Volume == 0, 0, Total_Size_Sq_Ft),
    Residential_Percentage = ifelse(Deal_Volume == 0, 0, Residential_Percentage),
    Industrial_Percentage = ifelse(Deal_Volume == 0, 0, Industrial_Percentage),
    Office_Percentage = ifelse(Deal_Volume == 0, 0, Office_Percentage),
    Retail_Percentage = ifelse(Deal_Volume == 0, 0, Retail_Percentage),
    Other_Percentage = ifelse(Deal_Volume == 0, 0, Other_Percentage)
  )

# perform row-wise updates
d_aggregated <- d_aggregated %>%
  rowwise() %>%
  mutate(
    Fed_Rate = ifelse(
      Deal_Volume == 0,
      mean(predictor_data$Fed_Rate[
        predictor_data$Period == Period
      ], na.rm = TRUE),
      Fed_Rate
    ),
    Unemployment_Rate = ifelse(
      Deal_Volume == 0,
      mean(predictor_data$Unemployment_Rate[
        predictor_data$Period == Period
      ], na.rm = TRUE),
      Unemployment_Rate
    ),
    HomeValue = ifelse(
      Deal_Volume == 0,
      mean(predictor_data$HomeValue[
        predictor_data$Period == Period
      ], na.rm = TRUE),
      HomeValue
    ),
    Opportunity_Zone = ifelse(
      Deal_Volume == 0,
      mean(predictor_data$Opportunity_Zone[
        predictor_data$Town == Town & predictor_data$Period == Period
      ], na.rm = TRUE),
      Opportunity_Zone
    ),
    across(
      starts_with("Estimate "),
      ~ ifelse(
        Deal_Volume == 0,
        mean(predictor_data[[cur_column()]][
          predictor_data$Town == Town & predictor_data$Period == Period
        ], na.rm = TRUE),
        .
      )
    )
  ) %>%
  ungroup()

# forward-backward fill for HomeValue and Estimate.* columns
d_aggregated <- d_aggregated %>%
  group_by(Town) %>%
  mutate(
    HomeValue = ifelse(
      is.na(HomeValue),
      zoo::na.locf(HomeValue, na.rm = FALSE, fromLast = FALSE),
      HomeValue
    ),
    HomeValue = ifelse(
      is.na(HomeValue),
      zoo::na.locf(HomeValue, na.rm = FALSE, fromLast = TRUE),
      HomeValue
    ),
    across(
      starts_with("Estimate "),
      ~ ifelse(
        is.na(.),
        zoo::na.locf(.x, na.rm = FALSE, fromLast = FALSE),
        .
      )
    ),
    across(
      starts_with("Estimate "),
      ~ ifelse(
        is.na(.),
        zoo::na.locf(.x, na.rm = FALSE, fromLast = TRUE),
        .
      )
    )
  ) %>%
  ungroup()

# fill missing values from nearest town if a town has no data for HomeValue or Estimate.*
fill_from_nearest <- function(row, target_column) {
  if (!is.na(row[[target_column]])) {
    return(row[[target_column]])
  }
  
  current_town <- row$Town
  current_period <- row$Period
  current_lat <- row$lat.census.town
  current_lon <- row$lon.census.town
  
  valid_towns <- d_aggregated %>%
    filter(!is.na(!!sym(target_column)) & Period == current_period) %>%
    mutate(distance = distHaversine(cbind(lon.census.town, lat.census.town), 
                                    c(current_lon, current_lat))) %>%
    arrange(distance)
  
  if (nrow(valid_towns) > 0) {
    return(valid_towns[[target_column]][1])
  }
  
  return(NA)
}

# apply nearest town logic
d_aggregated <- d_aggregated %>%
  rowwise() %>%
  mutate(
    HomeValue = ifelse(is.na(HomeValue), fill_from_nearest(cur_data(), "HomeValue"), HomeValue),
    across(
      starts_with("Estimate "),
      ~ ifelse(is.na(.), fill_from_nearest(cur_data(), cur_column()), .)
    )
  ) %>%
  ungroup()

# Forward-backward search for Avg_Deal_Size and Total_Size_Sq_Ft within each town
d_aggregated <- d_aggregated %>%
  group_by(Town) %>%
  mutate(
    Avg_Deal_Size = ifelse(
      is.na(Avg_Deal_Size),
      zoo::na.locf(Avg_Deal_Size, na.rm = FALSE, fromLast = FALSE),
      Avg_Deal_Size
    ),
    Avg_Deal_Size = ifelse(
      is.na(Avg_Deal_Size),
      zoo::na.locf(Avg_Deal_Size, na.rm = FALSE, fromLast = TRUE),
      Avg_Deal_Size
    ),
    Total_Size_Sq_Ft = ifelse(
      is.na(Total_Size_Sq_Ft),
      zoo::na.locf(Total_Size_Sq_Ft, na.rm = FALSE, fromLast = FALSE),
      Total_Size_Sq_Ft
    ),
    Total_Size_Sq_Ft = ifelse(
      is.na(Total_Size_Sq_Ft),
      zoo::na.locf(Total_Size_Sq_Ft, na.rm = FALSE, fromLast = TRUE),
      Total_Size_Sq_Ft
    )
  ) %>%
  ungroup()

# Define the function for nearest-town analysis
fill_from_nearest <- function(row, target_column) {
  if (!is.na(row[[target_column]])) {
    return(row[[target_column]])  # Return existing value if it's not NA
  }
  
  # Extract relevant information for the current row
  current_town <- row$Town
  current_period <- row$Period
  current_lat <- row$lat.census.town
  current_lon <- row$lon.census.town
  
  # Find the nearest town with valid data for the target column
  valid_towns <- d_aggregated %>%
    filter(!is.na(!!sym(target_column)) & Deal_Volume > 1 & Period == current_period) %>%
    mutate(distance = distHaversine(cbind(lon.census.town, lat.census.town), 
                                    c(current_lon, current_lat))) %>%
    arrange(distance)  # Sort by distance
  
  if (nrow(valid_towns) > 0) {
    return(valid_towns[[target_column]][1])  # Return the value from the nearest valid town
  }
  
  return(NA)  # Return NA if no valid data is found
}

# Apply nearest-town logic to Avg_Deal_Size and Total_Size_Sq_Ft
d_aggregated <- d_aggregated %>%
  rowwise() %>%
  mutate(
    Avg_Deal_Size = ifelse(is.na(Avg_Deal_Size), fill_from_nearest(cur_data(), "Avg_Deal_Size"), Avg_Deal_Size),
    Total_Size_Sq_Ft = ifelse(is.na(Total_Size_Sq_Ft), fill_from_nearest(cur_data(), "Total_Size_Sq_Ft"), Total_Size_Sq_Ft)
  ) %>%
  ungroup()

print("Data processing complete - starting variable selection and INLA model")
sum(is.na(d_aggregated$lat.census.town))

### SOME VISUALIZATIONS ###

## visualization of deals across towns ## 
d_aggregated$Period
df_yearly <- d_aggregated %>%
  mutate(Year = as.numeric(substr(Period, 1, 4))) %>%
  group_by(Town, Year) %>%
  summarize(Deal_Volume = sum(Deal_Volume), .groups = "drop") %>%
  mutate(log_Deal_Volume = log(Deal_Volume + 1)) %>%
  filter(Town != "NA")

top_towns <- df_yearly %>%
  group_by(Town) %>%
  summarize(Total_Deal_Volume = sum(Deal_Volume)) %>%
  top_n(25, Total_Deal_Volume) %>%
  pull(Town)

df_yearly <- df_yearly %>%
  filter(Town %in% top_towns)

# now plot a heatmap
ggplot(df_yearly, aes(x = Year, y = reorder(Town, log_Deal_Volume), fill = log_Deal_Volume)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c() +
  labs(title = "Deal Volume by Town and Year",
       x = "Year", y = "Town") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 7, angle = 30),  # Adjust size and angle as needed
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
ggsave(filename = "figures/fig2.png", plot = last_plot(), width = 10, height = 6, dpi = 300)


## yearly deal volume by town ## 
ggplot(df_yearly, aes(x = Year, y = Deal_Volume, group = Town)) +
  geom_line() +
  facet_wrap(~ Town, scales = "free_y") +
  labs(title = "Yearly Deal Volume Trends by Town",
       x = "Year", y = "Deal Volume") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6)
  )
ggsave(filename = "figures/fig3.png", plot = last_plot(), width = 10, height = 6, dpi = 300)

## bubble map ##

town_volume <- d_aggregated %>%
  group_by(Town, lat.census.town, lon.census.town) %>%
  summarize(Deal_Volume = sum(Deal_Volume), .groups = "drop")

bbox <- make_bbox(lon = town_volume$lon.census.town, 
                  lat = town_volume$lat.census.town, 
                  f = 0.1)  # 'f' is the zoom factor (10% padding)
california_map <- map_data("state", region = "california")
ggplot() +
  # Plot the base map
  geom_polygon(data = california_map, 
               aes(x = long, y = lat, group = group),
               fill = "lightgray", color = "white") +
  # Overlay the bubble points
  geom_point(data = town_volume, 
             aes(x = lon.census.town, y = lat.census.town, 
                 size = Deal_Volume, color = Deal_Volume), 
             alpha = 0.7) +
  # Use a viridis color scale
  scale_color_viridis_c(option = "C") +
  # Adjust the size scale if needed
  scale_size_continuous(range = c(2, 10)) +
  # Labels and title
  labs(title = "Deal Volume by Town (Spatial Distribution)",
       x = "Longitude", y = "Latitude", 
       size = "Deal Volume", color = "Deal Volume") +
  # Minimal theme
  theme_minimal() +
  # Coordinate system
  coord_fixed(1.3)
ggsave(filename = "figures/fig4.png", plot = last_plot(), width = 10, height = 6, dpi = 300)

#### RUNNING THE INLA MODEL ####

d_modeling <- d_aggregated


d_modeling <- d_modeling %>%
  select(-`Estimate With earnings`, `Estimate With health insurance coverage`)

all_numeric_columns <- names(d_modeling)[4:52]

# don't want to normalize coordinates, lets leave them out
## additionally, want to leave out Percentage, Total Deal Size, Sq. Ft, etc.
## they're proxies for deal volume

numeric_columns <- setdiff(all_numeric_columns, c("lat.census.town", "lon.census.town", "Residential_Percentage", "Industrial_Percentage", "Office_Percentage", "Retail_Percentage", "Other_Percentage", "Avg_Deal_Size", "Total_Size_Sq_Ft"))


d_modeling <- d_modeling %>%
  mutate(
    Town = as.factor(Town),
    Period = as.factor(Period)
  )

# create numerical tile variable - probably not needed
d_modeling <- d_modeling %>%
  arrange(Period) %>%
  mutate(Period_numeric = as.numeric(as.factor(Period)))

# scale numeric predictors for LASSO, INLA
d_modeling <- d_modeling %>%
  mutate(across(all_of(numeric_columns), scale, .names = "{.col}_scaled"))

### For simplicity, lets consolidate predictors

d_modeling <- d_modeling %>%
  mutate(
    Car_scaled = `Estimate Car, truck, or van -- carpooled_scaled`,
    Other_Transportation_scaled = `Estimate Public transportation (excluding taxicab)_scaled` + `Estimate Walked_scaled`,
    Children_scaled = `Estimate Own children under 6 years_scaled` + `Estimate Own children 6 to 17 years_scaled`,
    Corporate_scaled = `Estimate Sales and office occupations_scaled` +
      `Estimate Information_scaled` +
      `Estimate Finance and insurance, and real estate and rental and leasing_scaled` +
      `Estimate Public administration_scaled`,
    Blue_Collar_scaled = `Estimate Natural resources, construction, and maintenance occupations_scaled` +
      `Estimate Production, transportation, and material moving occupations_scaled` +
      `Estimate Agriculture, forestry, fishing and hunting, and mining_scaled` +
      `Estimate Manufacturing_scaled` +
      `Estimate Retail trade_scaled` +
      `Estimate Transportation and warehousing, and utilities_scaled`,
    Other_Services_scaled = `Estimate Educational services, and health care and social assistance_scaled` +
      `Estimate Arts, entertainment, and recreation, and accommodation and food services_scaled` +
      `Estimate Other services, except public administration_scaled` +
      `Estimate Self-employed in own not incorporated business workers_scaled`,
    Govt_Benefits_scaled = `Estimate With Social Security_scaled` +
      `Estimate With retirement income_scaled` +
      `Estimate With Supplemental Security Income_scaled`+ `Estimate With health insurance coverage!!With public coverage_scaled`,
    Welfare_Benefits_scaled = `Estimate With cash public assistance income_scaled` +
      `Estimate With Food Stamp/SNAP benefits in the past 12 months_scaled`
  )
d_modeling$es
# Derived predictors on the raw scale too - for prediction stuff later
d_modeling <- d_modeling %>%
  mutate(
    Car_raw = `Estimate Car, truck, or van -- carpooled_scaled` * 
      sd(`Estimate Car, truck, or van -- carpooled`) + 
      mean(`Estimate Car, truck, or van -- carpooled`),
    Other_Transportation_raw = 
      (`Estimate Public transportation (excluding taxicab)_scaled` * 
         sd(`Estimate Public transportation (excluding taxicab)`) + 
         mean(`Estimate Public transportation (excluding taxicab)`)) +
      (`Estimate Walked_scaled` * sd(`Estimate Walked`) + mean(`Estimate Walked`)),
    Children_raw = 
      (`Estimate Own children under 6 years_scaled` * sd(`Estimate Own children under 6 years`) + 
         mean(`Estimate Own children under 6 years`)) +
      (`Estimate Own children 6 to 17 years_scaled` * sd(`Estimate Own children 6 to 17 years`) + 
         mean(`Estimate Own children 6 to 17 years`)),
    Corporate_raw = 
      (`Estimate Sales and office occupations_scaled` * sd(`Estimate Sales and office occupations`) + 
         mean(`Estimate Sales and office occupations`)) +
      (`Estimate Information_scaled` * sd(`Estimate Information`) + mean(`Estimate Information`)) +
      (`Estimate Finance and insurance, and real estate and rental and leasing_scaled` * 
         sd(`Estimate Finance and insurance, and real estate and rental and leasing`) +
         mean(`Estimate Finance and insurance, and real estate and rental and leasing`)) +
      (`Estimate Public administration_scaled` * sd(`Estimate Public administration`) + mean(`Estimate Public administration`)),
    Blue_Collar_raw = 
      (`Estimate Natural resources, construction, and maintenance occupations_scaled` * 
         sd(`Estimate Natural resources, construction, and maintenance occupations`) +
         mean(`Estimate Natural resources, construction, and maintenance occupations`)) +
      (`Estimate Production, transportation, and material moving occupations_scaled` * 
         sd(`Estimate Production, transportation, and material moving occupations`) +
         mean(`Estimate Production, transportation, and material moving occupations`)) +
      (`Estimate Agriculture, forestry, fishing and hunting, and mining_scaled` * 
         sd(`Estimate Agriculture, forestry, fishing and hunting, and mining`) +
         mean(`Estimate Agriculture, forestry, fishing and hunting, and mining`)) +
      (`Estimate Manufacturing_scaled` * sd(`Estimate Manufacturing`) + mean(`Estimate Manufacturing`)) +
      (`Estimate Retail trade_scaled` * sd(`Estimate Retail trade`) + mean(`Estimate Retail trade`)) +
      (`Estimate Transportation and warehousing, and utilities_scaled` * 
         sd(`Estimate Transportation and warehousing, and utilities`) +
         mean(`Estimate Transportation and warehousing, and utilities`)),
    Other_Services_raw = 
      (`Estimate Educational services, and health care and social assistance_scaled` * 
         sd(`Estimate Educational services, and health care and social assistance`) +
         mean(`Estimate Educational services, and health care and social assistance`)) +
      (`Estimate Arts, entertainment, and recreation, and accommodation and food services_scaled` * 
         sd(`Estimate Arts, entertainment, and recreation, and accommodation and food services`) +
         mean(`Estimate Arts, entertainment, and recreation, and accommodation and food services`)) +
      (`Estimate Other services, except public administration_scaled` * 
         sd(`Estimate Other services, except public administration`) +
         mean(`Estimate Other services, except public administration`)) +
      (`Estimate Self-employed in own not incorporated business workers_scaled` * 
         sd(`Estimate Self-employed in own not incorporated business workers`) +
         mean(`Estimate Self-employed in own not incorporated business workers`)),
    Govt_Benefits_raw = 
      (`Estimate With Social Security_scaled` * sd(`Estimate With Social Security`) + 
         mean(`Estimate With Social Security`)) +
      (`Estimate With retirement income_scaled` * sd(`Estimate With retirement income`) + 
         mean(`Estimate With retirement income`)) +
      (`Estimate With Supplemental Security Income_scaled` * sd(`Estimate With Supplemental Security Income`) + 
         mean(`Estimate With Supplemental Security Income`)) + (`Estimate With health insurance coverage!!With public coverage` * sd(`Estimate With health insurance coverage!!With public coverage`) + 
                                                                  mean(`Estimate With health insurance coverage!!With public coverage`)),
    Welfare_Benefits_raw = 
      (`Estimate With cash public assistance income_scaled` * sd(`Estimate With cash public assistance income`) + 
         mean(`Estimate With cash public assistance income`)) +
      (`Estimate With Food Stamp/SNAP benefits in the past 12 months_scaled` * 
         sd(`Estimate With Food Stamp/SNAP benefits in the past 12 months`) +
         mean(`Estimate With Food Stamp/SNAP benefits in the past 12 months`))
  )

d_modeling$Estaimte
# consolidated predictors
consolidated_predictors <- c(
  "Car_scaled", "Other_Transportation_scaled", "Children_scaled",
  "Corporate_scaled", "Blue_Collar_scaled", "Other_Services_scaled",
  "Govt_Benefits_scaled", "Welfare_Benefits_scaled"
)

# other predictors to keep based on Lasso  judgement
other_predictors <- c("Fed_Rate_scaled",
                      "Unemployment_Rate_scaled", "HomeValue_scaled", "Estimate Worked at home_scaled",
                      "Estimate Mean travel time to work (minutes)_scaled",
                      "Estimate Unpaid family workers_scaled", "Estimate Median household income (dollars)_scaled",
                      "Estimate With health insurance coverage!!With private health insurance_scaled",
                      "Estimate No health insurance coverage_scaled", "Opportunity_Zone_scaled"
)

# final predictors for modeling
final_predictors <- c(other_predictors, consolidated_predictors)

na_rows <- d_modeling %>%
  select(all_of(final_predictors)) %>%
  apply(1, function(x) any(is.na(x)))

d_modeling <- d_modeling[!na_rows, ]

y <- d_modeling$Deal_Volume


## first, let's do some visualizations with the consolidated predictors
## plotting syntax helped by Chat-GPT

plot_predictor_map <- function(data, predictor, map_data, title_suffix = "") {
  # Ensure the predictor exists in the data
  if (!predictor %in% colnames(data)) {
    stop(paste("Predictor", predictor, "not found in the data."))
  }
  
  # Create the plot
  p <- ggplot() +
    # Base map with uniform fill
    geom_polygon(
      data = map_data,
      aes(x = long, y = lat, group = group),
      fill = "lightgray",
      color = "white",
      alpha = 0.7
    ) +
    # Bubble map for Deal Volume colored by predictor
    geom_point(
      data = data,
      aes_string(x = "lon.census.town", y = "lat.census.town", 
                 color = predictor, size = "Deal_Volume"),
      alpha = 0.6
    ) +
    # Color scale for the predictor
    scale_color_viridis_c(option = "C", name = paste(predictor)) +
    # Size scale for Deal Volume
    scale_size_continuous(name = "Deal Volume", range = c(2, 10)) +
    # Labels and Title
    labs(
      title = paste("Deal Volume and", title_suffix, predictor, "by Town"),
      x = "Longitude",
      y = "Latitude"
    ) +
    # Theme settings
    theme_minimal() +
    theme(
      legend.position = "right",
      plot.title = element_text(hjust = 0.5)
    ) +
    # Fixed coordinate ratio
    coord_fixed(1.3)
  
  # Print the plot
  print(p)
}
town_volume <- d_modeling %>%
  group_by(Town, lat.census.town, lon.census.town) %>%
  summarize(
    Deal_Volume = sum(Deal_Volume),
    Unemployment_Rate = mean(Unemployment_Rate, na.rm = TRUE),
    HomeValue = mean(HomeValue, na.rm = TRUE),
    Estimate.Median.household.income..dollars. = mean(`Estimate Median household income (dollars)`, na.rm = TRUE),
    Estimate.No.health.insurance.coverage_scaled = mean(`Estimate No health insurance coverage_scaled`, na.rm = TRUE),
    Welfare_Benefits_scaled = mean(Welfare_Benefits_scaled, na.rm = TRUE),
    Blue_Collar_scaled = mean(Blue_Collar_scaled, na.rm = TRUE),
    .groups = "drop"
  )
california_map <- map_data("state", region = "california")
predictors <- c(
  "Unemployment_Rate",
  "HomeValue",
  "Estimate.Median.household.income..dollars.",
  "Estimate.No.health.insurance.coverage_scaled",
  "Welfare_Benefits_scaled",
  "Blue_Collar_scaled"
)
for (pred in predictors) {
  plot_predictor_map(
    data = town_volume,
    predictor = pred,
    map_data = california_map,
    title_suffix = ""
  )
}
# save plots
for (i in seq_along(predictors)) {
  plot_predictor_map(data = town_volume, predictor = predictors[i], map_data = california_map, title_suffix = "")
  ggsave(filename = sprintf("figures/fig%d.png", 4 + i), plot = last_plot(), width = 10, height = 6, dpi = 300)
}

predictors_for_plotting <- c(
  "Fed_Rate_scaled",
  "Unemployment_Rate_scaled",
  "HomeValue_scaled",
  "Estimate Worked at home_scaled",
  "Estimate Mean travel time to work (minutes)_scaled",
  "Estimate Unpaid family workers_scaled",
  "Estimate Median household income (dollars)_scaled",
  "Estimate With health insurance coverage!!With private health insurance_scaled",
  "Estimate No health insurance coverage_scaled",
  "Opportunity_Zone_scaled",
  "Other_Transportation_scaled",
  "Blue_Collar_scaled",
  "Govt_Benefits_scaled",
  "Welfare_Benefits_scaled"
)
  
# Summarize Deal Volume and select predictors
town_volume <- d_modeling %>%
  group_by(Town, lat.census.town, lon.census.town) %>%
  summarize(
    Deal_Volume = sum(Deal_Volume, na.rm = TRUE),
    across(all_of(predictors_for_plotting), mean, na.rm = TRUE),
    .groups = "drop"
  )

# Reshape data to long format for faceting
town_volume_long <- town_volume %>%
  pivot_longer(
    cols = all_of(predictors_for_plotting),
    names_to = "Predictor",
    values_to = "Value"
  )

ggplot(town_volume_long, aes(x = Value, y = Deal_Volume)) +
  geom_point(alpha = 0.6, aes(color = Value)) +
  geom_smooth(method = "loess", se = FALSE, color = "black") +
  facet_wrap(~ Predictor, scales = "free_x") +
  scale_y_log10() +
  scale_color_viridis_c(option = "C") +
  labs(
    title = "Relationship Between Predictors and Deal Volume Across Towns",
    x = "Scaled Predictor Value",
    y = "Deal Volume (Log Scale)",
    color = "Predictor Value"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 8),
    axis.text = element_text(size = 6),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )
ggsave(filename = "figures/fig11.png", plot = last_plot(), width = 12, height = 8, dpi = 300)

#####
#  LASSO Regression for Variable Selection
######

X <- model.matrix(
  ~ . -1,
  data = d_modeling %>%
    select(final_predictors)
)

lasso_model <- cv.glmnet(X, y, alpha = 1, family = "poisson")
lasso_coefficients <- coef(lasso_model, s = "lambda.1se")
coefficients_matrix <- as.matrix(lasso_coefficients)
non_zero_indices <- which(coefficients_matrix != 0)
selected_predictors <- rownames(coefficients_matrix)[non_zero_indices]
selected_predictors <- selected_predictors[selected_predictors != "(Intercept)"]

print("Selected Predictors according to Lasso")
print(selected_predictors)

### keep this commented, shuffling variables 
### doesn't provide much reduction of WAIC

######
# Model Selection Using WAIC
###### 

# # Define candidate predictors
# candidate_predictors <- final_predictors
# selected_set <- c()
# remaining_set <- candidate_predictors
# 
# best_waic <- Inf
# best_model <- NULL
# best_set <- NULL
# 
# # Prepare spatial elements for INLA
# d_modeling <- d_modeling %>%
#   drop_na(lon.census.town, lat.census.town)
# 
# d_sf <- st_as_sf(d_modeling, coords = c("lon.census.town", "lat.census.town"), crs = 4326)
# d_projected <- st_transform(d_sf, crs = 3310)
# projected_coords <- st_coordinates(d_projected)
# 
# town_coords_unique <- d_projected %>%
#   as.data.frame() %>%
#   distinct(Town, .keep_all = TRUE)
# 
# coords <- as.matrix(st_coordinates(st_as_sf(town_coords_unique, crs = 3310)))
# 
# mesh <- inla.mesh.2d(
#   loc = coords,
#   max.edge = c(50000, 200000),
#   cutoff = 5000
# )
# 
# spde <- inla.spde2.pcmatern(
#   mesh = mesh,
#   alpha = 2,
#   prior.range = c(50000, 0.5),
#   prior.sigma = c(1, 0.1)
# )
# 
# spatial_index <- inla.spde.make.index("spatial.field", n.spde = spde$n.spde)
# 
# town_to_index <- data.frame(
#   Town = town_coords_unique$Town,
#   coord_index = 1:nrow(town_coords_unique)
# )
# 
# d_modeling <- d_modeling %>%
#   left_join(town_to_index, by = "Town")
# 
# # Create the A matrix
# A <- inla.spde.make.A(mesh, loc = coords[d_modeling$coord_index, ])
# 
# # # Run forward selection
# # while (length(remaining_set) > 0) {
# #   improvement <- FALSE
# #   for (p in remaining_set) {
# #     current_set <- c(selected_set, p)
# #     formula_str <- paste0("y ~ -1 + f(spatial.field, model = spde) + f(Period, model = 'ar1') + ", paste(current_set, collapse = " + "))
# #     formula <- as.formula(formula_str)
# #     
# #     # Prepare covariate data
# #     covariate_data <- d_modeling %>%
# #       select(all_of(current_set), Period, Town)
# #     
# #     stack <- inla.stack(
# #       data = list(y = y),
# #       A = list(A, 1),
# #       effects = list(spatial.field = spatial_index, covariate_data),
# #       tag = "data"
# #     )
# #     
# #     # Run the INLA model
# #     result_temp <- inla(
# #       formula,
# #       data = inla.stack.data(stack),
# #       family = "poisson",
# #       control.predictor = list(A = inla.stack.A(stack), compute = TRUE),
# #       control.compute = list(dic = TRUE, waic = TRUE)
# #     )
# #     
# #     waic_val <- result_temp$waic$waic
# #     
# #     if (waic_val < best_waic) {
# #       best_waic <- waic_val
# #       best_model <- result_temp
# #       best_set <- current_set
# #       improvement <- TRUE
# #     }
# #   }
# #   
# #   if (improvement) {
# #     selected_set <- best_set
# #     remaining_set <- setdiff(candidate_predictors, selected_set)
# #   } else {
# #     break
# #   }
# # }
# # 
# # # Print the best set of predictors
# # print(best_set)

# Use the best set for the final model

colnames(d_modeling) <- gsub(" ", ".", colnames(d_modeling))
colnames(d_modeling) <- colnames(d_modeling) %>%
  gsub("\\(|\\)", ".", .) %>%  # Replace parentheses with dots
  gsub("!!", ".", .) 

best_set <- c(
  "Fed_Rate_scaled",
  "Unemployment_Rate_scaled",
  "HomeValue_scaled",
  "Estimate.Worked.at.home_scaled",
  "Estimate.Mean.travel.time.to.work..minutes._scaled",
  "Estimate.Unpaid.family.workers_scaled",
  "Estimate.Median.household.income..dollars._scaled",
  "Estimate.With.health.insurance.coverage.With.private.health.insurance_scaled",
  "Estimate.No.health.insurance.coverage_scaled",
  "Opportunity_Zone_scaled",
  "Other_Transportation_scaled",
  "Blue_Collar_scaled",
  "Govt_Benefits_scaled",
  "Welfare_Benefits_scaled"
)

#########################
### SPATIAL SETUP #######
#########################

# Drop rows with missing coordinates
d_modeling <- d_modeling %>%
  drop_na(lon.census.town, lat.census.town)

na_rows <- d_modeling %>%
  select(all_of(best_set)) %>%
  apply(1, function(x) any(is.na(x)))


d_modeling <- d_modeling[!na_rows, ]

y <- d_modeling$Deal_Volume


# Convert to sf object with given CRS (assume WGS84 for lat/lon)
d_sf <- st_as_sf(d_modeling, coords = c("lon.census.town", "lat.census.town"), crs = 4326)
d_projected <- st_transform(d_sf, crs = 3310)
projected_coords <- st_coordinates(d_projected)

# Extract unique towns with coordinates
town_coords_unique <- d_projected %>%
  as.data.frame() %>%
  distinct(Town, .keep_all = TRUE)

# Get coordinate matrix for towns
coords <- as.matrix(st_coordinates(st_as_sf(town_coords_unique, crs = 3310)))

# Build mesh
mesh <- inla.mesh.2d(
  loc = coords,
  max.edge = c(50000, 200000),  # adjust as needed
  cutoff = 5000
)

plot(mesh)
ggsave(filename = "figures/fig12.png", plot = last_plot(), width = 12, height = 8, dpi = 300)

# Create SPDE model
spde <- inla.spde2.pcmatern(
  mesh = mesh,
  alpha = 2,
  prior.range = c(50000, 0.5),
  prior.sigma = c(1, 0.1)
)

# Make index for spatial field
spatial_index <- inla.spde.make.index("spatial.field", n.spde = spde$n.spde)

# Create a lookup for each Town to its coordinate index
town_index <- data.frame(
  Town = town_coords_unique$Town,
  coord_index = 1:nrow(town_coords_unique)
)

# Join the coordinate index back to d_modeling
d_modeling <- d_modeling %>%
  left_join(town_index, by = "Town")

# Create the A matrix for INLA
A <- inla.spde.make.A(mesh, loc = coords[d_modeling$coord_index, ])

#########################
### CREATE FORMULA ######
#########################

formula_str <- paste0(
  "y ~ -1 + f(spatial.field, model = spde) + f(Period, model = 'ar1') + ",
  paste(best_set, collapse = " + ")
)
formula <- as.formula(formula_str)

#########################
### INLA STACK ##########
#########################
covariate_data <- d_modeling %>%
  select(all_of(best_set), Period, Town)

stack <- inla.stack(
  data = list(y = y),
  A = list(A, 1),
  effects = list(spatial.field = spatial_index,
    covariate_data),
  tag = "data"
)

#########################
### RUN INLA MODEL ######
#########################

# Run the model WITHOUT WAIC, only DIC
best_result <- inla(
  formula,
  data = inla.stack.data(stack),
  family = "poisson",
  control.predictor = list(A = inla.stack.A(stack), compute = TRUE),
  control.compute = list(dic = TRUE, waic = TRUE)
)

# Print summary of results
summary(best_result)

# Plot results (optional)
plot(best_result)
# get fitted values
fitted_values <- best_result$summary.fitted.values$mean[inla.stack.index(stack, tag = "data")$data]

ggplot(data.frame(Observed = y, Fitted = fitted_values), aes(x = Observed, y = Fitted)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Observed vs Fitted Deal Volume - Best Model",
       x = "Observed Deal Volume",
       y = "Fitted Deal Volume") +
  theme_minimal()
ggsave(filename = "figures/fig13.png", plot = last_plot(), width = 10, height = 6, dpi = 300)

### zero inflated ###
zero_inflated <- inla(
  formula,
  data = inla.stack.data(stack),
  family = "zeroinflatedpoisson0",
  control.predictor = list(A = inla.stack.A(stack), compute = TRUE),
  control.compute = list(dic = TRUE, waic = TRUE)
)

summary(zero_inflated)

zero_inflated <- inla(
  formula,
  data = inla.stack.data(stack),
  family = "zeroinflatedpoisson0",
  control.predictor = list(A = inla.stack.A(stack), compute = TRUE),
  control.compute = list(dic = TRUE, waic = TRUE)
)
inla.doc("negativebinomial")
summary(zero_inflated)

### neg bin ###
prediction_result_nb <- inla(
  formula,
  data = inla.stack.data(stack),
  family = "nbinomial2",  
  control.predictor = list(
    A = inla.stack.A(stack), 
    compute = TRUE, 
    link = 1  # Log-link function
  ),
  control.compute = list(
    dic = TRUE, 
    waic = TRUE  
  )
)
summary(prediction_result_nb)
#### Some Visualizations ###################################################

### Latent Space Mean and Variance ####

# Create the projector for the mesh
gproj <- inla.mesh.projector(mesh, dims = c(300, 300))  # Adjust dims for resolution
# Project the mean and standard deviation of the spatial field
g.mean <- inla.mesh.project(gproj, best_result$summary.random$spatial.field$mean)
g.sd <- inla.mesh.project(gproj, best_result$summary.random$spatial.field$sd)


library(lattice)
library(gridExtra)

grid.arrange(
  levelplot(
    g.mean, scales = list(draw = FALSE), xlab = '', ylab = '',
    main = 'Spatial Field Mean', col.regions = viridis::viridis(16)
  ),
  levelplot(
    g.sd, scales = list(draw = FALSE), xlab = '', ylab = '',
    main = 'Spatial Field SD', col.regions = viridis::viridis(16)
  ),
  nrow = 1
)
# Convert projection results to data frames for ggplot2
mean_df <- data.frame(
  x = rep(gproj$x, each = length(gproj$y)),
  y = rep(gproj$y, times = length(gproj$x)),
  value = as.vector(g.mean)
)

sd_df <- data.frame(
  x = rep(gproj$x, each = length(gproj$y)),
  y = rep(gproj$y, times = length(gproj$x)),
  value = as.vector(g.sd)
)


ggplot(mean_df, aes(x = x, y = y, fill = value)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(
    title = "Spatial Field Mean",
    x = "Projected Longitude",
    y = "Projected Latitude",
    fill = "Latent Effect"
  ) +
  theme_minimal()
ggsave(filename = "figures/fig14.png", plot = last_plot(), width = 10, height = 6, dpi = 300)

# Plot the standard deviation
ggplot(sd_df, aes(x = x, y = y, fill = value)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(
    title = "Spatial Field SD",
    x = "Projected Longitude",
    y = "Projected Latitude",
    fill = "Latent Effect"
  ) +
  theme_minimal()
ggsave(filename = "figures/fig15.png", plot = last_plot(), width = 10, height = 6, dpi = 300)

### Visualize Posterior Distribution of Predictors ###

# Visualize Posterior distribution across townships
str(best_result)
fixed_names <- best_set


for (fn in fixed_names) {
  # Open a PNG device
  png(filename = sprintf("figures/fig%d.png", 16 + which(fixed_names == fn)), 
      width = 10, height = 6, units = "in", res = 300)
  
  # Generate the Base R plot
  marginal <- best_result$marginals.fixed[[fn]]
  plot(marginal, type = "l", main = paste("Posterior Marginal of", fn),
       xlab = fn, ylab = "Density")
  abline(v = inla.emarginal(function(x) x, marginal), col = "red") # Add mean line
  
  # Close the PNG device
  dev.off()
}

# visualize temporal dependence
best_result$summary.hyperpar
png(filename = "figures/fig31.png", width = 10, height = 6, units = "in", res = 300)
rho_marginal <- best_result$marginals.hyperpar[["Rho for Period"]]
plot(rho_marginal, type = "l", main = "Posterior of AR1 Rho", xlab = "Rho", ylab = "Density")
abline(v = inla.emarginal(function(x) x, rho_marginal), col = "red") # Posterior mean
dev.off()

# visualize spatial range
range_marginal <- best_result$marginals.hyperpar[["Range for spatial.field"]]

# Plot the posterior distribution
png(filename = "figures/fig32.png", width = 10, height = 6, units = "in", res = 300)
plot(range_marginal, type = "l",
     main = "Posterior Distribution of Spatial Range",
     xlab = "Spatial Range (in units of mesh projection, e.g., meters)",
     ylab = "Density")
abline(v = inla.emarginal(function(x) x, range_marginal), col = "red", lty = 2, lwd = 2)
legend("topright", legend = "Posterior Mean", col = "red", lty = 2, bty = "n")
dev.off()

period_numeric <- d_modeling$Period_numeric
index_est <- inla.stack.index(stack, tag = "data")$data
fitted_mean <- best_result$summary.fitted.values$mean[index_est]
fitted_lower <- best_result$summary.fitted.values$`0.025quant`[index_est]
fitted_upper <- best_result$summary.fitted.values$`0.975quant`[index_est]

# Aggregate by period if multiple towns per period
period_summary <- data.frame(period_numeric, Fitted = fitted_mean, Lower = fitted_lower, Upper = fitted_upper) %>%
  group_by(period_numeric) %>%
  summarise(Fitted = mean(Fitted), Lower = mean(Lower), Upper = mean(Upper))

ggplot(period_summary, aes(x = period_numeric, y = Fitted)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.2) +
  labs(title = "Fitted Deal Volume Over Time",
       x = "Time (Period Numeric)",
       y = "Fitted Deal Volume") +
  theme_minimal()
ggsave(filename = "figures/fig33.png", plot = last_plot(), width = 10, height = 6, dpi = 300)


###########################################################################

#### PREDICTING FUTURE DEAL FLOW #########################################

### Define future period #### 

# Create a new `d_modeling_future` with NA rows, setting Period to 2024-01-01
d_modeling_future <- data.frame(Town = unique(d_modeling$Town))
d_modeling_future$Period <- as.Date("2024-01-01")

# Step 2: Add missing columns from `best_set` as NA
required_columns <- c("Town", "Period", best_set)
for (col in setdiff(required_columns, colnames(d_modeling_future))) {
  d_modeling_future[[col]] <- NA
}

d_modeling$Period <- as.Date(d_modeling$Period)
# Step 3: Process Home Value data
home_value_2024 <- read.csv("Cleaneddata/z_home_value_unique.csv") %>%
  mutate(Date = as.Date(Date)) %>%
  filter(Date > as.Date("2023-06-01") & Date < as.Date("2024-01-01")) %>%
  group_by(City) %>%
  summarize(HomeValue = mean(HomeValue, na.rm = TRUE)) %>%
  filter(City %in% unique(d_modeling_future$Town)) %>%
  mutate(HomeValue_scaled = scale(HomeValue, center = mean(d_modeling$HomeValue, na.rm = TRUE),
                                  scale = sd(d_modeling$HomeValue, na.rm = TRUE)))

# Join Home Value data to `d_modeling_future`
d_modeling_future <- d_modeling_future %>%
  left_join(home_value_2024 %>% select(City, HomeValue_scaled) %>%
              rename(Town = City),
            by = "Town")

# Step 4: Process Federal Rate data
fed_2024 <- read.csv("Cleaneddata/fed_cleaned.csv") %>%
  mutate(Date = as.Date(Date)) %>%
  filter(Date > as.Date("2023-06-01") & Date < as.Date("2024-01-01")) %>%
  summarize(Fed_Rate = mean(Rate, na.rm = TRUE)) %>%
  mutate(Fed_Rate_scaled = scale(Fed_Rate, center = mean(d_modeling$Fed_Rate, na.rm = TRUE),
                                 scale = sd(d_modeling$Fed_Rate, na.rm = TRUE))) %>%
  select(Fed_Rate_scaled)

# Assign Federal Rate data to `d_modeling_future`
d_modeling_future$Fed_Rate_scaled <- rep(fed_2024$Fed_Rate_scaled, nrow(d_modeling_future))

## Process BLS data

bls <- read_excel("Rawdata/BLS_Rawdata.xlsx")

# set colnames to be row three values
colnames(bls) <- bls[2, ]
# remove first three rows
bls <- bls[-c(1:3), ]

bls <- bls[bls$`ST FIPS Code` == "06", ]

bls$Area <- gsub(",.*", "", bls$Area)


bls <- bls %>% 
  separate_rows(Area, sep = "-{1,2}") %>%
  mutate(Area = trimws(Area))

bls <- bls[!is.na(bls$Area), ]

bls$Date <- as.Date(paste(bls$Year, bls$Month, "01", sep="-"), format = "%Y-%m-%d")
bls$`Unemployment Rate` <- as.numeric(bls$`Unemployment Rate`)

bls <- bls %>%
  select(Area,`Unemployment Rate`, Date) %>% filter(Date > as.Date("2023-06-01") & Date < as.Date("2024-01-01")) %>% group_by(Area) %>% summarize(`Unemployment Rate` = mean(as.numeric(`Unemployment Rate`), na.rm = TRUE)) 

nearest_matches <- read.csv("Cleaneddata/nearest_matches_bls.csv")
nearest_matches$Town <- gsub(", CA", "", nearest_matches$Town)
nearest_matches$Area <- gsub(", CA", "", nearest_matches$Area)

d_modeling_future <- d_modeling_future %>%
  left_join(nearest_matches, by = "Town") %>%
  left_join(bls, by = c("Area" = "Area"))

d_modeling_future$Unemployment_Rate_scaled <- scale(d_modeling_future$`Unemployment Rate`, center = mean(d_modeling$Unemployment_Rate, na.rm = TRUE),
                                                    scale = sd(d_modeling$Unemployment_Rate, na.rm = TRUE))

# Define predictors to keep the same as 2023-01-01
predictors_constant <- c("Opportunity_Zone_scaled")

predictors_median_growth <- c(
  "Estimate.Worked.at.home", 
  "Welfare_Benefits_raw", 
  "Other_Transportation_raw",
  "Estimate.Median.household.income..dollars.",
  "Govt_Benefits_raw", 
  "Estimate.No.health.insurance.coverage",
  "Estimate.Unpaid.family.workers", 
  "Estimate.Mean.travel.time.to.work..minutes.",
  "Estimate.With.health.insurance.coverage.With.private.health.insurance",
  "Estimate.No.health.insurance.coverage", 
  "Blue_Collar_raw"
)

# Step 1: Calculate Growth Rates
growth_data_per_town <- d_modeling %>%
  filter(Period %in% as.Date(c("2018-01-01", "2023-01-01"))) %>%
  select(Town, Period, all_of(predictors_median_growth)) %>%
  pivot_wider(
    names_from = Period, 
    values_from = all_of(predictors_median_growth), 
    names_sep = "_Value_"
  ) %>%
  # Replace NA with 0 for both periods
  mutate(across(ends_with("_Value_2018-01-01"), ~ replace_na(., 0))) %>%
  mutate(across(ends_with("_Value_2023-01-01"), ~ replace_na(., 0)))

for (predictor in predictors_median_growth) {
  start_col <- paste0(predictor, "_Value_2018-01-01")
  end_col <- paste0(predictor, "_Value_2023-01-01")
  growth_col <- paste0("GrowthRate_", predictor)
  
  # Compute growth rate with handling for zero start values
  growth_data_per_town <- growth_data_per_town %>%
    mutate(
      !!growth_col := case_when(
        abs(.data[[start_col]]) < 1e-6 & abs(.data[[end_col]]) < 1e-6 ~ 0,
        abs(.data[[start_col]]) < 1e-6 & abs(.data[[end_col]]) >= 1e-6 ~ NA_real_,
        TRUE ~ (.data[[end_col]] - .data[[start_col]]) / abs(.data[[start_col]])
      )
    )
}

# Step 2: Reshape and Adjust Growth Rates
growth_long <- growth_data_per_town %>%
  select(Town, starts_with("GrowthRate_")) %>%
  pivot_longer(
    cols = starts_with("GrowthRate_"),
    names_to = "Predictor",
    names_prefix = "GrowthRate_",
    values_to = "GrowthRate"
  )

# Identify extreme outliers per predictor
growth_long <- growth_long %>%
  group_by(Predictor) %>%
  mutate(
    Q1 = quantile(GrowthRate, 0.25, na.rm = TRUE),
    Q3 = quantile(GrowthRate, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    Lower_Bound = Q1 - 1.5 * IQR,
    Upper_Bound = Q3 + 1.5 * IQR,
    Is_Outlier = GrowthRate < Lower_Bound | GrowthRate > Upper_Bound
  ) %>%
  ungroup()

# Calculate the 75th quartile and median growth rates per predictor
growth_quartiles <- growth_long %>%
  group_by(Predictor) %>%
  summarize(
    Q3_GrowthRate = quantile(GrowthRate, 0.75, na.rm = TRUE),
    Median_GrowthRate = median(GrowthRate, na.rm = TRUE)
  )

# Join quartile information back to the growth_long data
growth_long <- growth_long %>%
  left_join(growth_quartiles, by = "Predictor") %>%
  mutate(
    Adjusted_GrowthRate = case_when(
      Is_Outlier ~ Q3_GrowthRate,                                     # Assign 75th quartile if outlier
      is.na(GrowthRate) ~ Median_GrowthRate,                         # Assign median if growth rate was NA (start=0 & end!=0)
      TRUE ~ GrowthRate                                                # Otherwise, keep original growth rate
    )
  )

# Reshape back to wide format with adjusted growth rates
adjusted_growth_data <- growth_long %>%
  select(Town, Predictor, Adjusted_GrowthRate) %>%
  pivot_wider(
    names_from = Predictor, 
    values_from = Adjusted_GrowthRate, 
    names_prefix = "GrowthRate_"
  )

# Step 3: Define `latest_values` for "2023-01-01"
latest_values <- d_modeling %>%
  filter(Period == as.Date("2023-01-01")) %>%
  select(Town, all_of(predictors_constant), all_of(predictors_median_growth))

# Handle missing values if any
latest_values <- latest_values %>%
  mutate(across(all_of(predictors_median_growth), ~ replace_na(.x, 0)))

# Step 4: Merge `latest_values` with `adjusted_growth_data`
projected_values <- latest_values %>%
  left_join(adjusted_growth_data, by = "Town")

# Step 5: Project 2024 Values
# Project constant predictors (keep them the same)
projected_values <- projected_values %>%
  mutate(across(all_of(predictors_constant), ~ .x, .names = "{col}_projected"))

# Project median growth predictors
projected_values <- projected_values %>%
  mutate(across(
    .cols = all_of(predictors_median_growth),
    .fns = ~ .x * (1 + get(paste0("GrowthRate_", cur_column()))),
    .names = "{col}_projected"
  ))

# Handle NAs in projected growth rates (optional)
projected_values <- projected_values %>%
  mutate(across(
    .cols = all_of(predictors_median_growth),
    .fns = ~ ifelse(is.na(get(paste0("GrowthRate_", cur_column()))), .x, .x),
    .names = "{col}_projected"
  ))

# Step 6: Scale Projected Values
projected_normalized <- projected_values %>%
  mutate(across(
    ends_with("_projected"), 
    ~ as.numeric(scale(.x, 
                       center = mean(d_modeling[[gsub("_projected", "", cur_column())]], na.rm = TRUE),
                       scale = sd(d_modeling[[gsub("_projected", "", cur_column())]], na.rm = TRUE))),
    .names = "{col}_scaled"
  )) %>%
  select(Town, ends_with("_scaled"))

# Merge with projected_normalized
d_modeling_future <- d_modeling_future %>%
  left_join(projected_normalized, by = "Town")


# Select relevant columns and clean column names
d_modeling_future <- d_modeling_future %>%
  select(
    Town, Period, 
    Fed_Rate_scaled, Unemployment_Rate_scaled, HomeValue_scaled.y, 
    Opportunity_Zone_scaled.y, Estimate.Worked.at.home_projected_scaled, 
    Welfare_Benefits_raw_projected_scaled, Other_Transportation_raw_projected_scaled,
    Govt_Benefits_raw_projected_scaled, Estimate.With.health.insurance.coverage.With.private.health.insurance_projected_scaled,
    Estimate.No.health.insurance.coverage_projected_scaled, 
    Estimate.Unpaid.family.workers_projected_scaled, 
    Estimate.Mean.travel.time.to.work..minutes._projected_scaled, Blue_Collar_raw_projected_scaled, Estimate.Median.household.income..dollars._projected_scaled
  ) %>%
  rename_with(
    ~ gsub("_raw", "", .), 
    ends_with("_raw_projected_scaled")
  ) %>%
  rename_with(
    ~ gsub("_projected", "", .), 
    ends_with("_projected_scaled")
  ) %>%
  rename_with(
    ~ gsub("\\.y$", "", .), 
    ends_with(".y")
  )


d_modeling_future <- d_modeling_future %>%
  left_join(
    d_modeling %>%
      group_by(Town) %>%
      slice(1) %>%  # Take the first row for each Town
      ungroup() %>%
      select(Town, coord_index),  # Only keep the necessary column
    by = "Town"
  )

d_modeling$Period <- as.factor(d_modeling$Period)

##### Fit INLA Model ####################################

# Ensure `2024-01-01` is a level in `Period`
d_modeling_future$Period <- factor(
  d_modeling_future$Period,
  levels = c(levels(d_modeling$Period), "2024-01-01")  # Add the new level
)

# Create the future projection matrix for spatial effects
A_future <- inla.spde.make.A(
  mesh = mesh,
  loc = coords[d_modeling_future$coord_index, ]
)
d_modeling_future %>% colnames()

# Define the prediction stack
stack_future <- inla.stack(
  data = list(y = rep(NA, nrow(d_modeling_future))),  # Placeholder for predictions
  A = list(A_future, 1),  # Projection matrices
  effects = list(
    spatial.field = spatial_index,  # Spatial field effects
    d_modeling_future %>% select(all_of(best_set), Period)  # Fixed effects and time effect
  ),
  tag = "future"
)

# Combine the original stack with the prediction stack
stack_combined <- inla.stack(stack, stack_future)

# Use the existing model to generate predictions
prediction_result <- inla(
  formula,
  data = inla.stack.data(stack_combined),
  family = "poisson",
  control.predictor = list(A = inla.stack.A(stack_combined), compute = TRUE, link=1),
  control.compute = list(dic = TRUE, waic = TRUE)  # Disable diagnostics for predictions
)


# get predictions
# note, these are on the count scale, not the log scale

index_future <- inla.stack.index(stack_combined, tag = "future")$data
future_predictions <- prediction_result$summary.fitted.values[index_future, ]

# Add predictions to `d_modeling_future`
d_modeling_future <- d_modeling_future %>%
  mutate(
    Predicted_Mean = future_predictions$mean,
    Predicted_SD = future_predictions$sd,
    Predicted_Lower = future_predictions$`0.025quant`,
    Predicted_Upper = future_predictions$`0.975quant`
  )

index_original <- inla.stack.index(stack, tag = "data")$data

fitted_values <- best_result$summary.fitted.values[index_original, ]



#### compare with actual deal data ####

d <- read_excel("Rawdata/Preqin_Rawdata.xlsx")
d <- data.frame(d)
columns_to_check <- c(
  "DEAL.NAME", "DEAL.DATE", "DEAL.TYPE", "PRIMARY.LOCATION", "PRIMARY.ASSET.TYPE", 
  "DEAL.OVERVIEW", "INVESTORS...BUYERS..FIRMS.", "BOUGHT.FROM...SELLERS..FIRMS.", 
  "ASSET.REGIONS", "ASSET.COUNTRIES", "ASSET.STATES", 
  "ASSET.CITIES", "ASSETS.TYPES", "ASSETS", "BUYER.TYPE", "SELLER.TYPE"
)

# omit rows with NA in the specified columns
d_cleaned <- d[complete.cases(d[, columns_to_check]), ]
columns_to_add_back <- c("DEAL.SIZE..USD.MN.", "NO..OF.ASSETS", "TOTAL.SIZE..SQ..FT..")
d_final <- d_cleaned[, c(columns_to_check, columns_to_add_back)]
# clean column names
clean_colnames <- function(col_names) {
  col_names <- gsub("\\.+", ".", col_names)
  col_names <- sapply(strsplit(col_names, "\\."), function(parts) {
    parts <- str_to_title(parts)
    paste(parts, collapse = ".")
  })
  return(col_names)
}
colnames(d_final) <- clean_colnames(colnames(d_final))
# get only california
ca = d_final[d_final$Asset.States == "CA", ]
# keep only first location for each deal
ca$Asset.Cities <- gsub(",.*", "", ca$Asset.Cities)
# misc fix
ca[ca$Asset.Cities == "Palo Alta", "Asset.Cities" ] <- "Palo Alto"
ca$Deal.Date <- as.Date(ca$Deal.Date, format = "%m/%d/%Y")
ca <- ca %>% filter(Deal.Date > as.Date("2023-06-01") & Deal.Date < as.Date("2024-01-01"))
nearest_matches_deal <- read.csv("Cleaneddata/nearest_matches_deal.csv") %>% select(Town, Asset.Cities)

ca <- ca %>% left_join(nearest_matches_deal, by = c("Asset.Cities" = "Asset.Cities"))
town_deal_data <- ca %>% group_by(Town) %>% summarize(Deal_Volume = n())
all_towns <- d_modeling_future %>%
  select(Town) %>%
  distinct()

town_deal_data_complete <- all_towns %>%
  left_join(town_deal_data, by = "Town") %>%
  mutate(Deal_Volume = replace_na(Deal_Volume, 0))


### make some plots
### Chat-GPT used for most syntax here

unique_coords <- d_modeling %>%
  select(Town, lat.census.town, lon.census.town) %>%
  distinct(Town, .keep_all = TRUE)
d_modeling_future_plotting <- d_modeling_future %>%
  left_join(unique_coords, by = "Town")

# merge predicted and actual deal volumes
comparison_data <- d_modeling_future_plotting %>%
  select(Town, Predicted_Mean, Predicted_SD, Predicted_Lower, Predicted_Upper, lat.census.town, lon.census.town) %>%
  left_join(town_deal_data_complete, by = "Town") %>%
  rename(Actual_Deals = Deal_Volume) %>%
  mutate(Actual_Deals = replace_na(Actual_Deals, 0))

comparison_data_positive <- comparison_data %>%
  filter(Actual_Deals > 0)

comparison_data_ordered <- comparison_data_positive %>%
  arrange(Predicted_Mean) %>%
  mutate(Town = factor(Town, levels = Town))

predicted_means_plot <- ggplot(comparison_data_ordered, aes(x = Predicted_Mean, y = Town)) +
  geom_point(color = "blue", size = 2) +
  geom_errorbarh(aes(xmin = Predicted_Lower, xmax = Predicted_Upper), height = 0.2, color = "blue") +
  labs(
    title = "Predicted Deal Volumes by Town with 95% Credible Intervals",
    x = "Predicted Deal Volume",
    y = "Town"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.y = element_text(size = 6)
  )

# Display the Predicted Means Plot
print(predicted_means_plot)
ggsave(filename = "figures/fig34.png", plot = predicted_means_plot, width = 10, height = 6, dpi = 300)


bar_chart_data <- comparison_data_positive %>%
  select(Town, Predicted_Mean, Predicted_Upper, Actual_Deals) %>%
  pivot_longer(
    cols = c(Predicted_Mean, Predicted_Upper, Actual_Deals),
    names_to = "Type",
    values_to = "Deal_Count"
  )

bar_chart <- ggplot(bar_chart_data, aes(x = reorder(Town, -Deal_Count), y = Deal_Count, fill = Type)) +
  geom_col(position = position_dodge(width = 0.8)) +
  labs(
    title = "Predicted Means, Predicted Upper Quartile vs Actual Deals by Town",
    x = "Town",
    y = "Deal Volume",
    fill = "Deal Type"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.title = element_text(face = "bold")
  )

print(bar_chart)
ggsave(filename = "figures/fig35.png", plot = bar_chart, width = 10, height = 6, dpi = 300)


comparison_data_map <- comparison_data_positive %>%
  mutate(
    Color = case_when(
      Actual_Deals > Predicted_Upper ~ "red",
      Actual_Deals < Predicted_Upper ~ "green",
      TRUE ~ "gray"
    )
  )

# map plot to visualize predicted deal volume
map_plot <- ggplot() +
  # Base California map
  geom_polygon(data = california_map, aes(x = long, y = lat, group = group),
               fill = "gray95", color = "white") +
  # Points for predicted and actual deal comparison
  geom_point(data = comparison_data_map, 
             aes(x = lon.census.town, y = lat.census.town, size = Predicted_Upper, color = Color),
             alpha = 0.7) +
  scale_color_manual(
    values = c("red" = "red", "green" = "green", "gray" = "gray"),
    labels = c("Actual > Predicted Upper", "Actual < Predicted Upper", "Actual = Predicted Upper")
  ) +
  scale_size_continuous(
    range = c(2, 6),
    name = "Predicted Upper Quartile"
  ) +
  labs(
    title = "Map of California: Predicted Upper Quartile Deal Volumes",
    x = "Longitude",
    y = "Latitude",
    color = "Deal Comparison"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  coord_fixed(1.3) +
  # Add labels for towns where Actual > Predicted Upper (Red) and Actual < Predicted Upper (Green)
  geom_text_repel(
    data = subset(comparison_data_map, Color == "red" | Color == "green"),
    aes(x = lon.census.town, y = lat.census.town, label = Town),
    size = 3,
    max.overlaps = 20
  )

# Display the Updated Map Plot
print(map_plot)
ggsave(filename = "figures/fig36.png", plot = map_plot, width = 10, height = 6, dpi = 300)
