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

## NEED TO ADD VISUALIZATIONS HERE ##

## Los Angeles Leaflet Plot ###
## Syntax aided by Chat-GPT ##

df <- readRDS("Data.rds")

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

#### RUNNING THE INLA MODEL ####
d_modeling <- d_aggregated


d_modeling <- d_modeling %>%
  select(-`Estimate With earnings`, `Estimate With health insurance coverage`)

all_numeric_columns <- names(d_modeling)[4:52]

# dont want to normalize coordinates, lets leave them out
## additionally, want to leave out Percentage, Total Deal Size, Sq. Ft, etc.
## they proxies for deal volume

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
      `Estimate With Supplemental Security Income_scaled`+ `Estimate With health insurance coverage!!With public coverage`,
    Welfare_Benefits_scaled = `Estimate With cash public assistance income_scaled` +
      `Estimate With Food Stamp/SNAP benefits in the past 12 months_scaled`
  )

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

# consolidated predictors
consolidated_predictors <- c(
  "Car_scaled", "Other_Transportation_scaled", "Children_scaled",
  "Corporate_scaled", "Blue_Collar_scaled", "Other_Services_scaled",
  "Govt_Benefits_scaled", "Welfare_Benefits_scaled"
)

# other predictors to keep based on Lasso % judgement
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
  control.compute = list(dic = TRUE, waic = TRUE),
  verbose = TRUE
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


png("spatial_field_mean.png", width = 800, height = 600)
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
dev.off()

# Plot the mean


# Plot the standard deviation
png("spatial_field_sd.png", width = 800, height = 600)
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

### Visualize Posterior Distribution of Predictors ###

# Visualize Posterior distribution across townships
str(best_result)
fixed_names <- best_set
for (fn in fixed_names) {
  marginal <- best_result$marginals.fixed[[fn]]
  plot(marginal, type = "l", main = paste("Posterior Marginal of", fn),
       xlab = fn, ylab = "Density")
  abline(v = inla.emarginal(function(x) x, marginal), col = "red") # mean
}

#visualize difference between LA and SF
# Extract marginal data for a specific predictor (example: "Opportunity_Zone_scaled")
opportunity_zone_marginals <- best_result$marginals.fixed$Fed_Rate_scaled
marginal_df <- as.data.frame(opportunity_zone_marginals)
colnames(marginal_df) <- c("Value", "Density")

# Add town labels (for illustrative purposes)
marginal_df$Town <- ifelse(runif(nrow(marginal_df)) > 0.5, "San Diego", "San Francisco") # Replace with real labels

# Plot density
ggplot(marginal_df, aes(x = Value, y = Density, color = Town)) +
  geom_line(size = 1) +
  labs(
    title = "Posterior Distribution for 'Opportunity_Zone_scaled'",
    x = "Value",
    y = "Density"
  ) +
  theme_minimal()

# visualize temporal dependence
best_result$summary.hyperpar

rho_marginal <- best_result$marginals.hyperpar[["Rho for Period"]]
plot(rho_marginal, type = "l", main = "Posterior of AR1 Rho", xlab = "Rho", ylab = "Density")
abline(v = inla.emarginal(function(x) x, rho_marginal), col = "red") # Posterior mean

# visualize spatial range
range_marginal <- best_result$marginals.hyperpar[["Range for spatial.field"]]

# Plot the posterior distribution
plot(range_marginal, type = "l",
     main = "Posterior Distribution of Spatial Range",
     xlab = "Spatial Range (in units of mesh projection, e.g., meters)",
     ylab = "Density")
abline(v = inla.emarginal(function(x) x, range_marginal), col = "red", lty = 2, lwd = 2)
legend("topright", legend = "Posterior Mean", col = "red", lty = 2, bty = "n")

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
home_value_2024 <- read.csv("z_home_value_unique.csv") %>%
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
fed_2024 <- read.csv("fed_cleaned.csv") %>%
  mutate(Date = as.Date(Date)) %>%
  filter(Date > as.Date("2023-06-01") & Date < as.Date("2024-01-01")) %>%
  summarize(Fed_Rate = mean(Rate, na.rm = TRUE)) %>%
  mutate(Fed_Rate_scaled = scale(Fed_Rate, center = mean(d_modeling$Fed_Rate, na.rm = TRUE),
                                 scale = sd(d_modeling$Fed_Rate, na.rm = TRUE))) %>%
  select(Fed_Rate_scaled)

# Assign Federal Rate data to `d_modeling_future`
d_modeling_future$Fed_Rate_scaled <- rep(fed_2024$Fed_Rate_scaled, nrow(d_modeling_future))

# Define predictors to keep the same as 2023-01-01
predictors_constant <- c(
  "Residential_Percentage", "Industrial_Percentage",
  "Office_Percentage", "Other_Percentage", "Retail_Percentage",
  "Total_Size_Sq_Ft", "Opportunity_Zone"
)

# Define predictors to use median growth rate
predictors_median_growth <- c(
  "Estimate.Worked.at.home", "Welfare_Benefits_raw", "Other_Services_raw",
  "Govt_Benefits_raw", "Estimate.No.health.insurance.coverage",
  "Estimate.Unpaid.family.workers", "Estimate.Mean.travel.time.to.work..minutes."
)

growth_data_per_town <- d_modeling %>%
  filter(Period %in% as.Date(c("2018-01-01", "2023-01-01"))) %>%
  select(Town, Period, all_of(predictors_median_growth)) %>%
  pivot_wider(names_from = Period, values_from = all_of(predictors_median_growth), 
              names_sep = "_Value_") %>%
  mutate(across(ends_with("_Value_2018-01-01"), ~ replace_na(., 0))) %>%
  mutate(across(ends_with("_Value_2023-01-01"), ~ replace_na(., 0))) %>%
  rowwise() %>%
  mutate(across(all_of(predictors_median_growth), ~ {
    start_col <- paste0(cur_column(), "_Value_2018-01-01")
    end_col <- paste0(cur_column(), "_Value_2023-01-01")
    start_value <- cur_data()[[start_col]]
    end_value <- cur_data()[[end_col]]
    
    if (abs(start_value) < 1e-6) {
      if (abs(end_value) < 1e-6) {
        return(0)  # Zero growth if both are zero
      } else {
        return(NA) # Mark as needing median assignment
      }
    }
    
    growth_rate <- (end_value - start_value) / abs(start_value)
    
    return(growth_rate)
  }, .names = "GrowthRate_{col}")) %>%
  ungroup()

# Reshape growth_data_per_town to long format for easier processing
growth_long <- growth_data_per_town %>%
  select(Town, starts_with("GrowthRate_")) %>%
  pivot_longer(
    cols = starts_with("GrowthRate_"),
    names_to = "Predictor",
    names_prefix = "GrowthRate_",
    values_to = "GrowthRate"
  )

# Identify extreme outliers per predictor
# Define what constitutes an extreme outlier, e.g., beyond 1.5 * IQR from Q1 and Q3
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
  pivot_wider(names_from = Predictor, values_from = Adjusted_GrowthRate, 
              names_prefix = "GrowthRate_")

# Ensure column names match the predictors_median_growth
adjusted_growth_data <- adjusted_growth_data %>%
  rename_with(~ gsub("GrowthRate_", "", .), starts_with("GrowthRate_"))

# Step 4: Project 2024 values using adjusted growth rates

# Merge the adjusted growth rates with the latest_values
projected_values <- latest_values %>%
  left_join(adjusted_growth_data, by = "Town") %>%
  mutate(across(
    all_of(predictors_constant),
    ~ .x, # Keep constant values as is
    .names = "{col}_projected"
  )) %>%
  mutate(across(
    all_of(predictors_median_growth),
    ~ .x * (1 + get(.x)), # Use the adjusted growth rate
    .names = "{col}_projected"
  ))

# Normalize projected values and map to '_scaled'
projected_normalized <- projected_values %>%
  mutate(across(ends_with("_projected"), ~ scale(.x, center = mean(d_modeling[[gsub("_projected", "", cur_column())]], na.rm = TRUE),
                                                 scale = sd(d_modeling[[gsub("_projected", "", cur_column())]], na.rm = TRUE)),
                .names = "{col}_scaled")) %>%
  select(Town, ends_with("_scaled"))

# Join projected values to `d_modeling_future`
d_modeling_future <- d_modeling_future %>%
  left_join(projected_normalized, by = "Town")


# Select relevant columns and clean column names
# Select relevant columns and clean column names
d_modeling_future <- d_modeling_future %>%
  select(
    Town, Period, 
    Fed_Rate_scaled, HomeValue_scaled.y, 
    Residential_Percentage_projected_scaled, Other_Percentage_projected_scaled,     
    Industrial_Percentage_projected_scaled, Office_Percentage_projected_scaled, 
    Retail_Percentage_projected_scaled, Total_Size_Sq_Ft_projected_scaled, 
    Opportunity_Zone_projected_scaled, Estimate.Worked.at.home_projected_scaled, 
    Welfare_Benefits_raw_projected_scaled, Other_Services_raw_projected_scaled, 
    Govt_Benefits_raw_projected_scaled, 
    Estimate.No.health.insurance.coverage_projected_scaled, 
    Estimate.Unpaid.family.workers_projected_scaled, 
    Estimate.Mean.travel.time.to.work..minutes._projected_scaled
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
  control.predictor = list(A = inla.stack.A(stack_combined), compute = TRUE, link = 1),
  control.compute = list(dic = TRUE, waic = TRUE)  # Disable diagnostics for predictions
)

# Extract predictions
index_future <- inla.stack.index(stack_combined, tag = "future")$data
future_predictions <- prediction_result$summary.fitted.values[index_future, ]

summary(future_predictions)
# Add predictions to `d_modeling_future`
d_modeling_future <- d_modeling_future %>%
  mutate(
    Predicted_Mean = exp(future_predictions$mean),
    Predicted_SD = exp(future_predictions$sd),  # Note: This is not standard deviation in count space but transformed
    Predicted_Lower = exp(future_predictions$`0.025quant`),
    Predicted_Upper = exp(future_predictions$`0.975quant`)
  )

# Inspect transformed predictions
head(d_modeling_future)

# Inspect the predicted results
head(d_modeling_future)
d_modeling_future %>% select(Town, Period, Predicted_Mean, Predicted_Lower, Predicted_Upper)


# Extract the indices for the original data from the INLA stack
index_original <- inla.stack.index(stack, tag = "data")$data

# Extract the fitted values from the INLA model
# These are typically on the log scale for a Poisson model
fitted_values <- best_result$summary.fitted.values[index_original, ]

# Transform predictions to the response scale (exponentiation)
d_modeling <- d_modeling %>%
  mutate(
    Predicted_Mean = fitted_values$mean,
    Predicted_Lower = fitted_values$`0.025quant`,
    Predicted_Upper = fitted_values$`0.975quant`
  )

# Display the updated dataset with predictions
head(d_modeling) %>% select(Town, Period, Predicted_Mean, Predicted_Lower, Predicted_Upper)


