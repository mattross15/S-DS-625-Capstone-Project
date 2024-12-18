library(readxl)
library(stringr)
library(dplyr)
library(tidyverse)
library(tidyr)
library(lubridate)
library(tigris)
library(tidycensus)
library(sf)
library(tidygeocoder)
library(geosphere)
library(pbapply)
library(furrr)
library(usethis)

###  PREQIN DATA ####################################

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

####################################################


### BLS DATA #######################################

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

bls <- bls %>%
  select(Area, 'Civilian Labor Force', Employment, Unemployment, 'Unemployment Rate', Date)

####################################################

### FED DATA #######################################

fed <- read_excel("Rawdata/Fed_Rawdata.xls")
fed <- fed[-c(1:10), ]
colnames(fed) <- c("Date", "Rate")
fed <- fed %>%
  mutate(
    Date = as.Date(as.numeric(Date), origin = "1899-12-30"),
    Rate = as.numeric(Rate)
  )

write.csv(fed, "Cleaneddata/fed_cleaned.csv", row.names = FALSE)

####################################################

### ZILLOW DATA #######################################

z_home_value <- read.csv("/Users/mross/Downloads/Zip_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv")
z_home_value <- z_home_value[z_home_value$State == "CA", ]
z_home_value <- z_home_value %>%
  select(-c(RegionID, SizeRank, RegionName, RegionType, StateName, State, Metro))
names(z_home_value) <- names(z_home_value) %>%
  sub("^X", "", .) %>%       # Remove leading 'X'
  gsub("\\.", "-", .) 
date_columns <- setdiff(names(z_home_value), c("City", "CountyName"))
date_columns_after_2010 <- date_columns[as.Date(date_columns) >= as.Date("2010-01-01")]
z_home_value <- z_home_value %>%
  select(City, CountyName, all_of(date_columns_after_2010)) %>%
  pivot_longer(cols = -c(City, CountyName), names_to = "Date", values_to = "HomeValue")

# change date formulation to first day of next month, not end of month
z_home_value$Date <- as.Date(z_home_value$Date)
z_home_value$Date <- ceiling_date(z_home_value$Date, unit = "month")

####################################################

### ACS DATA #######################################

folder_path <- "Rawdata/ACS_Rawdata_ByCity"

# files we need have "Data" in their name
csv_files <- list.files(path = folder_path, pattern = "Data.*\\.csv$", full.names = TRUE)

extract_year <- function(filename) {
  year <- sub(".*ACSDP1Y(20\\d{2}).*", "\\1", filename)
  return(as.numeric(year))
}
# Read and bind the data
acs <- do.call(rbind, lapply(csv_files, function(file) {
  data <- read.csv(file)
  data$Year <- extract_year(file)
  return(data)
}))

# misc cleaning
acs[1,552] <- "Year"

colnames(acs) <- acs[1,]
acs <- acs[!is.na(as.numeric(as.character(acs$`Estimate!!EMPLOYMENT STATUS!!Population 16 years and over`))), ]
acs <- acs[, !duplicated(colnames(acs))]
columns_to_keep <- (colnames(acs) == "Year") | 
  (grepl("^(Estimate!!|Percent!!)", colnames(acs)) & 
     !grepl("Margin of Error", colnames(acs))) | (colnames(acs) == "Geographic Area Name")
acs_selected <- acs[, columns_to_keep]


# clean up syntax
colnames(acs_selected) <- gsub("!![^!]*!!", " ", colnames(acs_selected))
colnames(acs_selected) <- gsub("\\s+", " ", colnames(acs_selected))
colnames(acs_selected) <- trimws(colnames(acs_selected))
acs <- acs_selected
acs$`Geographic Area Name` <- gsub("city,.*", "", acs$`Geographic Area Name`)
acs$`Geographic Area Name` <- gsub("CDP.*", "", acs$`Geographic Area Name`)
acs$`Geographic Area Name` <- gsub("town.*", "", acs$`Geographic Area Name`)

####################################################

### OPPORRTUNITY ZONE DATA #######################################
opp <- read.csv("Rawdata/OpportunityZone_Rawdata.csv")
opp <- opp[opp$STUSAB == "CA", ]

opp_sf <- st_read("Rawdata/OpportunityZonesShapefile/OpportunityZoneShapefile_Rawdata.shp")

# Load California places (city boundaries) as sf object
ca_places <- places(state = "CA", cb = TRUE, year = 2020) %>%
  select(NAME, GEOID) 

# Ensure both layers (opp_sf and ca_places) are in the same CRS
opp_sf <- st_transform(opp_sf, crs = st_crs(ca_places))

# nearest city to each opportunity zone
nearest_cities <- st_nearest_feature(opp_sf, ca_places)

# Add nearest city name and GEOID to the Opportunity Zones data
opp_with_towns <- opp_sf %>%
  mutate(
    Nearest_City = ca_places$NAME[nearest_cities],
    City_GEOID = ca_places$GEOID[nearest_cities]
  )

opp = st_drop_geometry(opp_with_towns)
opp = opp[opp$STUSAB == "CA",]

## Contains census tracts that are "opportunity zones" as of 2019, 
## don't have change over time
## will include as indicator variable

opportunity_cities = opp %>%
  select(Nearest_City) %>%
  distinct()

####################################################

### MERGE DATA #######################################

df <- NULL

# we're going to match everything to the towns in ACS data, its most robust
df$Town <- unique(acs$`Geographic Area Name`)
df$Town <- trimws(df$Town)

months <- seq(as.Date("2010-01-01"), as.Date("2023-06-01"), by = "month")
df <- expand.grid(Town = df$Town, Date = months)
df <- df %>%
  left_join(fed, by = "Date") %>%
  left_join(bls, by = c("Town" = "Area", "Date" = "Date"))

df <- df %>%
  mutate(Town = paste(Town, "CA", sep = ", "))

bls <- bls %>%
  mutate(Area = gsub(",.*", "", Area)) %>%
  mutate(Area = paste(Area, "CA", sep = ", "))

#still a lot of missing towns here, lets assign BLS values based on the nearest towns

df_geo <- df %>%
  distinct(Town) %>%
  tidygeocoder::geocode(address = Town, method = "osm", lat = latitude, long = longitude)

bls_geo <- bls %>%
  distinct(Area) %>%
  tidygeocoder::geocode(address = Area, method = "osm", lat = latitude, long = longitude)

distance_results <- expand.grid(df_geo$Town, bls_geo$Area) %>%
  rename(Town = Var1, Area = Var2) %>%
  left_join(df_geo, by = "Town") %>%
  left_join(bls_geo, by = "Area", suffix = c("_df", "_bls")) %>%
  mutate(distance = distHaversine(cbind(longitude_df, latitude_df), cbind(longitude_bls, latitude_bls)))

nearest_matches <- distance_results %>%
  group_by(Town) %>%
  slice_min(order_by = distance, n = 1) %>%
  ungroup() %>%
  select(Town, Area)

write.csv(nearest_matches, "Cleaneddata/nearest_matches_bls.csv", row.names = FALSE) # for later processing

df <- df %>%
  left_join(nearest_matches, by = "Town") %>%  # Join the nearest matched Area
  left_join(bls, by = c("Area" = "Area", "Date" = "Date")) # Bring in BLS data for the matched area

df <- df %>%
  select(Town, Date, Rate, `Civilian Labor Force.y`, Employment.y, Unemployment.y, `Unemployment Rate.y`) %>%
  rename(
    Fed_Rate = Rate,
    Labor_Force = `Civilian Labor Force.y`,
    Employment = Employment.y,
    Unemployment = Unemployment.y,
    Unemployment_Rate = `Unemployment Rate.y`
  ) %>%
  mutate(Town = gsub(",.*", "", Town))

z_home_value_unique <- z_home_value %>%
  group_by(City, Date) %>%
  summarize(HomeValue = mean(HomeValue, na.rm = TRUE), .groups = "drop")

write.csv(z_home_value_unique, "Cleaneddata/z_home_value_unique.csv")

df <- df %>%
  left_join(z_home_value_unique, by = c("Town" = "City", "Date" = "Date"))

# add in ACS data
# get rid of columns where every value is "(X)"
acs <- acs[, !apply(acs == "(X)", 2, all)]

# keep only columns with "Estimate"
acs <- acs[, c("Geographic Area Name", "Year",grep("Estimate", colnames(acs), value = TRUE))]
acs <- acs %>%
  mutate(`Geographic Area Name` = trimws(`Geographic Area Name`))

# get ready to merge
df <- df %>%
  mutate(Town = trimws(Town),
         Date_Year = as.character(format(Date, "%Y"))) %>%
  left_join(acs, by = c("Town" = "Geographic Area Name", "Date_Year" = "Year"))

df <- df[df$Town != "California", ]

# add in opportunity zone data
df <- df %>%
  mutate(Opportunity_Zone = ifelse(Town %in% opportunity_cities$Nearest_City, 1, 0))

df_geo <- df %>%
  distinct(Town) %>%
  mutate(Town = paste(Town, "California", sep = ", ")) %>%
  tidygeocoder::geocode(address = Town, method = "osm", lat = latitude, long = longitude)

# misc fix
ca[ca$Asset.Cities == "Mountainview", "Asset Cities"] <- "Mountain View"


# for deal data - find closest city in ACS data to join
ca_geo <- ca %>%
  distinct(Asset.Cities) %>%
  mutate(Asset.Cities = paste(Asset.Cities, "California", sep = ", ")) %>%
  tidygeocoder::geocode(address = Asset.Cities, method = "osm", lat = latitude, long = longitude)

# calc distances between each Town in df and each Asset City in ca
distance_results <- expand.grid(df_geo$Town, ca_geo$Asset.Cities) %>%
  rename(Town = Var1, Asset.Cities = Var2) %>%
  left_join(df_geo, by = "Town") %>%
  left_join(ca_geo, by = c("Asset.Cities" = "Asset.Cities"), suffix = c("_df", "_ca")) %>%
  mutate(distance = distHaversine(cbind(longitude_df, latitude_df), cbind(longitude_ca, latitude_ca)))

# filter by distance <= 100km 0 - get rid of remote deals
nearest_matches <- distance_results %>%
  group_by(Asset.Cities) %>%
  slice_min(order_by = distance, n = 1) %>%
  ungroup() %>%
  filter(distance <= 100000) %>%
  select(Town, Asset.Cities, distance)

nearest_matches <- nearest_matches %>%
  mutate(
    Town = gsub(", California", "", Town),
    Asset.Cities = gsub(", California", "", Asset.Cities)
  )

# again, we'll use this later
write.csv(nearest_matches, "Cleaneddata/nearest_matches_deal.csv")

# Add nearest matches to the ca dataset
ca <- ca %>%
  left_join(nearest_matches, by = "Asset.Cities")


# Extract the Year-Month from the Date column in both ca and df
df <- df %>%
  mutate(Year_Month = format(as.Date(Date), "%Y-%m"))


ca <- ca %>%
  mutate(Year_Month = format(as.Date(Deal.Date), "%Y-%m"))
ca <- ca %>%
  filter(Deal.Date >= "2010-01-01" & Deal.Date <= "2023-06-01")

# for simplicity later, we're going to save predictor data here
saveRDS(df, "Data_Predictor.rds")

# Step 6: Merge ca and df on Town and Year_Month
merged_data <- ca %>%
  left_join(df, by = c("Town" = "Town", "Year_Month" = "Year_Month"))


### CLEANING MERGED DATA FURTHER ####################################

df <- merged_data

df <- df %>%
  select(-c("Deal.Name", "Deal.Date", "Primary.Location", "Deal.Overview", 
            "Asset.Regions", "Asset.Countries", "Asset.States", "Date"))
df <- df %>%
  mutate(
    Town = gsub(", California", "", Town),
    Asset.Cities = gsub(", California", "", Asset.Cities),
    Year = as.numeric(gsub("-.*", "", Year_Month))
  )

# remove unnessary/redundant columns
df <- df %>%
  select(-c(
    "Estimate Population 16 years and over",
    "Estimate In labor force",
    "Estimate In labor force!!Civilian labor force",
    "Estimate In labor force Employed",
    "Estimate In labor force Unemployed",
    "Estimate In labor force!!Armed Forces",       
    "Estimate Not in labor force",
    "Estimate Civilian labor force",
    "Estimate Females 16 years and over",
    "Estimate All parents in family in labor force",
    "Estimate Workers 16 years and over",
    "Estimate Other means",
    "Estimate Civilian employed population 16 years and over",
    "Estimate Civilian employed population 16 years and over.1",
    "Estimate Civilian employed population 16 years and over.2",
    "Estimate Total households",
    "Estimate Less than $10,000",                   
    "Estimate $10,000 to $14,999",
    "Estimate $15,000 to $24,999",
    "Estimate $25,000 to $34,999",
    "Estimate $35,000 to $49,999",
    "Estimate $50,000 to $74,999",
    "Estimate $75,000 to $99,999",
    "Estimate $100,000 to $149,999",
    "Estimate $150,000 to $199,999",
    "Estimate $200,000 or more",
    "Estimate Mean household income (dollars)",
    "Estimate With earnings!!Mean earnings (dollars)", # Retained "!!" as it matches your colnames(df)
    "Estimate With Social Security!!Mean Social Security income (dollars)",
    "Estimate With retirement income!!Mean retirement income (dollars)",
    "Estimate With Supplemental Security Income!!Mean Supplemental Security Income (dollars)",
    "Estimate With cash public assistance income!!Mean cash public assistance income (dollars)",
    "Estimate Families",
    "Estimate Median family income (dollars)",
    "Estimate Mean family income (dollars)",
    "Estimate Per capita income (dollars)",
    "Estimate Nonfamily households",
    "Estimate Median nonfamily income (dollars)",
    "Estimate Mean nonfamily income (dollars)",
    "Estimate Median earnings for workers (dollars)",
    "Estimate Median earnings for male full-time, year-round workers (dollars)",
    "Estimate Median earnings for female full-time, year-round workers (dollars)",
    "Estimate Civilian noninstitutionalized population",
    "Estimate Civilian noninstitutionalized population under 18 years",
    "Estimate Civilian noninstitutionalized population 18 to 64 years",
    "Estimate In labor force.1",
    "Estimate In labor force!!Employed",
    "Estimate In labor force With health insurance coverage",
    "Estimate In labor force With health insurance coverage!!With private health insurance",
    "Estimate In labor force With health insurance coverage!!With public coverage",
    "Estimate In labor force No health insurance coverage",
    "Estimate In labor force!!Unemployed",
    "Estimate In labor force With health insurance coverage.1",
    "Estimate In labor force With health insurance coverage!!With private health insurance.1",
    "Estimate In labor force With health insurance coverage!!With public coverage.1",
    "Estimate In labor force No health insurance coverage.1",
    "Estimate Not in labor force.1",
    "Estimate Not in labor force!!With health insurance coverage",
    "Estimate Not in labor force With private health insurance",
    "Estimate Not in labor force With public coverage",
    "Estimate Not in labor force!!No health insurance coverage",
    "Estimate All families",
    "Estimate All families!!With related children under 18 years",
    "Estimate All families With related children under 5 years only",
    "Estimate Married couple families",
    "Estimate Married couple families!!With related children under 18 years",
    "Estimate Married couple families With related children under 5 years only",
    "Estimate Families with female householder, no husband present",
    "Estimate Families with female householder, no husband present!!With related children under 18 years",
    "Estimate Families with female householder, no husband present With related children under 5 years only",
    "Estimate Under 18 years!!Related children under 18 years",
    "Estimate Under 18 years Related children under 5 years",
    "Estimate Under 18 years Related children 5 to 17 years"
  ))


# for multiple asset portfolios, just keep the first building
df$Address <- str_extract(df$Assets, "^[^,]+")

df$Address = paste0(df$Address, ", ", df$Asset.Cities, ", California")
# let's get address level coordinates for creating some plots

# first, some paralell processing since this is a lot
# syntax aided by Chat-GPT

# Set up parallel processing
plan(multisession, workers = parallel::detectCores() - 1)

# Set your Google API key for tidygeocoder
# options(tidygeocoder.google_api_key = "AIzaSyB13Mx7EXxARNCoYEH2ICJ40WdQbfcl7QI")
# Sys.setenv(GOOGLEGEOCODE_API_KEY = "AIzaSyB13Mx7EXxARNCoYEH2ICJ40WdQbfcl7QI")

# Function to geocode a single address using Google via tidygeocoder
geocode_address <- function(location) {
  tryCatch({
    print(paste("Geocoding address:", location))
    result <- geo(address = location, method = 'google', mode = 'single')
    if (!is.null(result) && nrow(result) > 0) {
      return(data.frame(lon = result$long, lat = result$lat))
    } else {
      print(paste("Geocoding failed for:", location))
      return(data.frame(lon = NA_real_, lat = NA_real_))
    }
  }, error = function(e) {
    print(paste("Error geocoding address:", location))
    print(e)
    return(data.frame(lon = NA_real_, lat = NA_real_))
  })
}

# google earth doesn't work for city wide addresses
# Function to geocode a single city using OSM via tidygeocoder
geocode_city <- function(location) {
  tryCatch({
    print(paste("Geocoding city:", location))
    result <- geo(address = location, method = 'osm', mode = 'single')
    if (!is.null(result) && nrow(result) > 0) {
      return(data.frame(lon = result$long, lat = result$lat))
    } else {
      print(paste("City geocoding failed for:", location))
      return(data.frame(lon = NA_real_, lat = NA_real_))
    }
  }, error = function(e) {
    print(paste("Error geocoding city:", location))
    print(e)
    return(data.frame(lon = NA_real_, lat = NA_real_))
  })
}

# Prepare your data

# Step 1: Geocode addresses from df$Address_full with parallel processing using tidygeocoder
df <- df %>%
  mutate(
    geocode_results = future_map(Address, geocode_address, .progress = TRUE),
    longitude = map_dbl(geocode_results, "lon"),
    latitude = map_dbl(geocode_results, "lat")
  ) %>%
  select(-geocode_results)  # Remove geocode_results column if not needed

# Step 2: Geocode unique cities from df$Asset.Cities using OSM via tidygeocoder
unique_cities <- df %>%
  distinct(Asset.Cities) %>%
  rename(city = Asset.Cities) %>%
  mutate(city_full = paste0(city, ", California"))  # Adjust as needed

city_coords <- unique_cities %>%
  mutate(
    geocode_results = future_map(city_full, geocode_city, .progress = TRUE),
    long_center = map_dbl(geocode_results, "lon"),
    lat_center = map_dbl(geocode_results, "lat")
  ) %>%
  select(city, long_center, lat_center)  # Keep only necessary columns

# Join city coordinates back to the main dataframe
df <- df %>%
  left_join(city_coords, by = c("Asset.Cities" = "city"))

# Calculate haversine distance and add sanity check column
df <- df %>%
  mutate(
    distance = distHaversine(
      cbind(longitude, latitude),
      cbind(long_center, lat_center)
    ),
    sanity_check = case_when(
      is.na(longitude) | is.na(latitude) ~ "Geocoding Failed",
      is.na(long_center) | is.na(lat_center) ~ "City Geocoding Failed",
      distance <= 50000 ~ "Pass",
      TRUE ~ "Fail"
    )
  )

# Clean up parallel processing plan
plan(sequential)

## some errors based on incomplete addresses
## had to manually clean

# load corrected data
e_fixed <- read_csv("Rawdata/geocoding_failures_fixed.csv") %>%
  select(-c("...5"))  # Remove the unnecessary column

# Set specified columns to NA for rows where sanity_check == "Fail"
cols_to_update <- c("longitude", "latitude", "long_center", "lat_center", "distance")
df <- df %>%
  mutate(across(all_of(cols_to_update), ~ ifelse(sanity_check == "Fail", NA, .)))

# Replace NA values in the specified columns with corrected data from e_fixed
df <- df %>%
  left_join(e_fixed, by = "Address", suffix = c("", ".fixed")) %>%
  mutate(
    longitude = coalesce(longitude.fixed, longitude),
    latitude = coalesce(latitude.fixed, latitude),
    long_center = coalesce(long_center.fixed, long_center),
    lat_center = coalesce(lat_center.fixed, lat_center),
    distance = coalesce(distance.fixed, distance)
  ) %>%
  select(-ends_with(".fixed"))

df <- df %>%
  mutate(sanity_check = ifelse(sanity_check == "Fail", "Pass", sanity_check))

saveRDS(df, "Data.rds")
