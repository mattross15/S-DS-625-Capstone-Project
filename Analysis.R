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


###### EXPLORING DATA ##################################################

## NEED TO ADD VISUALIZATIONS HERE ##



## Los Angeles Leaflet Plot ###

data <- read
# Filter data for Los Angeles
# Filter data for Los Angeles and arrange by time
la_data <- df %>%
  filter(Town == "Los Angeles") %>%
  arrange(Year_Month) %>%
  mutate(Year = as.numeric(substr(Year_Month, 1, 4)))  # Extract year from Year_Month

# Define a color palette based on the Year
year_colors <- colorFactor(palette = "viridis", domain = la_data$Year)

# Directory to save individual map snapshots
output_dir <- "leaflet_frames"
dir.create(output_dir, showWarnings = FALSE)

# Generate Leaflet maps for each Year_Month and save snapshots
unique_dates <- unique(la_data$Year_Month)
for (i in seq_along(unique_dates)) {
  current_date <- unique_dates[i]
  current_data <- la_data %>% filter(Year_Month <= current_date)
  
  # Create Leaflet map
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
        "<b>Deal Size (USD Mn):</b> ", Deal.Size.Usd.Mn., "<br>",
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

# Combine PNGs into a GIF
png_files <- list.files(output_dir, pattern = "*.png", full.names = TRUE)
gif <- image_read(png_files)
gif <- image_animate(gif, fps = 5)  # Adjust FPS for animation speed
image_write(gif, "los_angeles_deals.gif")
