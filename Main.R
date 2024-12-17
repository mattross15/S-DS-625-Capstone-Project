# libraries
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
library(ggplot2)
library(ggrepel)
library(car)
library(sf)
library(ggmap)
library(tmaptools)
library(gridExtra)
library(lattice)
library(tigris)
library(tidycensus)
library(pbapply)
library(furrr)
library(usethis)
if (!requireNamespace("INLA", quietly = TRUE)) {
  stop("INLA package not installed. See instructions below to install the package and run the script.")
} else {
  library(INLA)
}
library(ggrepel)
###### to install INLA, uncomment and run the following code:
# opt <- options()
# options(pkgType="both")
# install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
# options(opt)

###### set Google Maps API Key, uncomment and run:
# options(tidygeocoder.google_api_key = "AIzaSyB13Mx7EXxARNCoYEH2ICJ40WdQbfcl7QI")
# Sys.setenv(GOOGLEGEOCODE_API_KEY = "AIzaSyB13Mx7EXxARNCoYEH2ICJ40WdQbfcl7QI")


source("PrepData.R")
source("Analysis.R")


