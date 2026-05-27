#This script shows a simplified workflow used to estimate fin whales size population using Jolly-Seber-Schwarz-Arnason capture-recapture model

#Main workflow:
#1. Load data
#2. Create spatial grid
#3. Build capture-recapture histories
#4. Create habitat mask
#5. Fit JSSA model

# install.packages("pak")
pak::pak("patterninstitute/azores.fkw")

library(tidyverse)
library(patchwork)
library(sf)
library(secr)
library(openCR)
library(ggspatial)
library(azores.fkw)


#1. === LOAD DATA ===
# Files description
# bp.indidviduals: Contains photo-identification detections of fin whales (all unique individuals
# except the ones without valid geographic position)
# boat_trips_by_minute: Contains boat trips information aggregated by minute to reduce file size,
# but preserving the overall survey coverage
# openCR.analysis: Folder used to store files generated during the openCR analysis (capture.txt
# and effort.txt)
# my-functions_RM.R: Custom functions developed during my MSc thesis (created by Ramiro Magno)

# Choose the path were you saved the files in your computer
path <- "/Users/ruisantos/Desktop/Abundance_workshop/Workshop.EAA.FWA.LP/data-raw"
data_path <- "/Users/ruisantos/Desktop/Abundance_workshop/Workshop.EAA.FWA.LP/data"
bp_path <- here::here(path, "bp.individuals.xlsx")
boat_path <- here::here(path, "boat_trips_by_min.xlsx")
write_path <- here::here(data_path, "openCR.analysis")
source(file.path(path, "my-functions_RM.R"))
bp.individuals <- readxl::read_xlsx(bp_path)
bp.individuals <- bp.individuals |>
  dplyr::mutate(
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude)
  )
boat_trips_by_min <- readxl::read_xlsx(boat_path)

set.seed(1)
utm_resolution <- 1000
buffer <- 10000
trace <- TRUE


#2. === CREATE SPATIAL GRID ===
#Grid cells covering the study area are used as spatial detectors in the model

grid <- create_grid_N29(
  x_range = range(boat_trips_by_min$longitude),
  y_range = range(boat_trips_by_min$latitude),
  utm_resolution = utm_resolution
)

bp.individuals <- bp.individuals |>
  dplyr::filter(format(date, "%Y") != "2020") |>
  dplyr::filter(format(date, "%Y") != "2021")
length(unique(bp.individuals$individual))

#We have eliminated the individuals identified in years 2020 and 2021 due to lack of GPS tracks

bp.individuals$sighting_id <- as.integer(bp.individuals$sighting_id)
bp.individuals$date <- as.Date(bp.individuals$date)


#3. === BUILD CAPTURE-RECAPTURE HISTORIES ===
#Each fin whale detection is associated with a spataial detector/grid cell
#Cells were classified as ‘active’ if visited by an individual during a survey

individuals2 <-
  bp.individuals |>
  azores.fkw::add_sessions(
    psession_fn = \(x) azores.fkw::year(dat = x, dttm_var = "initial_time"),
    ssession_fn = \(x) azores.fkw::week(dat = x, dttm_var = "initial_time")
  )

capture_data <- dplyr::bind_cols(individuals2, azores.fkw::find_grid_cells(individuals2, grid= grid)["index"])
capture <- azores.fkw::create_capture(capture_data = capture_data) |>
  dplyr::filter(!is.na(Detector))

#BOAT EFFORT
#This step calculates survey effort for each grid cell and temporal sampling occasion
#Cells were classified as ‘active’ if visited by the boat during a survey

boat_effort_per_week <- boat_effort_per_week_N29(
  boat_trips = boat_trips_by_min,
  grid = grid
)
effort <- boat_effort_per_week

#CAPTHIST
#It describes which individual was detected, where, when and under which survey effort

caphist <- secr::read.capthist(
  captfile = azores.fkw::write_capture(capture,file = file.path(write_path, "capture.txt")
  ),
  trapfile = azores.fkw::write_effort(effort,file = file.path(write_path, "effort.txt")
  ),
  binary.usage = FALSE,
  detector = "multi"
)


#4. === CREATE HABITAT MASK ===
# It defines the area in which the animals are assumed to potentially occur and move
# Grid of spatial points that represent possible activity center locations for individuals
# in the population

Portugal <- sf::st_read(
  file.path(path, "maps:portugal/Portugal.shp")
)
Algarve <- Portugal |>
  dplyr::filter(NAME_1 == "Faro")
exclude_regions <- sf::st_transform(Algarve,crs = 32629)
habitat_mask <- create_mask_N29(
  x_range = range(boat_trips_by_min$longitude),
  y_range = range(boat_trips_by_min$latitude),
  utm_resolution = utm_resolution,
  buffer = buffer,,
  exclude_regions = exclude_regions
)
capture_data_sf <- capture_data |>
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  sf::st_transform(crs = 32629)
effort_sf <- effort |>
  dplyr::select(c("index", "easting", "northing")) |>
  sf::st_as_sf(coords = c("easting", "northing"), crs = 32629)
ggplot2::ggplot() +
  ggplot2::geom_sf(data = exclude_regions, fill = NA, color = "black") +
  ggplot2::geom_point(data = habitat_mask, ggplot2::aes(x = x, y = y), color = "blue", size = 0.5) +
  ggplot2::theme_minimal() +
  ggplot2::labs(title = "Mask + Faro District") +
  ggplot2::geom_sf(data = capture_data_sf) +
  ggplot2::geom_sf(data = effort_sf)
# === Range in meters along easting (`x`) ===
x_length <- diff(range(habitat_mask$x)) + utm_resolution
# === Range in meters along northing (`y`) ===
y_length <- diff(range(habitat_mask$y)) + utm_resolution
# === Full area (including area of islands) in hectares ===
full_area_ha <- x_length * y_length / utm_resolution
# === Study area (excludes area of islands) in hectares ===
study_area_ha <- (nrow(habitat_mask) * utm_resolution^2) / 10000


#5. === FIT JSSA MODELS ===
# After comparing the three model (JSAAsecrb, JSAAsecrf, JSAAsecrl) using AIC, the best model is JSAAsecrb

JSSAsecrb <- openCR::openCR.fit(
  caphist,
  type = 'JSSAsecrb',
  mask = habitat_mask,
  trace = trace
)
# === Estimate density ===
density_estimates_JSSAsecrb <- predict(JSSAsecrb, type = 'D')
(superD_JSSAsecrb <- density_estimates_JSSAsecrb$superD * study_area_ha)

# === Save results part of the results===
opencr_results <- list(
  superpopulation = opencr_results$superpopulation,
  density_estimates = opencr_results$density_estimates
)

#6. Save one the same path on your computer
save(
  opencr_results,
  file = "/Users/ruisantos/Desktop/Abundance_workshop/Workshop.EAA.FWA.LP/man/opencr_results_Bp_MB_2026_173_94.RData"
)
