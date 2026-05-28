#This script shows a simplified workflow used to estimate fin whales size population using Jolly-Seber-Schwarz-Arnason capture-recapture model

library(Workshop.EAA.FWA.LP)

#1. === LOAD DATA ===

#1.1. Include only the Algarve region
data("exclude_regions")

ggplot2::ggplot() +
  ggplot2::geom_sf(data = exclude_regions, fill = NA, color = "black") +
  ggplot2::theme_minimal()

#1.2. BP INDIVIDUALS as capture data
# Contains photo-identification detections of fin whales (all unique individuals
# except the ones without valid geographic position)
data("capture_data_sf")

ggplot2::ggplot() +
  ggplot2::geom_sf(data = capture_data_sf) +
  ggplot2::geom_sf(data = exclude_regions, fill = NA, color = "black") +
  ggplot2::theme_minimal()

#1.3.BOAT EFFORT
# This step calculates survey effort for each grid cell and temporal sampling occasion
# Cells were classified as ‘active’ if visited by the boat during a survey
data("effort_sf")

ggplot2::ggplot() +
  ggplot2::geom_sf(data = effort_sf) +
  ggplot2::geom_sf(data = exclude_regions, fill = NA, color = "black") +
  ggplot2::theme_minimal()

#1.4. HABITAT MASK ===
# It defines the area in which the animals are assumed to potentially occur and move
data("habitat_mask")

ggplot2::ggplot() +
  ggplot2::geom_point(data = habitat_mask, ggplot2::aes(x = x, y = y), color = "blue", size = 0.5) +
  ggplot2::geom_sf(data = exclude_regions, fill = NA, color = "black") +
  ggplot2::theme_minimal()

#1.5.CAPTHIST
#It describes which individual was detected, where, when and under which survey effort
data("caphist")


#1.6. === JSSA MODEL Result===
# After comparing the three model (JSAAsecrb, JSAAsecrf, JSAAsecrl) using AIC, the best model is JSAAsecrb
data("opencr_results_summarised")
opencr_results_summarised

#2. === JSSA MODEL Model for run ===
set.seed(1)
utm_resolution <- 1000
buffer <- 10000
trace <- TRUE

JSSAsecrb <- openCR::openCR.fit(
  caphist,
  type = 'JSSAsecrb',
  mask = habitat_mask,
  trace = trace
)

# === Range in meters along easting (`x`) ===
x_length <- diff(range(habitat_mask$x)) + utm_resolution
# === Range in meters along northing (`y`) ===
y_length <- diff(range(habitat_mask$y)) + utm_resolution
# === Full area (including area of islands) in hectares ===
full_area_ha <- x_length * y_length / utm_resolution
# === Study area (excludes area of islands) in hectares ===
study_area_ha <- (nrow(habitat_mask) * utm_resolution^2) / 10000
# === Estimate density ===
density_estimates_JSSAsecrb <- predict(JSSAsecrb, type = 'D')
(superD_JSSAsecrb <- density_estimates_JSSAsecrb$superD * study_area_ha)

# === Save results part of the results===
opencr_results <- list(
  model = JSSAsecrb,
  density_estimates = density_estimates_JSSAsecrb,
  superpopulation = superD_JSSAsecrb
)

opencr_results
