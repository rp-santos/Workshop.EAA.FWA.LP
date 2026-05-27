#This script shows a simplified workflow used to estimate fin whales size population using Jolly-Seber-Schwarz-Arnason capture-recapture model

#Main workflow:
#1. Load data
#2. Create spatial grid
#3. Build capture-recapture histories
#4. Create habitat mask
#5. Fit JSSA model


remotes::install_github("rp-santos/Workshop.EAA.FWA.LP")
remotes::install_github("patterninstitute/azores.fkw")

library(tidyverse)
library(patchwork)
library(sf)
library(secr)
library(openCR)
library(ggspatial)
library(azores.fkw)
library(Workshop.EAA.FWA.LP)

#1. === LOAD DATA ===

#1.1. BP INDIVIDUALS
# Contains photo-identification detections of fin whales (all unique individuals
# except the ones without valid geographic position)
# Select only the Algarve region from Portugal to plot the data
data("bp.individuals")
data("exclude_regions")
ggplot2::ggplot() +
  ggplot2::geom_point(data = bp.individuals, ggplot2::aes(x = latitude, y = longitude), color = "blue", size = 0.5) +
  ggplot2::theme_minimal()

#1.2.BOAT EFFORT
#This step calculates survey effort for each grid cell and temporal sampling occasion
#Cells were classified as ‘active’ if visited by the boat during a survey
data("boat_trips_by_min")

#1.3.CAPTHIST
#It describes which individual was detected, where, when and under which survey effort
data("caphist")


#1.4. HABITAT MASK, CAPTURE DATA SF and EFFORT SF ===
# It defines the area in which the animals are assumed to potentially occur and move
data("habitat_mask")
data("capture_data_sf")
data("effort_sf")

ggplot2::ggplot() +
  ggplot2::geom_sf(data = exclude_regions, fill = NA, color = "black") +
  ggplot2::geom_point(data = habitat_mask, ggplot2::aes(x = x, y = y), color = "blue", size = 0.5) +
  ggplot2::theme_minimal() +
  ggplot2::labs(title = "Mask + Faro District") +
  ggplot2::geom_sf(data = capture_data_sf) +
  ggplot2::geom_sf(data = effort_sf)

#2. === JSSA MODEL Result===
# After comparing the three model (JSAAsecrb, JSAAsecrf, JSAAsecrl) using AIC, the best model is JSAAsecrb
data("opencr_results_summarised")
opencr_results_summarised



