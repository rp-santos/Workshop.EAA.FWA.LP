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

data("bp.individuals")
data("boat_trips_by_min")
data("caphist")
