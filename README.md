<!-- README.md is generated from README.Rmd. Please edit that file -->

# Workshop.EAA.FWA.LP

<!-- badges: start -->

<!-- badges: end -->

The goal of `{Workshop.EAA.FWA.LP}` is to provide an R Compendium for
48 hours of Estimating Animal Abundance @ FCUL by CEAUL - 11th and 12th of June 2026

## Installation

``` r
# install.packages("pak")
pak::pak("rp-santos/Workshop.EAA.FWA.LP")
```

## Bp Sightings

A data set of sightings of Fin whales in the Faro area, Algarve, Portugal, during the
period of 2022 and 2024:

``` r
library(tidyverse)
library(patchwork)
library(sf)
library(secr)
library(openCR)
library(ggspatial)
library(azores.fkw)
