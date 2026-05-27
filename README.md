<!-- README.md is generated from README.Rmd. Please edit that file -->

# Workshop.EAA.FWA.LP

<!-- badges: start -->

<!-- badges: end -->

The goal of `{Workshop.EAA.FWA.LP}` is to provide an R Compendium for
48 hours of Estimating Animal Abundance @ FCUL by CEAUL - 11th and 12th of June 2026

## Installation

``` r
# install.packages("remotes")
remotes::install_github("rp-santos/Workshop.EAA.FWA.LP")
```

## Algarve region

A sf object exclusive of the Algarve region:

``` r
data("exclude_regions")

ggplot2::ggplot() +
  ggplot2::geom_sf(data = exclude_regions, fill = NA, color = "black") +
  ggplot2::theme_minimal()
```



## Bp capture data

A data set of sightings of Fin whales in the Faro area, Algarve, Portugal, during the
period of 2022 and 2024:

``` r
data("capture_data_sf")

ggplot2::ggplot() +
  ggplot2::geom_sf(data = capture_data_sf) +
  ggplot2::geom_sf(data = exclude_regions, fill = NA, color = "black") +
  ggplot2::theme_minimal()
```

## Effort data

A data set of boat effort from 2022 to 2024 in the Faro area, Algarve, Portugal

``` r
data("effort_sf")

ggplot2::ggplot() +
  ggplot2::geom_sf(data = effort_sf) +
  ggplot2::geom_sf(data = exclude_regions, fill = NA, color = "black") +
  ggplot2::theme_minimal()
```

## Habitat mask

Area that whales can be moving that should represent 4 times model sigma values

``` r
data("habitat_mask")

ggplot2::ggplot() +
  ggplot2::geom_point(data = habitat_mask, ggplot2::aes(x = x, y = y), color = "blue", size = 0.5) +
  ggplot2::geom_sf(data = exclude_regions, fill = NA, color = "black") +
  ggplot2::theme_minimal()
```

## AFWPSE (Algarve Fin Whale Population Size Estimation) 

Area that whales can be moving that should represent 4 times model sigma values

``` r
data("opencr_results_summarised")
opencr_results_summarised
```
