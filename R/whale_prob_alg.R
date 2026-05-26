#' Compute spatial probability surface for whale detections
#'
#' @description
#'
#' [whale_prob_alg()] estimates a spatial probability surface for whale
#' detections by combining a detection function with distances between
#' observed locations and a spatial mask/grid.
#'
#' The function accumulates detection probabilities across all observed
#' individuals and normalises them to produce a relative spatial
#' probability surface.
#'
#' @param mask A matrix or data frame of spatial locations (e.g. habitat
#'   mask or grid points). The first two columns must correspond to
#'   easting and northing coordinates.
#'
#' @param grid A raster object representing the spatial grid used for
#'   extracting detector coordinates.
#'
#' @param capture A data frame containing capture history information.
#'   Must include a column named `Detector` indicating the assigned grid
#'   cell index for each observation.
#'
#' @param params A named list containing detection parameters:
#'   \describe{
#'     \item{sigma}{Spatial scale parameter of the detection function}
#'     \item{lambda0}{Baseline detection parameter}
#'   }
#'
#' @param detectfn A detection function used to convert distances into
#'   probabilities. Defaults to [hex()].
#'
#' @returns
#'
#' A tibble containing the original mask coordinates and a column
#' `p`, representing the normalised spatial probability of detection.
#'
#' @details
#'
#' The function:
#'
#' 1. Extracts detector coordinates from the raster grid
#' 2. Converts them to UTM coordinates using [longlat_to_utm_alg()]
#' 3. Computes distances between each capture and all mask points using
#'    [whale_distances()]
#' 4. Applies the detection function to convert distances into
#'    probabilities
#' 5. Aggregates probabilities across all captures
#' 6. Normalises the resulting surface to sum to 1
#'
#' @examples
#' \dontrun{
#'
#' surf <- whale_prob_alg(
#'   mask = mask,
#'   grid = grid,
#'   capture = capture,
#'   params = list(sigma = 3000, lambda0 = 0.2)
#' )
#'
#' head(surf)
#'
#' }
#'
#' @export
whale_prob_alg <- function(mask, grid, capture, params, detectfn = hex) {

  p <- vector(mode = "double", nrow(mask))
  grid_tbl <-
    raster::coordinates(grid) |>
    tibble::as_tibble() |>
    dplyr::rename(lon = .data$x, lat = .data$y) |>
    longlat_to_utm_alg()


  for (i in seq_len(nrow(capture))) {

    d <- whale_distances(i, mask = mask, grid_tbl = grid_tbl, capture = capture)
    p <- p + detectfn(d, sigma = params$sigma, lambda0 = params$lambda0)
  }

  dplyr::bind_cols(mask, p = p / sum(p)) |>
    tibble::as_tibble()
}
