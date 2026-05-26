#' Compute distances between a whale location and mask points
#'
#' @description
#'
#' [whale_distances()] calculates the Euclidean distance between a
#' focal whale location (identified through a capture history) and all
#' spatial points in a mask/grid.
#'
#' This function is typically used in spatial capture-recapture (SECR)
#' workflows to compute detection distances between an individual
#' location and detector grid cells.
#'
#' @param i Integer index identifying the focal row in the capture
#'   history.
#'
#' @param mask A matrix or object containing spatial coordinates of
#'   detector locations. The first two columns must correspond to
#'   x (easting) and y (northing) coordinates.
#'
#' @param grid_tbl A data frame or tibble containing detector
#'   information, including:
#'   \describe{
#'     \item{easting}{X coordinate of detector locations}
#'     \item{northing}{Y coordinate of detector locations}
#'   }
#'
#' @param capture A data frame containing capture information with at
#'   least a column named `Detector` indicating the assigned detector
#'   index for each observation.
#'
#' @returns
#' A numeric vector of Euclidean distances between the focal whale
#' location and all points in `mask`.
#'
#' @details
#'
#' The function:
#'
#' 1. Extracts the detector index associated with observation `i`
#' 2. Retrieves the corresponding easting and northing coordinates
#' 3. Computes Euclidean distances to all mask/grid points
#' 4. Returns a numeric vector of distances
#'
#' @examples
#' \dontrun{
#'
#' d <- whale_distances(
#'   i = 1,
#'   mask = mask_matrix,
#'   grid_tbl = grid,
#'   capture = capture_history
#' )
#'
#' head(d)
#'
#' }
#'
#' @export
whale_distances <- function(i, mask, grid_tbl, capture) {

  detector_index <- capture[i, "Detector", drop = TRUE]

  # x-coordinate of whale i
  x_i <- grid_tbl[detector_index, "easting", drop = TRUE]
  # y-coordinate of whale i
  y_i <- grid_tbl[detector_index, "northing", drop = TRUE]

  dx <- x_i - mask[, 1]
  dy <- y_i - mask[, 2]

  # Distance between point (x_i, y_i) and grid cell points
  d <- sqrt(dx^2 + dy^2)

  d

}
