#' Convert longitude/latitude coordinates to UTM
#'
#' @description
#'
#' [longlat_to_utm_alg()] converts geographic coordinates (longitude
#' and latitude) into projected UTM coordinates using the
#' `sf` package.
#'
#' This function is designed for spatial analyses in the Algarve /
#' eastern Atlantic region, using UTM zone 29N (EPSG:32629) by default.
#'
#' @param longlat A data frame or tibble containing geographic
#'   coordinates.
#'
#'   Must include the columns:
#'
#'   \describe{
#'     \item{lon}{Longitude in decimal degrees (WGS84)}
#'     \item{lat}{Latitude in decimal degrees (WGS84)}
#'   }
#'
#' @param crs_from Numeric EPSG code of the input coordinate reference
#'   system. Defaults to `4326` (WGS84).
#'
#' @param crs_to Numeric EPSG code of the output coordinate reference
#'   system. Defaults to `32629` (UTM zone 29N).
#'
#' @returns
#'
#' A tibble with UTM coordinates:
#'
#' \describe{
#'   \item{easting}{Projected x-coordinate in meters}
#'   \item{northing}{Projected y-coordinate in meters}
#' }
#'
#' @details
#'
#' The function performs the following steps:
#'
#' 1. Checks that required columns (`lon`, `lat`) are present
#'
#' 2. Converts the input data frame into an `sf` POINT object
#'    using the specified input CRS
#'
#' 3. Transforms coordinates into UTM zone 29N (or user-defined CRS)
#'
#' 4. Extracts projected coordinates and returns them as a tibble
#'
#' @examples
#' \dontrun{
#'
#' coords <- data.frame(
#'   lon = c(-8.9, -8.7),
#'   lat = c(37.0, 37.2)
#' )
#'
#' utm <- longlat_to_utm_alg(coords)
#'
#' head(utm)
#'
#' }
#'
#' @export
longlat_to_utm_alg <- function(longlat, crs_from = 4326, crs_to = 32629) {
  if (!rlang::has_name(longlat, "lon"))
    stop("`longlat` must have a column named `lon`")

  if (!rlang::has_name(longlat, "lat"))
    stop("`longlat` must have a column named `lat`")

  # Create an sf POINT object with longitude/latitude coordinates
  lonlat_points <-
    sf::st_as_sf(longlat, coords = c("lon", "lat"), crs = crs_from)  # EPSG:4326 for WGS84

  # Transform the points to UTM projection (Algarve region - UTM Zone 29N)
  utm_points <- sf::st_transform(lonlat_points, crs = crs_to)

  # Convert the UTM points back to a tibble
  utm_coords <- sf::st_coordinates(utm_points) |>
    tibble::as_tibble() |>
    dplyr::rename(easting = "X", northing = "Y")

  utm_coords

}
