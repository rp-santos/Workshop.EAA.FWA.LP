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
