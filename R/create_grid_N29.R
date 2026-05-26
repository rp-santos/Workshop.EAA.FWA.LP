#' Create a spatial grid in UTM zone 29N
#'
#' @description
#'
#' [create_grid_N29()] generates a spatial raster grid covering a
#' specified study area using UTM zone 29N coordinates
#' (EPSG:32629). The grid is first created in projected UTM
#' coordinates using [create_utm_grid_N29()], then transformed to
#' geographic coordinates (WGS84).
#'
#' The resulting object is returned as a raster layer that can be
#' used for spatial analyses, detector allocation, habitat masks,
#' and effort standardization in capture-recapture workflows.
#'
#' @param x_range Numeric vector of length 2 defining the minimum
#'   and maximum x coordinates (UTM eastings) of the study area.
#'
#' @param y_range Numeric vector of length 2 defining the minimum
#'   and maximum y coordinates (UTM northings) of the study area.
#'
#' @param utm_resolution Numeric value indicating the grid cell
#'   resolution in meters. Defaults to `5000`.
#'
#' @param x_pad_perc Numeric value indicating the percentage of
#'   padding added to the x-axis extent. Defaults to `0.05`.
#'
#' @param y_pad_perc Numeric value indicating the percentage of
#'   padding added to the y-axis extent. Defaults to `0.05`.
#'
#' @param x_offset Numeric value defining an offset applied to the
#'   x coordinates of the grid. Defaults to `0`.
#'
#' @param y_offset Numeric value defining an offset applied to the
#'   y coordinates of the grid. Defaults to `0`.
#'
#' @returns
#' A `RasterLayer` object in WGS84 geographic coordinates
#' representing the spatial grid.
#'
#' Grid dimensions (`ncol` and `nrow`) are derived from the output
#' of [create_utm_grid_N29()].
#'
#' @details
#'
#' The function performs the following steps:
#'
#' 1. Creates a regular grid in UTM zone 29N using
#'    [create_utm_grid_N29()]
#'
#' 2. Converts the grid to an `sf` object
#'
#' 3. Transforms coordinates from UTM zone 29N to WGS84
#'
#' 4. Converts the transformed grid into a `RasterLayer`
#'
#' The final raster is suitable for spatial matching with
#' cetacean sightings, detector placement, and effort allocation.
#'
#' @examples
#' \dontrun{
#' grid <- create_grid_N29(
#'   x_range = c(200000, 300000),
#'   y_range = c(4100000, 4200000)
#' )
#'
#' plot(grid)
#' }
#'
#' @export
create_grid_N29 <-
  function(x_range,
           y_range,
           utm_resolution = 5000,
           x_pad_perc = 0.05,
           y_pad_perc = 0.05,
           x_offset = 0,
           y_offset = 0) {

    utm_grid <-
      create_utm_grid_N29(
        x_range = x_range,
        y_range = y_range,
        utm_resolution = utm_resolution,
        x_pad_perc = x_pad_perc,
        y_pad_perc = y_pad_perc,
        x_offset = x_offset,
        y_offset = y_offset
      )

    # Define the UTM zone for your data
    utm_zone <- "+proj=utm +zone=29 +datum=WGS84 +units=m +no_defs"

    # Create an sf object from the data frame using the UTM zone
    utm_sf <- sf::st_as_sf(utm_grid, coords = c("x", "y"), crs = utm_zone)

    # Transform the sf object to the WGS84 projection
    wgs84_sf <- sf::st_transform(utm_sf, "+proj=longlat +datum=WGS84 +no_defs")

    # Transform the sf object to the WGS84 projection and convert to RasterLayer
    raster_layer <- raster::raster(wgs84_sf, ncol = attr(utm_grid, "nx"), nrow = attr(utm_grid, "ny"))

    raster_layer
  }
