#' Create a regular UTM grid in zone 29N
#'
#' @description
#'
#' [create_utm_grid_N29()] generates a regular spatial grid in
#' UTM zone 29N (EPSG:32629) based on longitude and latitude ranges.
#'
#' The function converts geographic coordinates (WGS84) into UTM
#' coordinates and creates a regularly spaced grid using
#' [secr::make.grid()].
#'
#' Optional padding can be applied to expand the study area extent,
#' and offsets can be used to shift grid cell locations relative to
#' detector positions or habitat masks.
#'
#' @param x_range Numeric vector of length 2 defining the minimum
#'   and maximum longitude values of the study area.
#'
#' @param y_range Numeric vector of length 2 defining the minimum
#'   and maximum latitude values of the study area.
#'
#' @param utm_resolution Numeric value indicating the spacing between
#'   grid points in meters. Defaults to `1000`.
#'
#' @param x_pad_perc Numeric value indicating the percentage of
#'   padding added to the longitude range. Defaults to `0.05`.
#'
#' @param y_pad_perc Numeric value indicating the percentage of
#'   padding added to the latitude range. Defaults to `0.05`.
#'
#' @param x_offset Numeric value defining an offset applied to the
#'   x coordinates (eastings) of the grid. Defaults to
#'   `-utm_resolution/2`.
#'
#' @param y_offset Numeric value defining an offset applied to the
#'   y coordinates (northings) of the grid. Defaults to
#'   `-utm_resolution/2`.
#'
#' @returns
#'
#' A data frame containing UTM grid coordinates with columns:
#'
#' \describe{
#'   \item{x}{UTM easting coordinate}
#'   \item{y}{UTM northing coordinate}
#' }
#'
#' The object also contains attributes:
#'
#' \describe{
#'   \item{nx}{Number of grid cells along the x-axis}
#'   \item{ny}{Number of grid cells along the y-axis}
#' }
#'
#' @details
#'
#' The function performs the following steps:
#'
#' 1. Expands the geographic extent using padding percentages
#'
#' 2. Converts longitude/latitude coordinates to UTM zone 29N using
#'    [azores.fkw::longlat_to_utm()]
#'
#' 3. Computes the number of grid cells required along each axis
#'
#' 4. Generates a regular UTM grid using [secr::make.grid()]
#'
#' 5. Applies optional coordinate offsets to shift grid locations
#'
#' The resulting grid can be used to create detector arrays,
#' habitat masks, or spatial rasters for SECR analyses.
#'
#' @examples
#' \dontrun{
#'
#' utm_grid <- create_utm_grid_N29(
#'   x_range = c(-9.5, -8.5),
#'   y_range = c(36.5, 37.5)
#' )
#'
#' head(utm_grid)
#'
#' }
#'
#' @export
create_utm_grid_N29 <-
  function(x_range,
           y_range,
           utm_resolution = 1000,
           x_pad_perc = 0.05,
           y_pad_perc = 0.05,
           x_offset = -utm_resolution/2,
           y_offset = -utm_resolution/2) {

    # Padding
    x_padding <- x_pad_perc * diff(x_range)
    y_padding <- y_pad_perc * diff(y_range)

    x_range2 <- c(x_range[1] - x_padding, x_range[2] + x_padding)
    y_range2 <- c(y_range[1] - y_padding, y_range[2] + y_padding)

    longlat_ranges <-
      data.frame(lon = x_range2,
                 lat = y_range2)

    utm_ranges <- azores.fkw::longlat_to_utm(longlat_ranges, crs_to = 32629)

    nx <-
      round((utm_ranges$easting[2] - utm_ranges$easting[1]) / utm_resolution) + 1
    ny <-
      round((utm_ranges$northing[2] - utm_ranges$northing[1]) / utm_resolution) + 1

    utm_grid <-
      secr::make.grid(
        nx = nx,
        ny = ny,
        spacing = utm_resolution,
        originxy = c(utm_ranges$easting[1], utm_ranges$northing[1])
      )

    attr(utm_grid, "nx") <- nx
    attr(utm_grid, "ny") <- ny

    utm_grid$x <- utm_grid$x + x_offset
    utm_grid$y <- utm_grid$y + y_offset

    utm_grid
  }
