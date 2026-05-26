#' Create a habitat mask in UTM zone 29N
#'
#' @description
#'
#' [create_mask_N29()] generates a spatial habitat mask for
#' capture-recapture analyses using UTM zone 29N coordinates
#' (EPSG:32629).
#'
#' The mask is created from a regular UTM grid generated with
#' [create_utm_grid_N29()] and converted into a habitat mask using
#' [secr::make.mask()]. The resulting object can be used in spatially
#' explicit capture-recapture (SECR) and open population analyses.
#'
#' Optional exclusion polygons can be provided to remove unsuitable
#' habitat areas (e.g. land masses).
#'
#' @param x_range Numeric vector of length 2 defining the minimum
#'   and maximum x coordinates (UTM eastings) of the study area.
#'
#' @param y_range Numeric vector of length 2 defining the minimum
#'   and maximum y coordinates (UTM northings) of the study area.
#'
#' @param utm_resolution Numeric value indicating the spacing between
#'   mask points in meters. Defaults to `1000`.
#'
#' @param x_pad_perc Numeric value indicating the percentage of
#'   padding added to the x-axis extent. Defaults to `0.05`.
#'
#' @param y_pad_perc Numeric value indicating the percentage of
#'   padding added to the y-axis extent. Defaults to `0.05`.
#'
#' @param buffer Numeric value indicating the buffer distance (in
#'   meters) added around the trapping grid. Defaults to `12000`.
#'
#' @param exclude_regions Optional spatial polygon object defining
#'   areas to exclude from the habitat mask (e.g. land areas).
#'   Defaults to `NULL`.
#'
#' @returns
#'
#' An object of class `mask` generated with [secr::make.mask()],
#' suitable for use in SECR and openCR analyses.
#'
#' @details
#'
#' The function performs the following steps:
#'
#' 1. Creates a regular UTM grid using
#'    [create_utm_grid_N29()]
#'
#' 2. Applies a half-cell offset so mask points are centered relative
#'    to detector locations
#'
#' 3. Generates a habitat mask using [secr::make.mask()]
#'
#' 4. Optionally excludes regions defined in `exclude_regions`
#'
#' The resulting mask defines the spatial state space used for
#' estimating animal density and movement parameters.
#'
#' @examples
#' \dontrun{
#'
#' habitat_mask <- create_mask_N29(
#'   x_range = c(200000, 300000),
#'   y_range = c(4100000, 4200000)
#' )
#'
#' plot(habitat_mask)
#'
#' }
#'
#' @export
create_mask_N29 <-
  function(x_range,
           y_range,
           utm_resolution = 1000,
           x_pad_perc = 0.05,
           y_pad_perc = 0.05,
           buffer = 12000,
           exclude_regions = NULL) {

    trapping_grid <-
      create_utm_grid_N29(
        x_range = x_range,
        y_range = y_range,
        utm_resolution = utm_resolution,
        x_pad_perc = x_pad_perc,
        y_pad_perc = y_pad_perc,
        x_offset = -utm_resolution/2,
        y_offset = -utm_resolution/2
      )

    if (!is.null(exclude_regions)) {
      mask <-
        secr::make.mask(
          trapping_grid,
          nx = attr(trapping_grid, "nx"),
          ny = attr(trapping_grid, "ny"),
          spacing = utm_resolution,
          buffer = buffer,
          poly = exclude_regions,
          poly.habitat = FALSE
        )
    } else {
      mask <-
        secr::make.mask(
          trapping_grid,
          nx = attr(trapping_grid, "nx"),
          ny = attr(trapping_grid, "ny"),
          spacing = utm_resolution,
          buffer = buffer
        )
    }

    mask
  }
