#' Calculate weekly boat survey effort by grid cell
#'
#' @description
#'
#' [boat_effort_per_week_N29()] calculates the total survey effort
#' (in minutes) per week and per spatial grid cell based on boat
#' tracking data.
#'
#' Boat positions are assigned to spatial grid cells using
#' [azores.fkw::find_grid_cells()], and effort is summarized as
#' the number of recorded minutes within each detector/grid cell.
#'
#' The function can return detector coordinates either in
#' geographic coordinates (longitude/latitude) or projected UTM
#' coordinates (EPSG:32629).
#'
#' @param boat_trips A data frame containing boat tracking data.
#'   Must include the following columns:
#'
#'   \describe{
#'     \item{trip}{Trip identifier}
#'     \item{week}{Week identifier}
#'     \item{longitude}{Longitude coordinates in decimal degrees}
#'     \item{latitude}{Latitude coordinates in decimal degrees}
#'   }
#'
#' @param grid A spatial grid object generated with
#'   [create_grid_N29()].
#'
#'   Defaults to `create_grid_N29()`.
#'
#' @param coordinates Character string indicating the coordinate
#'   system used for detector locations in the output.
#'
#'   Accepted values are:
#'
#'   \describe{
#'     \item{"utm"}{Returns UTM coordinates (default)}
#'     \item{"longlat"}{Returns longitude/latitude coordinates}
#'   }
#'
#' @returns
#'
#' A tibble where each row corresponds to a detector/grid cell
#' and columns contain the total number of survey minutes per
#' week.
#'
#' If `coordinates = "utm"`, the output includes:
#'
#'   \describe{
#'     \item{index}{Grid cell identifier}
#'     \item{easting}{UTM easting coordinate}
#'     \item{northing}{UTM northing coordinate}
#'   }
#'
#' If `coordinates = "longlat"`, the output includes:
#'
#'   \describe{
#'     \item{index}{Grid cell identifier}
#'     \item{longitude}{Longitude coordinate}
#'     \item{latitude}{Latitude coordinate}
#'   }
#'
#' Remaining columns correspond to weekly survey effort values
#' expressed in total minutes.
#'
#' @details
#'
#' The function performs the following steps:
#'
#' 1. Assigns each boat position to a spatial grid cell using
#'    [azores.fkw::find_grid_cells()]
#'
#' 2. Groups observations by week and detector/grid cell
#'
#' 3. Calculates the total number of survey minutes within each
#'    grid cell
#'
#' 4. Optionally converts detector coordinates from geographic
#'    coordinates to UTM zone 29N coordinates
#'
#' The resulting effort matrix can be used for spatially explicit
#' capture-recapture analyses and habitat-based effort correction.
#'
#' @examples
#' \dontrun{
#'
#' effort <- boat_effort_per_week_N29(
#'   boat_trips = boat_trips_by_min
#' )
#'
#' }
#'
#' @export
boat_effort_per_week_N29 <-
  function(boat_trips,
           grid = create_grid_N29(),
           coordinates = c("utm", "longlat")) {

    if (!rlang::has_name(boat_trips, "trip"))
      stop("`boat_trips` must have a column named trip")

    if (!rlang::has_name(boat_trips, "week"))
      stop("`boat_trips` must have a column named week")

    if (!rlang::has_name(boat_trips, "longitude"))
      stop("`boat_trips` must have a column named longitude")

    if (!rlang::has_name(boat_trips, "latitude"))
      stop("`boat_trips` must have a column named latitude")

    coordinates <- match.arg(coordinates)

    # Find detectors for any given moment of `boat_trips`
    detectors <- azores.fkw::find_grid_cells(points = boat_trips, grid = grid)

    effort <-
      boat_trips["week"] |>
      dplyr::bind_cols(detectors) |>
      dplyr::count(.data$week,
                   .data$index,
                   .data$longitude,
                   .data$latitude,
                   name = "total_minutes") |>
      tidyr::pivot_wider(
        names_from = c("week"),
        values_from = "total_minutes",
        values_fill = 0L
      )

    # Change effort to use UTM instead of Longitude/Latitude coordinates
    if (identical(coordinates, "utm")) {
      longlat <-
        dplyr::rename(effort[c("latitude", "longitude")],
                      lat = "latitude",
                      lon = "longitude")

      utm <- azores.fkw::longlat_to_utm(longlat, crs_to = 32629)
      effort <- tibble::tibble(effort["index"], utm[c("easting", "northing")], effort[-(1:3)])
    }

    effort
  }
