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
