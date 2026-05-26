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
