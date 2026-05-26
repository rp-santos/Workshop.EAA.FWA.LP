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
