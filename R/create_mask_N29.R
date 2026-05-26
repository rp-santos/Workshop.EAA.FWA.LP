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
