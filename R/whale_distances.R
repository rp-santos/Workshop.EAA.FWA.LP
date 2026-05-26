whale_distances <- function(i, mask, grid_tbl, capture) {
  
  detector_index <- capture[i, "Detector", drop = TRUE]
  
  # x-coordinate of whale i
  x_i <- grid_tbl[detector_index, "easting", drop = TRUE]
  # y-coordinate of whale i
  y_i <- grid_tbl[detector_index, "northing", drop = TRUE]
  
  dx <- x_i - mask[, 1]
  dy <- y_i - mask[, 2]
  
  # Distance between point (x_i, y_i) and grid cell points
  d <- sqrt(dx^2 + dy^2)
  
  d
  
}
