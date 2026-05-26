#' @export
whale_prob_alg <- function(mask, grid, capture, params, detectfn = hex) {

  p <- vector(mode = "double", nrow(mask))
  grid_tbl <-
    raster::coordinates(grid) |>
    tibble::as_tibble() |>
    dplyr::rename(lon = .data$x, lat = .data$y) |>
    longlat_to_utm_alg()


  for (i in seq_len(nrow(capture))) {

    d <- whale_distances(i, mask = mask, grid_tbl = grid_tbl, capture = capture)
    p <- p + detectfn(d, sigma = params$sigma, lambda0 = params$lambda0)
  }

  dplyr::bind_cols(mask, p = p / sum(p)) |>
    tibble::as_tibble()
}
