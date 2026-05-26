#' Detection function (half-normal form)
#'
#' @description
#'
#' [hex()] computes the detection probability as a function of distance
#' using an exponential decay model commonly used in spatial
#' capture-recapture (SECR) analyses.
#'
#' The function describes how detection probability decreases with
#' increasing distance from the detector, controlled by scale
#' parameters `sigma` and `lambda0`.
#'
#' @param d Numeric vector of distances between animals and detectors.
#'
#' @param sigma Positive numeric parameter controlling the rate of
#'   spatial decay in detection probability. Larger values indicate
#'   broader detection ranges.
#'
#' @param lambda0 Positive numeric parameter representing baseline
#'   detection intensity at distance zero.
#'
#' @returns
#' A numeric vector of detection probabilities, bounded between 0 and 1.
#'
#' @details
#'
#' The detection function is defined as:
#'
#' \deqn{p(d) = 1 - \exp(-\lambda_0 \exp(-d / \sigma))}
#'
#' where:
#'
#' - \(d\) is the distance from detector
#' - \(\sigma\) controls spatial decay
#' - \(\lambda_0\) is the baseline detection parameter
#'
#' This formulation is commonly used in SECR and openCR models for
#' modeling detection probability as a decreasing function of distance.
#'
#' @examples
#' \dontrun{
#'
#' d <- seq(0, 10000, by = 100)
#'
#' p <- hex(d = d, sigma = 3000, lambda0 = 0.2)
#'
#' plot(d, p, type = "l")
#'
#' }
#' @export
hex <- function(d, sigma, lambda0) {

  1 - exp(- lambda0 * exp(-d / sigma))

}
