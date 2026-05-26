# 16 HEX	hazard exponential detection function
# see ?detectfn
hex <- function(d, sigma, lambda0) {
  
  1 - exp(- lambda0 * exp(-d / sigma))
  
}
