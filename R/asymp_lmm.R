#' Asymptotic Power Computation for Linear Mixed Effects Model (LMM)
#'
#' @param n numeric.
#' @param p numeric.
#' @param m numeric.
#' @param delta numeric.
#' @param sig2 numeric.
#' @param sig2_b1 numeric.
#' @param d numeric.
#'
#' @return numeric.
#' @export
#' 
asymp_lmm <- function(n = 1000, p = 0.3, m = 4, delta = 3, sig2 = 0.08, sig2_b1 = 0.0008, d = 0.005) {
  ## To compare empirical with theoretical power estimation for testing H1:Beta3 = d
  # A <- (12 * sig2) / (delta^2 * m * (m^2-1)) + sig2_b1
  # A <- 1/(delta * (m^2-1)) + sig2_b1/sig2
  measures <- (seq(m) - 1) * delta
  A <- 1 / sum((measures - mean(measures))^2) + sig2_b1 / sig2
  ncp.d <- (n * 2 * p * (1 - p) * d^2) / (sig2 * A)
  stats::pchisq(stats::qchisq(0.95, 1), 1, ncp.d, lower.tail = FALSE)
}
