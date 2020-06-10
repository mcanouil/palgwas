#' Asymptotic Power Computation for Generalised Estimating Equations (GEE)
#'
#' @param n numeric.
#' @param m numeric.
#' @param p numeric.
#' @param a numeric.
#' @param tau1 numeric.
#' @param tau2 numeric.
#' @param delta numeric.
#' @param d numeric.
#'
#' @return numeric.
#' @export
#' 
asymp_gee <- function(
  n = 1000,
  m = 4,
  p = 0.3,
  a = 0.9, # parameter of AR(1) process
  tau1 = 0.08, # variance associated to AR(1)
  tau2 = 0.07, # variance associated to local effect
  delta = 3,
  d = 0.005
) {
  ## solve equation for b
  lamb <- tau2 / tau1

  # roots.b <- polyroot( c(lamb * a, -(1-a^2)-lamb * (1+a^2), lamb * a) )
  # b1 <- as.numeric(roots.b[1])
  # b2 <- as.numeric(roots.b[2])   # always equal to 1 / b1

  a.coeff <- lamb * a
  b.coeff <- -(1 - a^2) - lamb * (1 + a^2)
  c.coeff <- lamb * a
  disc <- b.coeff^2 - 4 * a.coeff * c.coeff
  b1 <- (-b.coeff - sqrt(disc)) / (2 * a.coeff)
  b2 <- (-b.coeff + sqrt(disc)) / (2 * a.coeff)


  cov_arma <- function(m, a, b) {
    x <- matrix(0, m, m)
    gam0 <- ((1 - a^2) + (a - b)^2) / (1 - a^2)
    cst <- ((1 - a * b) * (a - b)) / (1 - a^2)
    for (i in 1:m) {
      for (j in 1:m) {
        if (abs(i - j) == 0) {
          x[i, j] <- gam0
        } else {
          x[i, j] <- cst * a^(abs(i - j) - 1)
        }
      }
    }
    x
  }
  b <- b1
  covARMA_eq <- (tau1 * a * (1 - a^2)) / 
    ((1 - a * b) * (a - b)) * cov_arma(m, a, b) # var-cov of ARMA(1, 1) equivalent

  ## Computation of NCP for testing H1:Beta3 = d
  # A = 1' * Mn * 1 where Mn is inverse of variance-covariance matrix of ARMA(1, 1)
  cst <- (tau1 * a * (1 - a^2)) / ((1 - a * b) * (a - b))
  A <- m * ((1 - a) / (1 - b))^2 - 
    (2 * (1 - a * b) * (1 - a) * (b - a) * (1 - b^m)) / 
      ((1 - b)^3 * (1 - a * b + (b - a) * b^m))
  A <- A / cst
  # B = t' * Mn * 1
  B <- delta * ((m + 1) / 2) * A
  # C = t' * Mn * t
  C <- (1 / (1 - b)^4) *
    (
      (1 - a)^2 * (1 - b)^2 * m * (m + 1) * (2 * m + 1) / 6 -
        (1 - a) * (1 - b) * (b - a) * m * (m + 1) +
        (b - a)^2 * m
    ) +
    ( (b - a) / ( (1 - b)^4 * ((1 - a * b)^2 - (b - a)^2 * b^(2 * m)) ) ) *
      (
        (b + a) * (1 - a * b)^2 -
          2 * a * (1 - b^2) * (1 - a * b) * (1 + m * (1 - a)) * b^m -
          (b - a) * ( b^2 * (1 - a^2) + (1 - b^2) * (1 + m * (1 - a))^2 ) * b^(2 * m)
      )
  C <- delta^2 * C / cst

  ncp.d <- n * 2 * p * (1 - p) * d^2 * ((A * C - B^2) / A)
  
  stats::pchisq(stats::qchisq(0.95, 1), 1, ncp.d, lower.tail = FALSE)
}
