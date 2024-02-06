dmvcauchy <- function(x, mu, sigma, logged = FALSE) {
  Rfast::dmvt(x, mu, sigma, nu = 1, logged)
}
