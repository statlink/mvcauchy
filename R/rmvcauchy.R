rmvcauchy <- function(n, mu, sigma) {
  Rfast::rmvt(n, mu, sigma, v = 1)
}
