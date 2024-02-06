mvcauchy.mle <- function(x, tol = 1e-7) {
  Rfast::mvt.mle(x, v = 1, tol)
}
