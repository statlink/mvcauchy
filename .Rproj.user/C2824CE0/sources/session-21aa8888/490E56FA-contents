mvcauchy.da <- function(xnew, x, ina, mod = NULL) {
  ina <- as.numeric(ina)
  d <- dim(x)[2]
  ni <- tabulate(ina)
  g <- length(ni)

  xnew <- as.matrix(xnew)
  if ( dim(xnew)[2] == 1 )  xnew <- matrix(xnew, ncol = d)
  nu <- dim(xnew)[1]

  est <- matrix(nrow = nu, ncol = g)

  if ( is.null(mod) ) {
    mod <- list()
    for (i in 1:g) {
      mod[[ i ]] <- mvcauchy::mvcauchy.mle(x[ina == i, ])
      est[, i] <- mvcauchy::dmvcauchy(xnew, mod[[ i ]]$location,
                            mod[[ i ]]$scatter, logged = TRUE) + log(ni[i])
    }

  } else {
    for (i in 1:g)  est[, i] <- mvcauchy::dmvcauchy(x[ina == i, ], mod[[ i ]]$location,
                                          mod[[ i ]]$scatter, logged = TRUE) + log(ni[i])

  } ## end if ( is.null(mod) )

  prob <- exp(est)
  prob <- prob / Rfast::rowsums( prob ) ## the probability of classification

  list( mod = mod, prob = prob, est = Rfast::rowMaxs(est) )
}
