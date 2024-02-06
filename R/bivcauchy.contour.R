bivcauchy.contour <- function(x, m = NULL, s = NULL, cont.lines = TRUE, add.points = TRUE) {
    n1 <- 100
    n2 <- 100
    x1 <- seq(min(x[, 1]) - 0.5, max(x[, 1]) + 0.5, length = n1)
    x2 <- seq(min(x[, 2]) - 0.5, max(x[, 2]) + 0.5, length = n2)

    if ( is.null(m) | is.null(s) ) {
      f <- mvcauchy::mvcauchy.mle(x)
      m <- f$location
      s <- f$scatter
    }
    r <- s[2]/sqrt(s[1] * s[4])
    con <- 0.5 * sqrt(pi) - lgamma(1/2) - 0.5 * log( det(pi * s) )
    z1 <- (x1 - m[1])/sqrt(s[1])
    z2 <- (x2 - m[2])/sqrt(s[4])
    mat1 <- outer(z1^2, rep(1, n1), "*")
    mat2 <- outer(rep(1, n2), z2^2, "*")
    mat3 <- tcrossprod(z1, z2)
    mat <- con - 1.5 * log1p( 1/(1 - r^2) * (mat1 + mat2 - 2 * r * mat3) )

    mat <- exp(mat)
    ind <- (mat < Inf)
    ind[ind == FALSE] <- NA
    mat <- mat * ind

    oldpar <- par(fg = NA)
    on.exit( par(oldpar) )
    filled.contour(x1, x2, mat, nlevels = 200, color.palette =  colorRampPalette( c( "blue","cyan","yellow","red") ),
                   xlab = colnames(x)[1], ylab = colnames(x)[2], cex.lab = 1.2, cex.axis = 1.2,
        key.axes = {axis(4, col = "black")},
        plot.axes = {
        if ( cont.lines )  contour(x1, x2, mat, nlevels = 10, col = 1, labcex = 0.8, lwd = 1.5, add = TRUE)
        if (add.points) {
          points(x[, 1], x[, 2], pch = 20, col = 1)
          points(m[1], m[2], pch = 10, cex = 1.5, col = 1)
        }
      }
    )
}
