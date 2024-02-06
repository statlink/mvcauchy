\name{Contour plot of the bivariate Cauchy distribution}
\alias{bivcauchy.contour}
\title{
Contour plot of the bivariate Cauchy distribution
}

\description{
Contour plot of the bivariate Cauchy distribution.
}

\usage{
bivcauchy.contour(x, m = NULL, s = NULL, cont.lines = TRUE, add.points = TRUE)
}

\arguments{
\item{x}{
A matrix with two columns containing the data.
}
\item{m}{
If you know the location vector place it here. In this case the function still
needs the argument with the data.
}
\item{s}{
If you know the scatter matrix place it here. In this case the function still
needs the argument with the data.
}
\item{cont.lines}{
Do you want the contour lines to appear?
}
\item{add.points}{
Do you want the data points to appear?
}
}

\value{
The contour plot.
}

\details{
The function plots the contours of the bivariate Cauchy distribution whose parameters
are either estimated from some data or they are given as inputs. If you want the contour
plot of the bivariate Cauchy distribution with some pre-specified location and scatter
matrix, then provide some cleverly specified data "x" so that you receive what you want.
See the examples.
}

\author{
Michail Tsagris and Christos Adam.

R implementation and documentation: Michail Tsagris \email{mtsagris@uoc.gr}
and Christos Adam \email{pada4m4@gmail.com}.
}

\seealso{
\code{\link{dmvcauchy}
}
}

\examples{
## not specified location and scatter
x <- as.matrix(iris[, 1:2])
bivcauchy.contour(x)

## with specified location and scatter
m <- colMeans(x)
s <- cov(x)
## in this case you may need to adjust the data points youself
## "play"" with minimum and maximum values of the two axes
y <- rbind( apply(x, 2, min) - 1, apply(x, 2, max) + 2 )
bivcauchy.contour(y, m = m, s = s, add.points = FALSE)
}
