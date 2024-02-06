\name{Random values simulation from the multivariate Cauchy distribution}
\alias{rmvcauchy}
\title{
Random values simulation from the multivariate Cauchy distribution
}

\description{
Random values simulation from the multivariate Cauchy distribution.
}

\usage{
rmvcauchy(n, mu, sigma)
}

\arguments{
\item{n}{
The sample size, a numerical value.
}
\item{mu}{
The mean vector in \eqn{R^d}.
}
\item{sigma}{
The scatter matrix in \eqn{R^d}.
}
}

%\details{
%The algorithm uses univariate normal random values and transforms them to multivariate via a spectral decomposition.
%}

\value{
A matrix with the simulated data.
}

\references{
Aitchison J. (1986). The statistical analysis of compositional data. Chapman & Hall.
}

\author{
Michail Tsagris.

R implementation and documentation: Michail Tsagris \email{mtsagris@uoc.gr}.
}

%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link{dmvcauchy}
}
}

\examples{
x <- as.matrix(iris[, 1:4])
mod <- mvcauchy.mle(x)
m <- mod$location
s <- mod$scatter
y <- rmvcauchy(1000, m, s)
mvcauchy.mle(y)
}
