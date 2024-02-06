\name{Density of the multivariate Cauchy distribution}
\alias{dmvcauchy}
\title{
Density of the multivariate Cauchy distribution
}
\description{
Density of the multivariate Cauchy distribution.
}

\usage{
dmvcauchy(x, mu, sigma, logged = FALSE)
}

\arguments{
\item{x}{
A numerical matrix with the data. The rows correspond to observations and the columns to variables.
}
\item{mu}{
The mean vector.
}
\item{sigma}{
The scatter matrix.
}
\item{logged}{
Should the logarithm of the density be returned (TRUE) or not (FALSE)?
}
}

\details{
The (log) density of the multivariate Cauchy distribution is calculated for given mean vector and covariance matrix.
}

\value{
A numerical vector with the density values calculated at each vector (row of the matrix x).
}

\references{
Kanti V. Mardia, John T. Kent and John M. Bibby (1979). Multivariate analysis. Academic Press, London.
}

\author{
Michail Tsagris.

R implementation and documentation: Michail Tsagris \email{mtsagris@uoc.gr}.
}
%\note{
%%  ~~further notes~~
%}

\seealso{
\code{ \link{rmvcauchy}
}
}

\examples{
x <- as.matrix(iris[, 1:4])
mod <- mvcauchy.mle(x)
m <- mod$location
s <- mod$scatter
a <- dmvcauchy(x, m, s)
}

