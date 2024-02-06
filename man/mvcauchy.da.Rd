\name{Maximum likelihood discriminant analysis using the multivariate Cauchy distribution}
\alias{mvcauchy.da}
\title{
Maximum likelihood discriminant analysis using the multivariate Cauchy distribution
}
\description{
Maximum likelihood discriminant analysis using the multivariate Cauchy distribution.
}

\usage{
mvcauchy.da(xnew, x, ina, mod = NULL)
}

\arguments{
\item{xnew}{
A numerical matrix with the new data whose class is to predicted. The rows correspond to observations and the columns to variables.
}
\item{x}{
A numerical matrix with the data. The rows correspond to observations and the columns to variables.
}
\item{ina}{
Should the logarithm of the density be returned (TRUE) or not (FALSE)?
}
\item{mod}{
This is a list with the estimated parameters of each class obtained from the function \code{\link{mvcauchy.mle}}.
If this is not available, then the function will compute the location and scatter from the available data.
}
}

\details{
Maximum likelihood discriminant analysis using the multivariate Cauchy distribution is performed.
}

\value{
A list including:
\item{mod}{
A list with the output produced by \code{\link{mvcauchy.mle}}, for each class.
}
\item{prob}{
The estimated probabilities of the new data of belonging to each group.
}
\item{est}{
he estimated group membership of the new data.
}
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
\code{ \link{rmvcauchy}, \link{mvcauchy.mle}
}
}

\examples{
x <- as.matrix(iris[, 1:4])
ina <- iris[, 5]
a <- mvcauchy.da(x, x, ina)
}

