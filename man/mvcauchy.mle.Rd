\name{MLE of the multivariate Cauchy distribution}
\alias{mvcauchy.mle}
\title{
MLE of the multivariate Cauchy distribution
}

\description{
MLE of the multivariate Cauchy distribution.
}

\usage{
mvcauchy.mle(x, tol = 1e-07)
}

\arguments{
\item{x}{
A matrix with numerical data.
}
\item{tol}{
The tolerance value to terminate the EM algorithm.
}
}

\details{
The location vector, scatter matrix and the value of the log-likelihood is calculated.
}

\value{
A list including:
\item{iters}{The number of iterations required for the EM algorihm to converge.
}
\item{loglik}{The value of the maximised log-likelihood.
}
\item{location}{
The location vector.
}
\item{scatter}{
The scatter matrix.
}
}

\references{
Nadarajah S. and Kotz S. (2008). Estimation methods for the multivariate t distribution.
Acta Applicandae Mathematicae, 102(1): 99--118.
}

\author{
Michail Tsagris.

R implementation and documentation: Michail Tsagris \email{mtsagris@uoc.gr}.
}
%\note{
%%  ~~further notes~~
%}

\seealso{
\code{ \link{rmvcauchy}, \link{dmvcauchy}
}
}

\examples{
x <- as.matrix(iris[, 1:2])
res <- mvcauchy.mle(x)
}

