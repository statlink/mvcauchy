\name{Prediction with Cauchy naive Bayes classifier}
\alias{cauchynb.pred}

\title{
Prediction with Cauchy naive Bayes classifier
}

\description{
Prediction with Cauchy naive Bayes classifier.
}

\usage{
cauchynb.pred(xnew, location, scale, ni)
}

\arguments{
\item{xnew}{
A numerical matrix with new predictor variables whose group is to be predicted.
}
\item{location}{
A matrix with the group location parameters of the Cauchy distribution. Each row corresponds to a group.
}
\item{scale}{
A matrix with the group scale parameters of the Cauchy distribution. Each row corresponds to a group.
}
\item{ni}{
A vector with the frequencies of each group.
}
}

%\details{
%
%}

\value{
A numerical vector with 1, 2, ... denoting the predicted group.
}

\references{
Friedman J., Hastie T. and Tibshirani R. (2017). The elements of statistical learning.
New York: Springer.
}

\author{
Michail Tsagris.

R implementation and documentation: Michail Tsagris \email{mtsagris@uoc.gr}.
}
%\note{
%%  ~~further notes~~
%}

\seealso{
\code{ \link{cauchy.nb}, \link{cv.cauchynb}
}
}

\examples{
x <- as.matrix(iris[, 1:4])
ina <- as.numeric(iris[, 5])
a <- mvcauchy::cauchy.nb(x, x, ina = ina)
est <- mvcauchy::cauchynb.pred(x, a$location, a$scale, a$ni)
table(ina, est)
}

