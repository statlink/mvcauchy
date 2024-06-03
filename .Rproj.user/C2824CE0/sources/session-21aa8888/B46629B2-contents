\name{Cross-validation for the Cauchy discriminant analysis}
\alias{cv.mvcauchyda}
\title{
Cross-validation for the Cauchy discriminant analysis
}

\description{
Cross-validation for the Cauchy discriminant analysis.
}

\usage{
cv.mvcauchyda(x, ina, nfolds = 10, folds = NULL, stratified = TRUE, seed = NULL)
}

\arguments{
\item{x}{
A matrix with the data.
}
\item{ina}{
A group indicator variable for the avaiable data.
}
\item{nfolds}{
The number of folds in the cross validation.
}
\item{folds}{
If you have the list with the folds supply it here. You can also leave it NULL
and it will create folds.
}
\item{stratified}{
Do you want the folds to be created in a stratified way? TRUE or FALSE.
}
\item{seed}{
You can specify your own seed number here or leave it NULL.
}
}

\details{
Cross validation is performed to select the optimal parameters for the maximum
likelihood Cauchy discriminant analysis and also estimate the rate of accuracy.
}

\value{
A list including:
\item{perf}{
The estimated rate of correct classification.
}
\item{runtime}{
The time required by the cross-validation procedure.
}
}

\references{
Friedman Jerome, Trevor Hastie and Robert Tibshirani (2017).
The elements of statistical learning. New York: Springer.
}

\author{
Michail Tsagris.

R implementation and documentation: Michail Tsagris \email{mtsagris@uoc.gr}.
}

%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link{mvcauchy.da}
}
}

\examples{
mod <- cv.mvcauchyda(as.matrix(iris[, 1:4]), iris[, 5])
mod
}

