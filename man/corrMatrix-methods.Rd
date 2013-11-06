\docType{methods}
\name{corrMatrix}
\alias{corrMatrix}
\alias{corrMatrix,copGaussCircular-method}
\alias{corrMatrix,copGaussExchangeable-method}
\alias{corrMatrix,copGaussFull-method}
\alias{corrMatrix,copGaussToeplitz-method}
\alias{corrMatrixD}
\alias{corrMatrixD,copGaussCircular-method}
\alias{corrMatrixD,copGaussExchangeable-method}
\alias{corrMatrixD,copGaussFull-method}
\alias{corrMatrixD,copGaussToeplitz-method}
\alias{invCorrMatrix}
\alias{invCorrMatrix,copGauss-method}
\alias{invCorrMatrixD}
\alias{invCorrMatrixD,copGauss-method}
\title{Correlation Matrix, Its Inverse, and Their Derivatives}
\description{
  Methods to compute the (inverse) correlation matrix of a
  Gaussian copula object, i.e., an object of class
  \code{copGauss}, as well as the derivative matrix or
  matrices with respect to the copula parameter (vector).
}
\examples{
(cG <- copGaussToeplitz(4, c(0.3, 0.2, 0.1)))
corrMatrix(cG)
corrMatrixD(cG)
invCorrMatrix(cG)
invCorrMatrixD(cG)
}
\references{
  Segers, J., van den Akker, R.A., Werker, B.J.M. (2013).
  Semiparametric Gaussian Copula Models: Geometry and
  Efficient Rank-Based Estimation.
  \url{http://arxiv.org/abs/1306.6658}
}

