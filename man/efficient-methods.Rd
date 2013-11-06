\docType{methods}
\name{effScore}
\alias{effInfMat}
\alias{effInfMat,copGauss-method}
\alias{effScore}
\alias{effScore,copGauss-method}
\title{Efficient Score Function and Information Matrix}
\description{
  Functions to compute the efficient score function and the
  efficient information matrix for a Gaussian copula within
  a Gaussian copula model. The efficient score function is
  represented as a vector of length k of quadratic forms in
  the Gaussianized observations, where k is the length of
  the parameter vector; the result is then the vector of
  matrices. The efficient information matrix is a k-by-k
  matrix.
}
\examples{
(cG <- copGaussToeplitz(3, c(0.2, 0.1)))
corrMatrix(cG)
effScore(cG)
effInfMat(cG)
}
\references{
  Segers, J., van den Akker, R.A., Werker, B.J.M. (2013).
  Semiparametric Gaussian Copula Models: Geometry and
  Efficient Rank-Based Estimation.
  \url{http://arxiv.org/abs/1306.6658}
}

