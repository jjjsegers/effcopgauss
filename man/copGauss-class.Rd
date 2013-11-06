\docType{class}
\name{copGaussFull}
\alias{copGauss}
\alias{copGaussCircular}
\alias{copGaussExchangeable}
\alias{copGaussExchangeble}
\alias{copGaussFull}
\alias{copGaussToeplitz}
\title{Gaussian copula classes.}
\usage{
  copGaussFull(dim, param)

  copGaussToeplitz(dim, param)

  copGaussExchangeble(dim, param)

  copGaussCircular(dim, param)
}
\arguments{
  \item{dim}{The copula dimension.}

  \item{param}{The copula parameter vector.}
}
\description{
  Classes to represent Gaussian copulas with structured
  correlation matrices.
}
\section{Slots}{
  \describe{ \item{\code{dim}:}{The dimension of the
  copula.} \item{\code{param}:}{The parameter vector.} }
}
\examples{
(cG <- copGaussFull(dim = 3, param = c(0.1, 0.2, 0.3)))
corrMatrix(cG)
(cG <- copGaussToeplitz(dim = 3, param = c(0.1, 0.2)))
corrMatrix(cG)
(cG <- copGaussExchangeable(dim = 4, param = 0.5))
corrMatrix(cG)
(cG <- copGaussCircular(dim = 4, param = 0.5))
corrMatrix(cG)
}
\references{
  Segers, J., van den Akker, R.A., Werker, B.J.M. (2013).
  Semiparametric Gaussian Copula Models: Geometry and
  Efficient Rank-Based Estimation.
  \url{http://arxiv.org/abs/1306.6658}
}

