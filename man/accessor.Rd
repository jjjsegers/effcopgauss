\docType{methods}
\name{param}
\alias{dim}
\alias{dim,copGauss-method}
\alias{param}
\alias{param,copGauss-method}
\title{Accessor functions for objects of class \code{copGauss}}
\description{
  Functions to access the various slots of a Gaussian
  copula object.
}
\examples{
cG <- copGaussToeplitz(3, c(0.2, 0.1))
dim(cG)
param(cG)
}

