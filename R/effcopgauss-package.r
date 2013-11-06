#' effcopgauss
#'
#' The package provides classes and methods for the calculation of the efficient score function and the efficient information matrix for the Euclidean parameter in a semiparametric Gaussian copula model with unknown margins. Currently implemented are the full model, Toeplitz correlation matrices, exchangeable correlation structures, and a circular model in dimension 4.
#'
#' @references
#' Segers, J., van den Akker, R.A., Werker, B.J.M. (2013). Semiparametric Gaussian Copula Models: Geometry and Efficient Rank-Based Estimation. \url{http://arxiv.org/abs/1306.6658}
#' @name effcopgauss
#' @docType package
#' @examples
#' (cG <- copGaussToeplitz(3, c(0.2, 0.1)))
#' corrMatrix(cG)
#' effScore(cG)
#' effInfMat(cG)
NULL
