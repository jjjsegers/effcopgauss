#' @include copGauss-class.r
#' @include accessor.r
NULL

#' Correlation Matrix, Its Inverse, and Their Derivatives
#' 
#' Methods to compute the (inverse) correlation matrix of a Gaussian copula object, i.e., an object of class \code{copGauss}, as well as the derivative matrix or matrices with respect to the copula parameter (vector).
#' 
#' @docType methods
#' @rdname corrMatrix-methods
#' @export
#' @name corrMatrix
#' @references
#' Segers, J., van den Akker, R.A., Werker, B.J.M. (2013). Semiparametric Gaussian Copula Models: Geometry and Efficient Rank-Based Estimation. \url{http://arxiv.org/abs/1306.6658}
#' @examples
#' (cG <- copGaussToeplitz(4, c(0.3, 0.2, 0.1)))
#' corrMatrix(cG)
#' corrMatrixD(cG)
#' invCorrMatrix(cG)
#' invCorrMatrixD(cG)
setGeneric(
  name = "corrMatrix",
  def = function(object) {
    standardGeneric("corrMatrix")
  }
)

#' @aliases corrMatrix,copGaussToeplitz-method
#' @rdname corrMatrix-methods
setMethod(
  f = "corrMatrix",
  signature = "copGaussToeplitz",
  definition = function(object) {
    theta <- param(object)
    return(toeplitz(c(1, theta)))
  }
)

#' @aliases corrMatrix,copGaussExchangeable-method
#' @rdname corrMatrix-methods
setMethod(
  f = "corrMatrix",
  signature = "copGaussExchangeable",
  definition = function(object) {
    p <- dim(object)
    theta <- param(object)
    R <- matrix(theta, nrow = p, ncol = p)
    diag(R) <- rep.int(1, p)
    return(R)
  }
)

#' @aliases corrMatrix,copGaussCircular-method
#' @rdname corrMatrix-methods
setMethod(
  f = "corrMatrix",
  signature = "copGaussCircular",
  definition = function(object) {
    theta <- param(object)
    return(toeplitz(c(1, theta, theta*theta, theta)))
  }
)

#' @aliases corrMatrix,copGaussFull-method
#' @rdname corrMatrix-methods
setMethod(
  f = "corrMatrix",
  signature = "copGaussFull",
  definition = function(object) {
    p <- dim(object)
    theta <- param(object)
    R <- diag(p)
    m <- 1L
    for (i in 1L:(p-1L)) {
      for (j in (i+1L):p) {
        R[i,j] <- R[j,i] <- theta[m]
        m <- m + 1L
      }
    }
    return(R)
  }
)