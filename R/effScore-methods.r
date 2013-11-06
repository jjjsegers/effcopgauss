#' @include copGauss-class.r
#' @include accessor.r
#' @include corrMatrix-methods.r
#' @include invCorrMatrix-methods.r
#' @include corrMatrixD-methods.r
#' @include invCorrMatrixD-methods.r
NULL


#' Efficient Score Function and Information Matrix
#' 
#' Functions to compute the efficient score function and the efficient information matrix for a Gaussian copula within a Gaussian copula model. The efficient score function is represented as a vector of length k of quadratic forms in the Gaussianized observations, where k is the length of the parameter vector; the result is then the vector of matrices. The efficient information matrix is a k-by-k matrix.
#' 
#' @docType methods
#' @export
#' @rdname efficient-methods
#' @name effScore
#' @references
#' Segers, J., van den Akker, R.A., Werker, B.J.M. (2013). Semiparametric Gaussian Copula Models: Geometry and Efficient Rank-Based Estimation. \url{http://arxiv.org/abs/1306.6658}
#' @examples
#' (cG <- copGaussToeplitz(3, c(0.2, 0.1)))
#' corrMatrix(cG)
#' effScore(cG)
#' effInfMat(cG)
setGeneric(
  name = "effScore",
  def = function(object) {
    standardGeneric("effScore")
  }
)

#' @aliases effScore,copGauss-method
#' @rdname efficient-methods
setMethod(
  f = "effScore",
  signature = "copGauss",
  definition = function(object) {
    R <- corrMatrix(object)
    S <- invCorrMatrix(object)
    Rd <- corrMatrixD(object)
    Sd <- invCorrMatrixD(object)
    k <- length(param(object))
    p <- dim(object)
    Ip <- diag(p)
    ones <- rep.int(1, p)
    if (identical(k, 1L)) {
      g <- - solve(Ip + R * S, (Rd * S) %*% ones)
      g <- diag(drop(g))
      A <- S %*% g + g %*% S - Sd
    } else {
      A <- array(0, dim = c(p, p, k))
      for (m in 1L:k) {
        g <- - solve(Ip + R * S, (Rd[,,m] * S) %*% ones)
        g <- diag(drop(g))
        A[,,m] <- S %*% g + g %*% S - Sd[,,m]
      }
    }
    return(A)
  }
)