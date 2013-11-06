#' @include copGauss-class.r
#' @include accessor.r
NULL

#' @export
#' @rdname corrMatrix-methods
#' @name corrMatrixD
setGeneric(
  name = "corrMatrixD",
  def = function(object) {
    standardGeneric("corrMatrixD")
  }
)

#' @aliases corrMatrixD,copGaussFull-method
#' @rdname corrMatrix-methods
setMethod(
  f = "corrMatrixD",
  signature = "copGaussFull",
  definition = function(object) {
    k <- length(param(object))
    p <- dim(object)
    Rd <- array(0, dim = c(p,p,k))
    m <- 1L
    for (i in 1L:(p-1L)) {
      for (j in (i+1L):p) {
        Rd[i,j,m] <- Rd[j,i,m] <- 1
        m <- m + 1L
      }
    }
    return(Rd)
  }
)

#' @aliases corrMatrixD,copGaussExchangeable-method
#' @rdname corrMatrix-methods
setMethod(
  f = "corrMatrixD",
  signature = "copGaussExchangeable",
  definition = function(object) {
    p <- dim(object)
    Rd <- matrix(1, nrow = p, ncol = p)
    diag(Rd) <- numeric(p)
    return(Rd)
  }
)

#' @aliases corrMatrixD,copGaussCircular-method
#' @rdname corrMatrix-methods
setMethod(
  f = "corrMatrixD",
  signature = "copGaussCircular",
  definition = function(object) {
    theta <- param(object)
    return(toeplitz(c(0, 1, 2 * theta, 1)))
  }
)

#' @aliases corrMatrixD,copGaussToeplitz-method
#' @rdname corrMatrix-methods
setMethod(
  f = "corrMatrixD",
  signature = "copGaussToeplitz",
  definition = function(object) {
    p <- dim(object)
    k <- p-1L
    Rd <- array(data = 0, dim = c(p, p, k))
    Ip <- diag(p)
    for (m in 1L:k) {
      u <- Ip[m+1,]
      Rd[,,m] <- toeplitz(u)
    }
    return(Rd)
  }
)