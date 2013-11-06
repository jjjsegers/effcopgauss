#' @include copGauss-class.r
#' @include accessor.r
#' @include corrMatrixD-methods.r
#' @include invCorrMatrix-methods.r
NULL

#' @export
#' @rdname corrMatrix-methods
#' @name invCorrMatrixD
setGeneric(
  name = "invCorrMatrixD",
  def = function(object) {
    standardGeneric("invCorrMatrixD")
  }
)

#' @aliases invCorrMatrixD,copGauss-method
#' @rdname corrMatrix-methods
setMethod(
  f = "invCorrMatrixD",
  signature = "copGauss",
  definition = function(object) {
    k <- length(param(object))
    S <- invCorrMatrix(object)
    Rd <- corrMatrixD(object)
    if (identical(k, 1L)) {
      Sd <- - S %*% Rd %*% S
    } else {
      p <- dim(object)
      Sd <- array(0, dim = c(p, p, k))
      for (m in 1L:k) {
        Sd[,,m] <- - S %*% Rd[,,m] %*% S
      }
    }
    return(Sd)
  }
)