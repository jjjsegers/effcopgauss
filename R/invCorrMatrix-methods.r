#' @include copGauss-class.r
#' @include accessor.r
#' @include corrMatrix-methods.r
NULL


#' @export
#' @rdname corrMatrix-methods
#' @name invCorrMatrix
setGeneric(
  name = "invCorrMatrix",
  def = function(object) {
    standardGeneric("invCorrMatrix")
  }
)

#' @aliases invCorrMatrix,copGauss-method
#' @rdname corrMatrix-methods
setMethod(
  f = "invCorrMatrix",
  signature = "copGauss",
  definition = function(object) {
    R <- corrMatrix(object)
    return(solve(R))
  }
)