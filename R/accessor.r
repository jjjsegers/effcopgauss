#' @include copGauss-class.r
NULL

#' Accessor functions for objects of class \code{copGauss}
#' 
#' Functions to access the various slots of a Gaussian copula object.
#' 
#' @docType methods
#' @export
#' @rdname accessor
#' @name param
#' @examples
#' cG <- copGaussToeplitz(3, c(0.2, 0.1))
#' dim(cG)
#' param(cG)
setGeneric(
  name = "param",
  def = function(object) {
    standardGeneric("param")
  }
)

#' @aliases param,copGauss-method
#' @rdname accessor
setMethod(
  f = "param",
  signature = "copGauss",
  definition = function(object) {
    return(object@param)
  }
)

#' @export
#' @rdname accessor
#' @name dim
setGeneric(name = "dim")

#' @aliases dim,copGauss-method
#' @rdname accessor
setMethod(
  f = "dim",
  signature = "copGauss",
  definition = function(x) {
    return(x@dim)
  }
)