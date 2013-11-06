# To be implemented (?): 
# 
# * A class for factor model(s)
# * Within each class: a slot for the names of the variables

#' Gaussian copula classes.
#' 
#' Classes to represent Gaussian copulas with structured correlation matrices.
#' 
#' @docType class
#' @rdname copGauss-class
#' @section Slots:
#' \describe{
#'  \item{\code{dim}:}{The dimension of the copula.}
#'  \item{\code{param}:}{The parameter vector.}
#' }
#' @param dim The copula dimension.
#' @param param The copula parameter vector.
#' @references
#' Segers, J., van den Akker, R.A., Werker, B.J.M. (2013). Semiparametric Gaussian Copula Models: Geometry and Efficient Rank-Based Estimation. \url{http://arxiv.org/abs/1306.6658}
#' @exportClass copGaussFull
#' @name copGaussFull
#' @examples
#' (cG <- copGaussFull(dim = 3, param = c(0.1, 0.2, 0.3)))
#' corrMatrix(cG)
#' (cG <- copGaussToeplitz(dim = 3, param = c(0.1, 0.2)))
#' corrMatrix(cG)
#' (cG <- copGaussExchangeable(dim = 4, param = 0.5))
#' corrMatrix(cG)
#' (cG <- copGaussCircular(dim = 4, param = 0.5))
#' corrMatrix(cG)
setClass(
  Class = "copGaussFull",
  slots = c(
    dim = "integer",
    param = "vector"
  ),
  validity = function(object) {
    p <- object@dim
    theta <- object@param
    k <- length(theta)
    if (p <= 1L) {
      return("Copula dimension should be at least 2.")
    } else if (!identical(2L * k, p * (p-1L))) {
      return("Length parameter vector does not match copula dimension.")
    } else if (min(theta) <= -1) {
      return("All parameter components should be larger than -1.")
    } else if (max(theta) >= +1) {
      return("All parameter components should be smaller than +1.")
    } else {
      R <- diag(p)
      m <- 1L
      for (i in 1L:(p-1L)) {
        for (j in (i+1L):p) {
          R[i,j] <- R[j,i] <- theta[m]
          m <- m + 1L
        }
      }
      if (min(eigen(R)$values <= 0)) {
        return("Correlation matrix should be positive definite.")
      } else return(TRUE)
    }
  }
)

#' @export
#' @rdname copGauss-class
copGaussFull <- function(dim, param) {
  new("copGaussFull",
      dim = as.integer(dim),
      param = as.numeric(param))
}

#' @exportClass copGaussToeplitz
#' @name copGaussToeplitz
#' @rdname copGauss-class
setClass(
  Class = "copGaussToeplitz",
  slots = c(
    dim = "integer",
    param = "numeric"
  ),
  validity = function(object) {
    p <- object@dim
    theta <- object@param
    R <- toeplitz(c(1, theta))
    k <- length(theta)
    if (!identical(p, k + 1L)) {
      return("The length of the parameter vector does not match the dimension of the copula.")
    } else if (min(theta) <= -1) {
      return("All parameter components should be larger than -1.")
    } else if (max(theta) >= 1) {
      return("All parameter components should be smaller than +1.")
    } else if (min(eigen(R)$values) <= 0) {
      return("Correlation matrix should be positive definite.")
    } else TRUE
  }
)

#' @export
#' @rdname copGauss-class
copGaussToeplitz <- function(dim, param) {
  new("copGaussToeplitz",
      dim = as.integer(dim),
      param = as.numeric(param))
}

#' @exportClass copGaussExchangeable
#' @name copGaussExchangeable
#' @rdname copGauss-class
setClass(
  Class = "copGaussExchangeable",
  slots = c(
    dim = "integer",
    param = "numeric"
  ),
  validity = function(object) {
    theta <- object@param
    p <- object@dim
    k <- length(theta)
    if (!identical(k, 1L)) {
      return("Parameter vector should be of length 1.")
    } else if (theta >= 1) {
      return("Parameter should be smaller than 1.")
    } else if (theta <= -1/p) {
      return("Parameter should be larger than -1/p, with p the dimension.")
    } else if (p < 2) {
      return("The dimension should be at least 2.")
    } else TRUE
  }
)

#' @export
#' @rdname copGauss-class
copGaussExchangeble <- function(dim, param) {
  new("copGaussExchangeable",
      dim = as.integer(dim),
      param = as.numeric(param))
}


#' @exportClass copGaussCircular
#' @name copGaussCircular
#' @rdname copGauss-class
setClass(
  Class = "copGaussCircular",
  slots = c(
    dim = "integer",
    param = "numeric"
  ),
  prototype = list(
    dim = 4L,
    param = 0
  ),
  validity = function(object) {
    theta <- object@param
    p <- object@dim
    k <- length(theta)
    if (!identical(k, 1L)) {
      return("Currently, the length of the parameter vector should be equal to 1.")
    } else if (theta <= -1) {
      return("The parameter should be larger than -1.") 
    } else if (theta >= 1) {
      return("The parameter should be smaller than +1.")
    } else if (!identical(p, 4L)) {
      return("Currently, the dimension of the copula should be equal to 4.")
    } else TRUE
  }
)
#' @export
#' @rdname copGauss-class
copGaussCircular <- function(dim, param) {
  new("copGaussCircular",
      dim = as.integer(dim),
      param = as.numeric(param))
}

#' @exportClass copGauss
#' @name copGauss
#' @rdname copGauss-class
setClassUnion(
  name = "copGauss",
  members = c(
    "copGaussFull",
    "copGaussToeplitz",
    "copGaussCircular",
    "copGaussExchangeable"
  )
)