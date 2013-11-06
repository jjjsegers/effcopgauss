#' @include copGauss-class.r
#' @include accessor.r
#' @include corrMatrix-methods.r
#' @include effScore-methods.r
NULL

#' @export
#' @name effInfMat
#' @rdname efficient-methods
setGeneric(
  name = "effInfMat",
  def = function(object) {
    standardGeneric("effInfMat")
  }
)

#' @aliases effInfMat,copGauss-method
#' @rdname efficient-methods
setMethod(
  f = "effInfMat",
  signature = "copGauss",
  definition = function(object) {
    R <- corrMatrix(object)
    l <- effScore(object)
    k <- length(param(object))
    inpr <- function(A, B) {
      .5 * sum(diag(A %*% R %*% B %*% R))
    }
    if (identical(k, 1L)) {
      I <- inpr(l, l)
    } else {
      I <- matrix(0, nrow = k, ncol = k)
      for (m1 in 1L:k) {
        for (m2 in 1L:m1) {
          I[m1, m2] <- inpr(l[,,m1], l[,,m2])
        }
      }
      for (m1 in 1L:(k-1L)) {
        for (m2 in (m1+1L):k) {
          I[m1, m2] <- I[m2, m1]
        }
      }
    }
    return(I)
  }
)
