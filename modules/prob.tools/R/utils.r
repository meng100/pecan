# Probabilistic modeling utility/helper functions.

#' Check if a matrix is positive definite, and return Cholesky factor
#'
#' Defines "positive definite" in the numerical sense of being able to run 
#' \code{chol(m)} without an error. To avoid wasting this computation, returns 
#' the result of \code{chol(m)} (the upper Cholesky factor) in addition to the logical.
#' 
#' @param m A matrix 
#'
#' @return A \code{list} with names \code{is_pos_def} and \code{chol_upper}.
#' @author Andrew Roberts
is_positive_definite <- function(m) {
  out <- tryCatch({
    chol_upper <- chol(m)
    list(is_pos_def=TRUE, chol_upper=chol_upper)
  }, error = function(e) {
    list(is_pos_def=FALSE, chol_upper=NULL)
  })
  return(out)
}

#' Check if a matrix is lower triangular
#'
#' A lower triangular matrix is one in which all entries strictly above the 
#' main diagonal are zero. Note that this function will run for non-square 
#' matrices, but it is primarily designed for square matrices.
#' 
#' @param m A matrix 
#'
#' @return \code{TRUE} if \code{m} is lower triangular, else \code{FALSE}.
#' @seealso \code{\link{is_upper_tri}}
#' @author Andrew Roberts
is_lower_tri <- function(m) {
  all(m[upper.tri(m)] == 0)
}

#' Check if a matrix is upper triangular
#'
#' A upper triangular matrix is one in which all entries strictly above the 
#' main diagonal are zero. Note that this function will run for non-square 
#' matrices, but it is primarily designed for square matrices.
#' 
#' @param m A matrix 
#'
#' @return \code{TRUE} if \code{m} is upper triangular, else \code{FALSE}.
#' @seealso \code{\link{is_upper_tri}}
#' @author Andrew Roberts
is_upper_tri <- function(m) {
  all(m[lower.tri(m)] == 0)
}
