# distributions/normal.r

#' Univariate normal (Gaussian) distribution
#'
#' The \code{Normal} class represents a univariate normal (Gaussian) distribution
#' with user-specified mean and standard deviation, and inherits from \code{\link{Distribution}}.
#'
#' @section Public fields:
#' \describe{
#'   \item{\code{mean}}{Mean of the distribution.}
#'   \item{\code{sd}}{Standard deviation of the distribution.}
#' }
#'
#' @section Public Methods:
#' \describe{
#'   \item{\code{initialize(mean = 0, sd = 1, ...)}}{
#'     Constructs a normal distribution with specified mean and standard deviation.
#'     The mean and standard deviation must be numeric scalars, and \code{sd}
#'     must be positive. Additional arguments are forwarded to the superclass constructor.
#'   }
#' }
#'
#' @details
#' The log-density is computed using \code{dnorm(x, mean, sd, log = TRUE)}. Samples are drawn
#' using \code{rnorm}. The distribution is always scalar (\code{shape = 1}).
#' Parameter checks are performed during initialization.
#'
#' @examples
#' # Standard normal distribution
#' norm <- Normal$new()
#' norm$sample(5)
#' norm$log_density(0)
#'
#' # Non-standard normal distribution
#' norm2 <- Normal$new(mean = 10, sd = 2)
#' norm2$sample(3)
#' norm2$log_density(-1)
#'
#' @seealso \code{\link{Distribution}}
#'
#' @docType class
#' @name Normal
#' @author Andrew Roberts
#' @export
Normal <- R6Class(
  classname = "Normal",
  inherit = Distribution,
  
  public = list(
    mean = NULL,
    sd = NULL,
    
    initialize = function(mean=0, sd=1, ...) {
      private$.validate_dist_params(mean, sd)
      super$initialize(shape=1L, ...)
      self$mean <- mean
      self$sd <- sd
    }
  ), 
  
  private = list(
    .constraint = "None",
    
    .log_density = function(x_arr) {
      dnorm(x_arr, mean=self$mean, sd=self$sd, log=TRUE)
    },
    
    .sample = function(n=1L) {
      matrix(rnorm(n, mean=self$mean, sd=self$sd), ncol=1L)
    }, 
    
    .validate_dist_params = function(mean, sd) {
      if(length(mean) != 1L) stop("`NormalDistribution` requires length 1 `mean`.")
      if(length(sd) != 1L) stop("`NormalDistribution` requires length 1 `sd`.")
      if(sd < 0) stop("`NormalDistribution` requires positive value for `sd`.")
    }
  )
)
