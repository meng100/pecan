# mcmc/mcmc_kernels.r

#' A basic Metropolis-Hastings kernel for MCMC
#'
#' An R6 class implementing a random walk Metropolis-Hastings (MH) kernel, 
#' using a symmetric multivariate normal proposal. The kernel is defined 
#' by specifying a proposal covariance matrix \eqn{C}. Given the current 
#' state \eqn{x}, a new state is proposed as 
#' \deqn{\tilde{x} \sim \mathcal{N}(x, C)}. With probability 
#' \deqn{\alpha(x, \tilde{x}) = \min[1, \exp(\ell(\tilde{x} - x))]} the
#' state \eqn{\tilde{x}} is returned; otherwise \eqn{x} is returned. 
#'
#' @section Public fields:
#' \describe{
#'   \item{\code{L}}{The upper-triangular Cholesky factor of the proposal covariance matrix.}
#'   \item{\code{d}}{Dimension of the state space (\code{ncol(L)}).}
#' }
#'
#' @section Public methods:
#' \describe{
#'   \item{\code{initialize(cov)}}{Constructs the kernel with the given covariance matrix.}
#'   \item{\code{step(state)}}{Samples the new state.}
#' }
#'
#' @examples
#' # 2D example
#' cov <- matrix(c(1, 0.5, 0.5, 2), nrow = 2)
#' kernel <- MetropolisKernel$new(cov)
#' kernel$d <- 2
#' old_state <- c(0, 0)
#' new_state <- kernel$step(old_state)
#'
#' @seealso \code{\link{MarkovKernel}}
#'
#' @docType class
#' @name MetropolisKernel
MetropolisKernel <- R6::R6Class(
  classname = "MetropolisKernel",
  inherit = MarkovKernel,
  public = list(
    L = NULL, 
    d = NULL,
    initialize = function(cov) {
      self$L <- t(chol(cov))
    }, 
    
    step = function(state) {
      state + self$L %*% rnorm(self$d)
    }
  )
)