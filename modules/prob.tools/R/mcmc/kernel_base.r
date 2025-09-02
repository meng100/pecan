# mcmc/kernel_base.r

#' Markov kernel abstract base class
#'
#' An abstract R6 class defining the interface for Markov kernels used in MCMC 
#' algorithms. A Markov kernel defines a transition mechanism via its 
#' \code{step} method, which simply outputs a new state given the current state.
#' A Markov kernel can optionally be adapted via the \code{update} method.
#'
#' Subclasses must implement the \code{step} method to advance the Markov chain,
#' and optionally the \code{update} method for adaptive behavior.
#'
#' This class should not be instantiated directly; instead, provide a function to
#' \code{make_markov_kernel()} or define a subclass.
#'
#' @section Public methods:
#' \describe{
#'   \item{\code{step(state)}}{Performs a single MCMC step, returning the next state.
#'     This method must be implemented in subclasses.}
#'   \item{\code{update(...)}}{Updates the kernel's internal state for adaptation.
#'   The default creates a non-adaptive kernel.}
#' }
#'
#' @examples
#' # Example subclass implementing a simple random-walk kernel
#' RandomWalkKernel <- R6::R6Class(
#'   classname = "RandomWalkKernel",
#'   inherit = MarkovKernel,
#'   public = list(
#'     step = function(state) state + rnorm(1),
#'     update = function(...) NULL
#'   )
#' )
#' kernel <- RandomWalkKernel$new()
#' kernel$step(0)  # Advance one step
#'
#' @seealso \code{\link{make_markov_kernel}}
#'
#' @docType class
#' @name MarkovKernel
MarkovKernel <- R6::R6Class(
  classname = "Kernel",
  public = list(
    step = function(state) stop("Abstract base MarkovKernel class cannot be instantiated."),
    update = function(...) NULL
  )
)


#' Factory class for generating user-defined Markov kernel objects from R functions
#'
#' An R6 class for generating Markov kernels from user-supplied functions.
#' This allows users to specify custom transition behavior for MCMC algorithms
#' while leveraging the standard MarkovKernel interface.
#'
#' The \code{step_fun} function should accept the current state as its argument
#' and return the next state. A Markov kernel generated using this class
#' will necessarily be stateless/non-adaptive.
#'
#' @section Public fields:
#' \describe{
#'   \item{\code{step_fun}}{A user-supplied function implementing the MCMC transition.}
#' }
#'
#' @section Public methods:
#' \describe{
#'   \item{\code{initialize(step_fun)}}{Constructs the kernel with the given transition function.}
#'   \item{\code{step(state)}}{Applies \code{step_fun} to the supplied state and returns the result.}
#' }
#'
#' @examples
#' # Random walk kernel via a user function
#' rw_fun <- function(x) x + rnorm(1)
#' kernel <- UserMarkovKernel$new(step_fun=rw_fun)
#' kernel$step(0)
#'
#' @seealso \code{\link{MarkovKernel}}, \code{\link{make_markov_kernel}}
#'
#' @docType class
#' @name UserMarkovKernel
UserMarkovKernel <- R6::R6Class(
  classname = "UserMarkovKernel",
  inherit = MarkovKernel,
  public = list(
    step_fun = NULL,
    initialize = function(step_fun) self$step_fun <- step_fun,
    step = function(state) self$step_fun(state)
  )
)


#' Create a Markov kernel object for MCMC
#'
#' Constructs a \code{MarkovKernel} R6 object from either a function or an 
#' existing \code{MarkovKernel} object. This allows both simple functions and 
#' stateful kernels to be used uniformly in MCMC algorithms.
#'
#' If a function is provided, it must take a state as input and return a new 
#' state; the function is used to define the \code{step} method of a new 
#' \code{MarkovKernel} object. The generated object will be of class
#' \code{UserMarkovKernel}. If an object of class \code{MarkovKernel} is 
#' provided, it is returned unmodified. This function is primarily for use
#' in \code{\link{run_mcmc_chain}}.
#'
#' @param obj Either a function implementing a Markov kernel step, or an existing
#'   \code{MarkovKernel} R6 object.
#'
#' @return A \code{MarkovKernel} R6 object suitable for use in MCMC routines.
#'
#' @seealso \code{\link{MarkovKernel}}, \code{\link{UserMarkovKernel}}
#'
#' @examples
#' # Example 1: Create a MarkovKernel from a simple random walk function
#' rw_step <- function(x) x + rnorm(1)
#' kernel1 <- make_markov_kernel(rw_step)
#'
#' # Example 2: Use an existing MarkovKernel object
#' kernel2 <- make_markov_kernel(kernel1)
#'
#' # Example 3: Error from an unsupported type
#' try(make_markov_kernel("not a function"))
#' @export
make_markov_kernel <- function(obj) {
  if(is.function(obj)) {
    UserMarkovKernel$new(step_fun=obj)
  } else if(is_markov_kernel(obj)) {
    obj
  } else {
    stop("Must provide a function or MarkovKernel object")
  }
}

#' Check whether an object inherits from \code{MarkovKernel}
#'
#' @param obj An R object.
#' 
#' @return \code{TRUE} if \code{obj} inherits from \code{MarkovKernel}; else \code{FALSE}.
#'
#' @seealso \code{\link{MarkovKernel}}
#' @export
is_markov_kernel <- function(obj) {
  inherits(obj, "MarkovKernel")
}




