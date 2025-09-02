# distributions/multivariate_normal.r

# Constants
LOG_TWO_PI <- log(2.0 * pi)

#' Multivariate normal (\code{N(m, C)}) distribution
#'
#' \code{MultivariateNormal} is an R6 class representing a multivariate normal (Gaussian) distribution,
#' inheriting from \code{\link{Distribution}}. It supports arbitrary mean vectors and covariance matrices,
#' and computation is performed using the Cholesky decomposition for numerical efficiency.
#'
#' The class provides methods to sample from the distribution and evaluate its log-density, as well as 
#' accessors for the mean vector, covariance matrix, precision matrix, and Cholesky factor. 
#' The covariance and Cholesky factor can be provided directly; if only one is supplied, 
#' the other is computed lazily as needed (once computed, the results are cached).
#'
#' @section Public fields:
#'   \describe{
#'     \item{\code{mean}}{Mean vector of the distribution.}
#'   }
#'
#' @section Active bindings:
#'   \describe{
#'     \item{\code{cov}}{Covariance matrix \eqn{C}.}
#'     \item{\code{chol_lower}}{Lower-triangular Cholesky factor of the covariance.}
#'     \item{\code{precision}}{Precision matrix (inverse covariance).}
#'   }
#'
#' @section Public Methods:
#'   \describe{
#'     \item{\code{initialize(mean = NULL, cov = NULL, chol_lower = NULL, check_pos_def = FALSE, ...)}}
#'       {Constructor. At least one of `mean`, `cov`, or `chol_lower` must be supplied. If `cov` is provided and
#'       `check_pos_def = TRUE`, it is checked and the Cholesky factor is cached.}
#'   }
#'
#' @details
#' - The dimension \eqn{d} is inferred from any of the provided parameters.
#' - Either covariance matrix (\code{cov}) or its Cholesky factor (\code{chol_lower}) 
#'   can be provided; if both are given, an error is raised.
#' - If only the mean is provided, covariance defaults to identity.
#' - If only the covariance/Cholesky factor is provided, mean defaults to zero vector.
#' - All sampling and density evaluation is performed via the Cholesky factor.
#'
#' @examples
#' # Create a 2D standard normal
#' mvn <- MultivariateNormal$new(mean = c(0, 0), cov = diag(c(1,2)))
#' print(mvn$mean)
#' x <- mvn$sample(5)         # Draw 5 samples
#' mvn$log_density(x)  # Evaluate log-density at the 5 samples
#'
#' # Using a custom Cholesky factor
#' chol_lower <- t(chol(diag(c(1, 2))))
#' mvn2 <- MultivariateNormal$new(mean = c(1, 1), chol_lower = chol_lower)
#'
#' @seealso \code{\link{Distribution}}
#'
#' @docType class
#' @name MultivariateNormal
#' @author Andrew Roberts
#' @export
MultivariateNormal <- R6Class(
  classname = "MultivariateNormal",
  inherit = Distribution,

  public = list(
    mean = NULL,

    initialize = function(mean=NULL, cov=NULL, chol_lower=NULL, check_pos_def=FALSE, ...) {
      private$.validate_dist_params(mean, cov, chol_lower, check_pos_def)
      d <- ifelse(!is.null(mean), length(drop(mean)),
                  ifelse(!is.null(cov), dim(cov)[1], dim(chol_lower)[1]))
      
      # Mean defaults to zero and cov defaults to identity.
      if(is.null(mean)) mean <- rep(0, d)
      if(is.null(cov) && is.null(chol_lower)) chol_lower <- diag(1.0, nrow=d, ncol=d)
      
      super$initialize(shape=d, ...)
      self$mean <- mean
      private$.cov <- cov
      private$.chol_lower <- chol_lower
    }
  ), 
  
  private = list(
    .cov = NULL,
    .chol_lower = NULL, # Lower Cholesky factor.
    .precision = NULL, # Precision (inverse covariance) matrix.
    .constraint = "None",
    
    .log_density = function(x) {
      # In: (n,d) matrix
      # Out: (n,1) vector of log-density evaluations.
      
      # solve L z = y to obtain z = L^{-1} y, where y=x-mean.
      z <- backsolve(self$chol_lower, t(x-self$mean))
      
      # Quadratic term.
      quad_term <- colSums(z^2)
      
      # Log determinant term.
      logdet_term <- 2 * sum(log(diag(self$chol_lower)))
      
      -0.5 * (quad_term + logdet_term + self$shape * LOG_TWO_PI)
    },
    
    .sample = function(n=1L) {
      # Returns (n,self$shape) matrix of samples.
      Z <- matrix(rnorm(n*self$shape), nrow=self$shape)
      x <- self$mean + self$chol_lower %*% Z
      t(x)
    }, 
    
    .validate_dist_params = function(mean=NULL, cov=NULL, chol_lower=NULL, check_pos_def=FALSE) {
      # At least one of the three args must be provided in order to infer 
      # the dimension. At most one of `cov` and `chol_lower` can be provided.
      
      d_mean <- d_cov <- d_chol_lower <- NA
      
      if(all(is.null(mean), is.null(cov), is.null(chol_lower))) {
        stop("MultivariateNormal requires at least one of `mean`, `cov`, `chol_lower`.")
      }
      
      if(!is.null(cov) && !is.null(chol_lower)) {
        stop("MultivariateNormal requires at most one of `cov` and `chol_lower`.")
      }
      
      if(!is.null(mean)) {
        if(!is.vector(drop(mean))) stop("MultivariateNormal requires 1d `mean` that can be coerced to vector.")
        d_mean <- length(drop(mean))
      }
      
      if(!is.null(cov)) {
        if(!is.matrix(cov)) stop("MultivariateNormal requires `cov` to be a matrix.")
        if(nrow(cov) != ncol(cov)) stop("MultivariateNormal requires `cov` to be a square matrix.")
        if(!isSymmetric(unname(cov))) stop("MultivariateNormal requires `cov` to be a symmetric matrix.")
        
        if(check_pos_def) {
          chol_results <- is_positive_definite(cov)
          if(chol_results$is_pos_def) { # Cache Cholesky computation.
            private$.chol_lower <- t(chol_results$chol_upper)
          } else {
            stop("MultivariateNormal requires `cov` to be positive definite. Error in `chol(cov)`.")
          }
        }
        
        d_cov <- nrow(cov)
      }
      
      if(!is.null(chol_lower)) {
        if(!is.matrix(chol_lower)) stop("MultivariateNormal requires `chol_lower` to be a matrix.")
        if(nrow(chol_lower) != ncol(chol_lower)) stop("MultivariateNormal requires `chol_lower` to be a square matrix.")
        if(!is_lower_tri(chol_lower)) stop("MultivariateNormal requires `chol_lower` to be lower triangular.")
        d_chol_lower <- nrow(chol_lower)
      }
      
      # Ensure dimensions are consistent.
      dims <- as.vector(na.omit(unique(c(d_mean, d_cov, d_chol_lower))))
      if(length(dims) > 1L) {
        stop("MultivariateNormal dimension mismatch between `mean`, `cov`, `chol_lower`.")
      }
    }
  ), 
  
  active = list(
    chol_lower = function(value) {
      if(missing(value)) { # Cache Cholesky factor if not previously computed.
        if(is.null(private$.chol_lower)) private$.chol_lower <- t(chol(private$.cov))
        return(private$.chol_lower)
      } else {
        stop("MultivariateNormal cov/chol_lower are immutable.")
      }
    }, 
    
    cov = function(value) {
      if(missing(value)) { # Cache covariance if not previously computed.
        if(is.null(private$.cov)) private$.cov <- tcrossprod(self$chol_lower)
        return(private$.cov)
      } else {
        stop("MultivariateNormal cov/chol_lower are immutable.")
      }
    }, 
    
    precision = function(value) {
      if(missing(value)) { # Cache precision if not previously computed.
        if(is.null(private$.precision)) private$.precision <- chol2inv(t(self$chol_lower))
        return(private$.precision)
      } else {
        stop("MultivariateNormal precision matrix cannot be set.")
      }
    }
  )
)