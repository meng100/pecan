# base_distribution.r
#
# Contains the base distribution classes from which specific distribution
# implementations subclass. This includes an abstract base `Distribution`
# class, and a `ProductDistribution` class that acts as a container for 
# multiple distribution objects.


#' Abstract base class for probability distributions
#'
#' \code{Distribution} is an R6 abstract base class representing multi-dimensional 
#' probability distributions. Subclasses implement specific distribution types 
#' by providing implementations of the relevant methods, most commonly
#' \code{.log_density} and \code{.sample}. This class provides input/output 
#' transformation methods to standardize representation and sampling for 
#' generic algorithms.
#'
#' Distributions are defined on spaces with arbitrary shape (scalars, vectors, arrays). 
#' Inputs and outputs can be represented in either array or flattened formats.
#' The array format is a vector or array with shape given by the \code{shape}
#' method. The flattened format transforms the array into a vector. Both formats
#' can accommodate batches of values by adding a dimension on the left. Thus,
#' a batch of \code{n} values is either a \code{c(n, shape)} array or 
#' \code{n, prod(shape)} matrix of flattened values. Distributions 
#' are implemented to be immutable; if a modified distribution with new 
#' parameters is needed, a new object should be created.
#' 
#' The abstract class cannot be instantiated directly.
#'
#' @section Public fields:
#'   \describe{
#'     \item{\code{rv_label}}{Distribution random variable name, for annotation and output.}
#'     \item{\code{rv_scalar_labels}}{Vector of labels for each scalar element of the random variable, if applicable.}
#'   }
#'
#' @section Active bindings:
#'   \describe{
#'     \item{\code{shape}}{Returns the shape of the distribution (immutable after initialization).}
#'     \item{\code{length}}{Returns the number of scalar elements in the random variable.}
#'     \item{\code{n_dims}}{Returns the number of dimensions of the array representation.}
#'     \item{\code{constraint}}{Returns constraint/type information, if present.}
#'   }
#'
#' @section Public Methods:
#'   \describe{
#'     \item{\code{initialize(shape, rv_label = NULL, rv_scalar_labels = NULL)}}{Constructs the distribution object (called from subclasses).}
#'     \item{\code{input_is_array(x)}}{Checks whether \code{x} is in array format compatible with the distribution's shape.}
#'     \item{\code{input_is_flat(x)}}{Checks whether \code{x} is a flattened vector/matrix representation.}
#'     \item{\code{transform_to_array(x, simplify = FALSE)}}{Converts input into array format. Errors if format is invalid.}
#'     \item{\code{transform_to_flat(x, simplify = FALSE)}}{Converts input into flattened format. Errors if format is invalid.}
#'     \item{\code{log_density(x)}}{Evaluates the log-density of input \code{x} (calls subclass implementation). Returns a numeric vector.}
#'     \item{\code{sample(n = 1L, flat = FALSE, simplify = FALSE)}}{Generates samples from the distribution. Output format is controlled by arguments.}
#'     \item{\code{print()}}{Prints summary information about the distribution.}
#'     \item{\code{simplify_array(x, check_type = TRUE)}}{Convenience method for converting singleton arrays to lower dimensions.}
#'   }
#'
#' @section Details:
#' - Inputs/outputs can be in array format (e.g., \code{c(n, shape)}) or flattened (e.g., matrix shape \code{(n, length)}).
#' - Input checking and reshaping utilities ease working with batch samples and named dimensions.
#' - Subclasses optionally implement methods such as \code{.log_density}, \code{.sample}, and \code{.validate_dist_params}.
#'   Methods that take values \code{x} as input should be implemented to accept the inputs in array format. They should
#'   similarly return values in this format. The abstract base class handles conversion to and from flat format.
#'   Subclasses should also set the \code{.constraint} private variable.
#'
#' @examples
#' # Abstract class -- do not instantiate directly
#' # Subclass example (for illustration, see \code{\link{Normal}} for the actual implementation):
#' Normal <- R6::R6Class(
#'   classname = "Normal"
#'   inherit = Distribution,
#'   public = list(
#'     initialize = function(mean=0, sd=1, ...) {
#'         private$.validate_dist_params(mean, sd)
#'         super$initialize(shape=1L, ...)
#'         self$mean <- mean
#'         self$sd <- sd
#'     }
#'   ),
#'   private = list(
#'     .log_density = function(x_arr) dnorm(x_arr, mean=self$mean, sd=self$sd, log=TRUE),
#'     .sample = function(n=1L) rnorm(n, mean=self$mean, sd=self$sd),
#'     .validate_dist_params = function(mean, sd) stopifnot(sd > 0)
#'   )
#' )
#' d <- NormalDistribution$new(mean=2, sd=1)
#' d$sample(5)
#' d$log_density(0)
#'
#' @seealso See subclasses implementing specific distributions 
#'   (e.g., \code{\link{NormalDistribution}}, \code{\link{ProductDistribution}}).
#'
#' @docType class
#' @name Distribution
#' @author Andrew Roberts
#' @export
Distribution <- R6Class(
  classname = "Distribution",
  public = list(
    rv_label = NULL,
    rv_scalar_labels = NULL,
    
    #' Create new distribution (do not call directly; use subclasses)
    #' @param shape Vector of dimensions of the random variable.
    #' @param rv_label Optional variable label.
    #' @param rv_scalar_labels Optional labels of scalar elements of the variable.
    initialize = function(shape, rv_label=NULL, rv_scalar_labels=NULL) {
      if (class(self)[1] == "Distribution") {
        stop("Distribution is an abstract class and cannot be instantiated directly.")
      }
      private$.shape <- shape
      self$rv_label <- rv_label
      self$rv_scalar_labels <- rv_scalar_labels # TODO: write setter for this to handle shape/validation
    },
    
    #' Check if input is in array format matching the distribution's dimensionality.
    #' @param x Object to check.
    #' @return Logical; TRUE if input has valid array shape.
    input_is_array = function(x) {
      if(!(is.vector(x) || is.array(x))) return(FALSE)
      
      if(is.vector(x)) shape_x <- length(x)
      else shape_x <- dim(x)
      
      n_dims_x <- length(shape_x)

      # Single value in array format.
      if((n_dims_x == self$n_dims) && all(shape_x == self$shape)) return(TRUE)
      
      # Multiple values in array format.
      if((n_dims_x == self$n_dims+1) && all(shape_x[-1] == self$shape)) return(TRUE)

      return(FALSE)
    },
    
    #' Check if input is a flat representation matching the distribution shape.
    #' @param x Object to check.
    #' @return Logical; TRUE if input is in valid flat format.
    input_is_flat = function(x) {
      if(!(is.vector(x) || is.array(x))) return(FALSE)
      
      if(is.vector(x)) shape_x <- length(x)
      else shape_x <- dim(x)
      
      len_x <- prod(shape_x)
      
      # Single value in flat format.
      if(is.vector(x) && (len_x == self$length)) return(TRUE)
      
      # Multiple values in flat format.
      if(is.matrix(x) && (ncol(x) == self$length)) return(TRUE)
      
      return(FALSE)
    },
    
    #' Convert input to array format matching distribution's shape.
    #' @param x Input value(s), in array or flat format.
    #' @param simplify If TRUE, singleton batches are squashed to lower dimensions.
    #' @return Array of shape matching distribution. For \code{n} values, either 
    #'  \code{c(n, shape)} just \code{shape} (if \code{n = 1} and \code{simplify = TRUE}).
    #' @note If `x` is already simple, then this function will not un-simplify,
    #'  even if \code{simplify = FALSE}.
    transform_to_array = function(x, simplify=FALSE) {
      if(self$input_is_array(x)) {
        if(simplify) private$.simplify_array(x)
        else x
      } else if(self$input_is_flat(x)) {
        private$.flat_to_array(x, simplify)
      } else {
        stop("Input `x` is not in valid array or flattened format.")
      }
    },
    
    #' Convert input to flat format representation of distribution's shape.
    #' @param x Input value(s), in array or flat format.
    #' @param simplify If TRUE, singleton batches are squashed to vector.
    #' @return For \code{n} values, either \code{(n, length)} matrix or
    #'  \code{length}-vector (if \code{n = 1} and \code{simplify = TRUE})
    #' @note If `x` is already simple, then this function will not un-simplify,
    #'  even if \code{simplify = FALSE}.
    transform_to_flat = function(x, simplify=FALSE) {
      if(self$input_is_flat(x)) {
        if(simplify) private$.simplify_flat(x)
        else x
      } else if(self$input_is_array(x)) {
        private$.array_to_flat(x, simplify)
      } else {
        stop("Input `x` is not in valid array or flattened format.")
      }
    },

    #' Evaluate log-density at points
    #' @param x Points to evaluate, in array or flat format.
    #' @return Numeric vector of log-densities, one per value.
    log_density = function(x) {
      x <- self$transform_to_array(x, simplify=FALSE)
      if(is.vector(x)) x <- matrix(x, nrow=1L)
      drop(private$.log_density(x))
    },
    
    #' Generate random samples from the distribution.
    #' @param n Number of samples.
    #' @param flat Return samples in flat format if TRUE.
    #' @param simplify Squash singleton batches to lower dimension if TRUE.
    #' @return Samples in array or flat format, according to arguments.
    sample = function(n=1L, flat=FALSE, simplify=FALSE) {
      x_arr <- private$.sample(n=n)
      
      if(flat) {
        self$transform_to_flat(x_arr, simplify)
      } else {
        if(simplify) private$.simplify_array(x_arr)
        else x_arr
      }
    }, 
    
    #' Print summary information for distribution object
    print = function() {
      cat("Distribution: ", class(self)[1])
      cat("\nShape: ", self$shape, " | Length: ", self$length)
      cat("\nConstraint: ", self$constraint)
      
      if(!is.null(self$rv_label)) cat("\nName: ", self$rv_label)
      if(!is.null(self$rv_scalar_labels)) cat("\nScalar names: ", self$scalar_names)
      cat("\n")
    }, 
    
    #' Public interface for simplifying array format
    #' @param x Array to simplify.
    #' @param check_type Validate input type before simplifying.
    #' @return If first dimension of array is one, then eliminates this dimension.
    #'  Other dimensions are not affected.
    #' @note This is the public interface to expose the functionality of the 
    #'  private method \code{.simplify_array} (which does not do type checking).
    #'  This is primarily defined so that `ProductDistribution` can access the 
    #   simplify array functionality of component distributions.
    simplify_array = function(x, check_type=TRUE) {
      if(check_type) {
        if(!self$input_is_array(x)) stop("`simplify_array(x)` requires `x` to be in array format.")
      }
      
      private$.simplify_array(x)
    }
  ),
  
  active = list(
    
    #' Distribution shape; e.g. \code{shape = 1} for scalar-valued parameter;
    #'  \code{shape = 3} for vector of length 3; \code{shape = c(3,3)} for
    #'  matrix-valued parameter. Higher-dimensional arrays are also allowed.
    shape = function(value) {
      if(missing(value)) private$.shape
      else stop("Cannot set `shape` of `Distribution` after initialization.")
    },
    
    #' Total number of scalar elements; i.e., length of the flattened vector
    #' format. This is the product of the values in \code{shape}.
    length = function(value) {
      if(missing(value)) prod(self$shape)
      else stop("Cannot set `length` of `Distribution`.")
    },
    
    #' Number of dimensions of random variable; i.e., the length of \code{shape}.
    n_dims = function(value) {
      if(missing(value)) length(self$shape)
      else stop("Cannot set `n_dims` of `Distribution`.")
    }, 
    
    #' String specifying constraints on the type of value in the support of 
    #' the distribution. "None" (not NULL) is used for unconstrained.
    constraint = function(value) {
      if(missing(value)) private$.constraint
      else stop("Cannot set `constraint` of `Distribution`.")
    }
  ),
  
  private = list(
    
    .shape = NULL, # e.g., c(1) for scalar, c(2) for vector, c(1,3,5) for 3d array.
    .constraint = NULL, # Constraint/type information. NULL = missing, "None" = unconstrained.
    
    #' Convert flattened input to array shape
    #' 
    #' The input \code{x} is required to be a vector with length equal to 
    #' \code{self$length()} or a matrix of shape \code{(n, self$length)}. In the former 
    #' case the input is converted to an array of shape \code{c(1,self$shape)}
    #' (if \code{simplify=FALSE})  or \code{self$shape} (if \code{simplify=TRUE}), and in 
    #' the latter case the input is converted to an array of shape 
    #' \code{c(n, self$shape)}. Subclasses may override this default method if
    #' specialty shaping is required. The reshaping is done in row major 
    #' order (i.e., "C" style).
    #' 
    #' @param x values in flat (vector or matrix) format.
    #' @param simplify Squashes singleton values to lower dimension if TRUE.
    #' 
    #' @return In general, returns array of shape \code{c(n, self$shape)}. 
    #' If \code{simplify=TRUE} and \code{n=1} then squashes to \code{self$shape}.
    #' 
    #' @note No input checking here; checking is done in public interface \code{transform_to_array()}.
    .flat_to_array = function(x_flat, simplify=FALSE) {
      
      # Handle single value case.
      if(is.vector(x_flat)) x_flat <- matrix(x_flat, nrow=1L)
      
      # R stores in column-major order (columns are contiguous in memory).
      # We thus transpose `x_flat` so that each value becomes a column. Values
      # are assigned to array in column-major order, then permuted afterwards
      # to maintain the convention that the first dimension is the number 
      # of values.
      n <- nrow(x_flat)
      arr <- array(t(x_flat), dim=c(self$shape, n)) # c(self$shape, n)
      arr <- aperm(arr, c(self$n_dims + 1L, seq_along(self$shape))) # c(n, self$shape)
      
      # Optionally simplify if there is only a single value.
      if(simplify) private$.simplify_array(arr)
      else arr
    },
    
    #' Convert array shape to flattened value
    #' 
    #' The input \code{x} must either be an array of shape \code{self$shape} or an 
    #' array of shape \code{c(n, self$shape)} (a batch of values). The input will 
    #' be converted to a matrix of shape \code{(n,self$length)} (where \code{n=1} in
    #' the former case). If \code{n=1} and \code{simplify=TRUE} then squashes to 
    #' \code{self$length} vector. This method exactly reverses the steps of 
    #' \code{from_flat()} to ensure consistent and reproducible conversion.
    #' 
    #' @return In general, returns matrix of shape \code{c(n, self$length)}. 
    #' If \code{simplify=TRUE} and \code{n=1} then squashes to \code{self$shape}.
    #' 
    #' @note No input checking here; checking is done in public interface \code{transform_to_flat()}.
    .array_to_flat = function(x_arr, simplify=FALSE) {
      
      # Handle the case where `x` is a single value.
      if(length(dim(x_arr)) == self$n_dims) x_arr <- array(x_arr, dim=c(1L, dim(x_arr)))
      
      n <- dim(x_arr)[1]
      x_arr <- aperm(x_arr, c(2:length(dim(x_arr)), 1L)) # c(self$shape, n)
      x_flat <- matrix(x_arr, nrow=self$length, ncol=n) # (self$length(), n)
      x_flat <- t(x_flat) # (n, self$length())
      
      # Optionally simplify if there is only a single value.
      if(simplify) private$.simplify_flat(x_flat)
      else x_flat
    },
    
    #' Simplify array batches to singleton arrays if possible.
    .simplify_array = function(x_arr) {
      if(dim(x_arr)[1] == 1L) array(x_arr, dim = dim(x_arr)[-1L])
      else x_arr
    },
    
    #' Simplify flat representation to vector if singleton batch.
    .simplify_flat = function(x_flat) {
      if(nrow(x_flat) == 1L) x_flat[1,]
      else x_flat
    },
    
    #' Abstract method; must be implemented by subclasses.
    #' @param x_arr Points to evaluate in array format.
    #' @return Numeric array of log-densities.
    .log_density = function(x_arr) {
      stop(".log_density() must be implemented by subclasses.")
    }, 
    
    #' Abstract method; must be implemented by subclasses.
    #' @param n Number of samples.
    #' @return Samples in array format.
    .sample = function(n=1L) {
      stop(".sample() must be implemented by subclasses.")
    }, 
    
    #' Abstract method; must be implemented by subclasses.
    #' Validation of distribution parameters at construction.
    .validate_dist_params = function(...) {
      stop(".validate_dist_params() must be implemented by subclasses.")
    }
    
  )
)


#' Container holding `Distribution` objects used to define a product distribution.
#' 
#' This class is effectively a wrapper around a list of `Distribution` objects.
#' `ProductDistribution` is itself a `Distribution`. Its log-density method 
#' is defined as the sum of the log-densities of its component `Distribution`
#' objects. Its `sampling` method either returns a list (one element per
#' `Distribution`) or appends the samples from each `Distribution` into 
#' a single matrix (the flat data format). Thus, the array vs flat data 
#' formats for a `Distribution` are replaced with list of array vs flat 
#' data formats for a `ProductDistribution`. For consistency, we still refer 
#' to the former as the "array" format. To convert between the two formats,
#' the ordering of the component list used to initialize the `ProductDistribution`
#' is preserved. The constructor accepts other `ProductDistribution` objects
#' in the constructor, but no nesting is done under the hood. The constructor
#' will simply extract the components and append them to the component list.
#' 
#' 
#' ProductDistribution: Cartesian product of \code{\link{Distribution}} objects
#'
#' \code{ProductDistribution} is an R6 class that represents the product (joint) of multiple
#' \code{Distribution} objects. It inherits from \code{\link{Distribution}} and provides
#' log-density and sampling methods that operate on the joint distribution.
#'
#' Internally, it is a container holding a list of \code{Distribution} objects.
#' - The \code{log_density()} method returns the sum of the log-densities of 
#'   component distributions, or optionally a matrix with component-wise log-densities.
#' - The \code{sample()} method returns either a list of arrays (one per component), 
#'   or a single matrix with all samples concatenated in flat format.
#' - The constructor accepts both \code{Distribution} and \code{ProductDistribution} 
#'   components; if a \code{ProductDistribution} is supplied, its components are 
#'   extracted to avoid nesting; i.e., the components of a \code{ProductDistribution}
#'   are always pure \code{Distribution} objects.
#' - Array vs flat formats for values are replaced by "list of arrays" vs "flat" 
#'   for product distributions.
#'
#' @section Public fields:
#'   \describe{
#'     \item{\code{components}}{List of component \code{Distribution} objects.}
#'   }
#'
#' @section Active bindings:
#'   \describe{
#'     \item{\code{n_components}}{The number of component distributions.}
#'     \item{\code{length}}{Total number of scalar entries in the combined product distribution.}
#'     \item{\code{n_dims}}{Not defined for product distribution; returns NULL.}
#'     \item{\code{shape}}{Not defined for product distribution; returns NULL.}
#'   }
#'
#' @section Public Methods:
#'   \describe{
#'     \item{\code{initialize(...)}}
#'       {Constructs the product distribution from one or more \code{Distribution} 
#'       objects (or other \code{ProductDistribution}s).}
#'     \item{\code{log_density(x, sum_components = TRUE)}}
#'       {Evaluates log-density; returns a vector (joint log-density per value) or matrix (per-component log-densities).}
#'     \item{\code{sample(n = 1L, flat = FALSE, simplify = FALSE)}}
#'       {Draws samples jointly; returns list of samples or flat concatenated matrix.}
#'     \item{\code{print(print_components = FALSE)}}
#'       {Prints a summary of the product distribution and (optionally) its components.}
#'     \item{\code{input_is_array(x)}}
#'       {Checks if \code{x} is a list of component arrays conforming to each distribution.}
#'     \item{\code{apply_component_method(method_name, args_list = NULL, ...)}}
#'       {Applies a public method to each component.}
#'     \item{\code{apply_component_field(field_name)}}
#'       {Accesses a public field from each component.}
#'   }
#'
#' @section Details:
#' - The ordering of components in the product is preserved, which is important 
#'   for consistent conversion between flat and array formats.
#' - Conversion between formats maintains consistency with the base \code{Distribution}. 
#'   class but replaces array/flat formats by list-of-arrays/flat-matrix for the joint product.
#' - The constructor does not recursively nest product distributions; the
#'   \code{components} attribute should always be a list of \code{Distribution}s.
#'
#' @examples
#' # Example: Product of normal and multivariarte normal distributions
#' d1 <- Normal$new()
#' d2 <- MultivariateNormal$new(mean=c(10, 12))
#' pd <- ProductDistribution$new(d1, d2)
#' samples <- pd$sample(5)
#' logdens <- pd$log_density(samples)
#' pd$print()
#'
#' @seealso \code{\link{Distribution}}
#'
#' @docType class
#' @name ProductDistribution
#' @author Andrew Roberts
#' @export
ProductDistribution <- R6Class(
  classname = "ProductDistribution",
  inherit = Distribution,

  public = list(
    components = NULL,

    initialize = function(...) {
      components <- list(...)
      if(length(components) == 0L) {
        stop("`ProductDistribution` cannot be initialized without components.")
      }
      
      if(!all(sapply(components, function(comp) is_distribution(comp)))) {
        stop("All `ProductDistribution` components must inherit from `Distribution`")
      }
      
      self$components <- list()
      for(comp in components) {
        if(is_product_distribution(comp)) self$components <- c(self$components, comp$components)
        else self$components <- c(self$components, list(comp))
      }

      # Product has no single shape, set to NULL.
      super$initialize(shape=NULL)
    },
    
    #' Evaluate log-density at multiple points
    #' 
    #' @param x list-of-arrays or flat format containing n input values.
    #' @param sum_components See return below.
    #' @return By default (\code{sum_components=TRUE}) n vector containing n log-density evals.
    #'  If \code{sum_components=FALSE} returns \code{(n,n_component)} matrix, where the 
    #'  log-density evaluations for each component are stacked in the rows.
    log_density = function(x, sum_components=TRUE) {
      x <- self$transform_to_array(x) # List of c(n, shp_i) arrays.
      ldens_components <- self$apply_component_method("log_density", x, "sapply") # (n,n_components)

      if(sum_components) rowSums(ldens_components)
      else ldens_components
    },

    sample = function(n=1L, flat=FALSE, simplify=FALSE) {
      # List of component samples.
      samp <- self$apply_component_method("sample", NULL, "lapply", 
                                          n=n, flat=flat, simplify=simplify)

      if(flat) {
        samp <- do.call(cbind, samp)
        if(simplify) private$.simplify_flat(samp)
        else samp
      } else {
        samp
      } 
    },
    
    print = function(print_components=FALSE) {
      str <- "ProductDistribution("
      for(i in seq_len(self$n_components)) {
        if(i != 1L) str <- paste0(str, ", ")
        shp <- paste0(self$components[[i]]$shape, collapse=",")
        str <- paste0(str, class(self$components[[i]])[1], "(", shp, ")")
      }
      str <- paste0(str, ")")
      
      str <- paste0(str, "\nNumber components: ", self$n_components, 
                    " | Length: ", self$length)
      cat(str, "\n")
      
      if(print_components) {
        for(comp in self$components) {
          cat("\n")
          comp$print()
        }
      }
    },
    
    input_is_array = function(x) {
      # TRUE if `x` is a list of component values, each of which is a valid array.
      # Note that `input_is_flat()` is inherited from `Distribution` unchanged.
      
      if(!is.list(x)) return(FALSE)
      if(length(x) != self$n_components) return(FALSE)
      
      all(self$apply_component_method("input_is_array", x, "sapply"))
    },

    #' Apply a public method to each component
    #' 
    #' Applies the method \code{method_name} to each component, where \code{args_list}
    #' is a list of length \code{n_component} containing arguments for each component
    #' method call. Only works for public methods. Additional arguments that 
    #' are constant across method calls can be forwarded via \code{...}.
    #'
    #' @param method_name The name of a public method of \code{\link{Distribution}}
    #' @param args_list list of length \code{n_components} of arguments to pass
    #'  to each method call. The argument is currently assumed to be the first
    #'  argument of the method.
    #' @param apply_func The "apply" function to use; e.g., \code{lapply} or \code{sapply}.
    #' @param ... Additional arguments to pass, fixed across all method calls.
    apply_component_method = function(method_name, args_list=NULL, apply_func="lapply", ...) {
      include_extra_args <- (length(list(...)) > 0L)
      
      # No arguments.
      if(is.null(args_list)) {
        if(include_extra_args) {
          return(match.fun(apply_func)(self$components, function(comp) comp[[method_name]](...)))
        } else {
          return(match.fun(apply_func)(self$components, function(comp) comp[[method_name]]()))
        }
      }
      
      # Has arguments.
      if(length(args_list) != self$n_components) {
        stop("`args_list` does not have length equal to number of components.")
      }
      
      if(include_extra_args) {
        match.fun(apply_func)(seq_len(self$n_components), 
                              function(i) self$components[[i]][[method_name]](args_list[[i]]))
      } else {
        match.fun(apply_func)(seq_len(self$n_components), 
                              function(i) self$components[[i]][[method_name]](args_list[[i]]), ...)
      }
    }, 
    
    
    #' Access a public attribute from each component
    #'
    #' @param field_name The name of a public field of \code{\link{Distribution}}
    #' @param apply_func The "apply" function to use; e.g., \code{lapply} or \code{sapply}.
    apply_component_field = function(field_name, apply_func="lapply") {
      # Only works for public fields.
      match.fun(apply_func)(self$components, function(comp) comp[[field_name]]) 
    }
  ), 
  
  active = list(
    n_components = function(value) {
      if(missing(value)) length(self$components)
      else stop("Cannot set `n_components` in `ProductDistribution`")
    }, 
    
    length = function(value) {
      # Total number of scalars composing the product distribution, NOT the 
      # component length. See `n_components` for the latter.
      if(missing(value)) as.integer(sum(self$apply_component_field("length", "sapply")))
      else stop("Cannot set `length` in `ProductDistribution`")
    },
    
    n_dims = function(value) {
      # No notion of `n_dims` for product distribution.
      if(missing(value)) NULL
      else stop("Cannot set `n_dims` in `ProductDistribution`")
    },
    
    shape = function(value) {
      # No notion of `shape` for product distribution.
      if(missing(value)) NULL
      else stop("Cannot set `shape` in `ProductDistribution`")
    }
  ), 
  
  private = list(
    .simplify_array = function(x_arr) {
      self$apply_component_method("simplify_array", x_arr, "lapply", check_type=FALSE)
    }, 
    
    .flat_to_array = function(x_flat, simplify=FALSE) {
      
      if(is.vector(x_flat)) x_flat <- matrix(x_flat, nrow=1L)
      
      x_arr <- vector(mode="list", length=self$n_components)
      col_idx_start <- 1L
      for(i in seq_len(self$n_components)) {
        col_idx_end <- col_idx_start + self$components[[i]]$length - 1L
        x_arr[[i]] <- self$components[[i]]$transform_to_array(x_flat[,col_idx_start:col_idx_end, drop=FALSE], simplify)
        col_idx_start <- col_idx_end + 1L
      }
      
      return(x_arr)
    }, 
    
    .array_to_flat = function(x_arr, simplify=FALSE) {
      mat_list <- self$apply_component_method("transform_to_flat", x_arr, "lapply", simplify=simplify)
      mat <- do.call(cbind, mat_list)
      
      if(simplify) super$.simplify_flat(mat)
      else mat
    }
  )
)


#' Check if an object is a \code{\link{Distribution}}
#' 
#' @param x An R object. 
#'
#' @return \code{TRUE} if \code{x} inherits from \code{Distribution}, else \code{FALSE}.
#' @seealso \code{\link{Distribution}}, \code{\link{is_product_distribution}}
#' @author Andrew Roberts
is_distribution <- function(x) {
  inherits(x, "Distribution")
}

#' Check if an object is a \code{\link{ProductDistribution}}
#' 
#' @param x An R object. 
#'
#' @return \code{TRUE} if \code{x} inherits from \code{ProductDistribution}, else \code{FALSE}.
#' @seealso \code{\link{ProductDistribution}}, \code{\link{is_distribution}}
#' @author Andrew Roberts
is_product_distribution <- function(x) {
  inherits(x, "ProductDistribution")
}


