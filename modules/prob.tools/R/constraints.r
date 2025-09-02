# constraints.r
# 
# Forward/inverse transformations for converting constrained parameters
# to unconstrained space and vice versa.

logit_map <- function(par, a=0, b=1) {
  # A log-odds (logit) transform that is translated to have domain (a,b).
  # This can be derived as the composition of the translation 
  # phi(x) = (x-a)/(b-a) with the usual logit transform, which accepts 
  # values in (0,1). If `par` is a vector of length > 1, then the transformation
  # is applied elementwise. The function is NOT vectorized over the arguments
  # `a` and `b`; these are scalar bounds.
  
  if(length(a) != 1L) {
    stop("`a` must be a scalar.")
  }
  
  if(length(b) != 1L) {
    stop("`b` must be a scalar.")
  }
  
  if(a >= b) {
    stop("`a` < `b` must hold.")
  }
  
  log((par - a) / (b - par))
}

inv_logit_map <- function(phi, a=0, b=1) {
  # The inverse map of `logit_map`. See comments for this function for 
  # details. 
  
  if(length(a) != 1L) {
    stop("`a` must be a scalar.")
  }
  
  if(length(b) != 1L) {
    stop("`b` must be a scalar.")
  }
  
  if(a >= b) {
    stop("`a` < `b` must hold.")
  }
  
  # This creates a deep copy for all supported input types for `phi` (vector,
  # matrix).
  inverse_logit <- phi
  
  # For positive values, use form that avoids numerical overflow.
  sel_geq_0 <- (phi >= 0)
  inverse_logit[sel_geq_0] <- 1/(1 + exp(-phi[sel_geq_0]))
  
  # For negative values, use form that avoids numerical overflow. When 
  # input is very small, e^x/(1+e^x) is approx e^x.
  sel_l_0 <- !sel_geq_0
  inverse_logit[sel_l_0] <- exp(phi[sel_l_0]) / (1 + exp(phi[sel_l_0]))
  
  # Map to (a,b).
  par <- a + inverse_logit * (b-a)
  attr(par, "log_det_J") <- log(b-a) + log(inverse_logit) + log(1-inverse_logit)
  
  return(par)
}


log_lower_map <- function(par, a=0) {
  if(length(a) != 1) {
    stop("`a` must be a scalar.")
  }
  
  log(par-a)
}


inv_log_lower_map <- function(phi, a=0) {
  if(length(a) != 1L) {
    stop("`a` must be a scalar.")
  }
  
  par <- a + exp(phi)
  attr(par, "log_det_J") <- phi
  
  return(par)
}


log_upper_map <- function(par, b) {
  if(length(b) != 1L) {
    stop("`b` must be a scalar.")
  }
  
  log(b-par)
}


inv_log_upper_map <- function(phi, b) {
  if(length(b) != 1L) {
    stop("`b` must be a scalar.")
  }
  
  par <- b - exp(phi)
  attr(par, "log_det_J") <- phi
  
  return(par)
}


simplex_map <- function(par) {
  # Implements the map used by Stan for unit simplex-valued parameters.
  # Vectorized so that `par` can be a matrix with one row per input. 
  # Since the d-simplex can be represented by d-1 values (due to the 
  # sum-to-one constraint), this map ignores the final value and thus maps 
  # to R^{d-1}, one dimension lower. The inverse map (see `inv_simplex_map()`)
  # accepts values in R^{d-1} and maps back to R^d. Thus, for an input 
  # `par` of dimension (num_params, d), returns a matrix of dimension 
  # (num_params, d-1). The ith row contains the d-1 unconstrained variables 
  # constructed by applying the map to the ith row of `par`.
  
  if(is.null(dim(par))) par <- matrix(par, nrow=1L)
  d <- ncol(par)
  
  #
  # Map `par` to intermediate variables `z`.
  #
  
  # If 2-simplex, then there is only one intermediate variable z1, and it
  # equals par1.
  if(d==2L) {
    z <- par[,1L, drop=FALSE]
  } else {
    par <- par[,1:(d-1), drop=FALSE]
    
    # Cumulative sums of each row of the matrix, excluding the final dimension.
    # Row i becomes par1_i, par1_i + par2_i, ..., (par1_i+...+par[d-1]_i).
    lens <- cbind(rep(1,nrow(par)), 
                  1-matrixStats::rowCumsums(par[,1:(d-2),drop=FALSE]))
    
    z <- par/lens
  }
  
  #
  # Map `z` to unconstrained variables `phi`.
  #
  
  # In some cases, some of the later components account for almost none of the
  # stick length. In these cases, due to numerical rounding, some of the 
  # z variables can be slightly above 1. We round these down to a number 
  # slightly less than 1.
  z[z >= 1] <- 1 - .Machine$double.eps
  phi <- logit_map(z)
  shift <- log(d-seq(1,d-1))
  add_vec_to_mat_rows(shift, phi)
}


inv_simplex_map <- function(phi) {
  # The inverse map to `simplex_map()`, including computation of the 
  # Jacobian determinant. As discussed in the comments in `simplex_map()`, 
  # for a d-simplex, this forward map maps to d-1 unconstrained variables 
  # `phi`. This function takes these d-1 variables and maps back to d variables
  # lying on the d-simplex. The function is vectorized so that `phi` can be 
  # a matrix of shape (num_params, d-1). Returns matrix of shape 
  # (num_params, d). The returned matrix has attribute `det_J`, which is a
  # vector of length num_params storing the absolute determinant of the 
  # Jacobian evaluated at each input parameter in `phi`.
  
  if(is.null(dim(phi))) phi <- matrix(phi, nrow=1L)
  d <- ncol(phi) + 1L
  
  # Map to intermediate variables.
  shift <- -log(d-seq(1,d-1))
  z <- inv_logit_map(add_vec_to_mat_rows(shift, phi))
  
  # Map to simplex.
  par <- matrix(nrow=nrow(phi), ncol=d)
  g <- matrix(nrow=nrow(phi), ncol=d-1)
  par[,1] <- z[,1]
  g[,1] <- 1
  lens <- 1 - par[,1]
  
  for(j in seq(2,d-1)) {
    par[,j] <- lens * z[,j]
    g[,j] <- par[,j] * (1-z[,j])
    lens <- lens - par[,j]
  }
  
  # Construct final variable.
  par[,d] <- 1 - rowSums(par[,1:(d-1), drop=FALSE])
  
  # Construct vector of absolute Jacobian determinants.
  attr(par, "log_det_J") <- rowSums(log(g))
  
  return(par)
}


id_map <- function(par) {
  # Identity map.
  
  if(is.null(dim(par))) par <- matrix(par, nrow=1L)
  par
}

inv_id_map <- function(phi) {
  # Identity map. 
  
  if(is.null(dim(phi))) phi <- matrix(phi, nrow=1L)
  attr(phi, "log_det_J") <- rep(0, nrow(phi))
  return(phi)
}
