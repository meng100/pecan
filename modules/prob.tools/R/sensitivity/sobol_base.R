#' Sobol-Jansen total-order sensitivity 
#'
#' @param settings  PEcAn settings object (used by sim_and_aggre)
#' @param X1        data.frame/matrix (n x p) first Sobol base sample.
#' @param X2        data.frame/matrix (n x p) second Sobol base sample.
#' @param sim_and_aggre function(U, settings) -> data.frame with
#'        columns: run_id, and one scalar column per output variable (e.g., NEE, LAI, ...).
#'        Must return exactly nrow(U) rows in the same order as U/run_ids.
#' @param vars      character vector of output variable names to analyze (must be columns in the
#'        returned data from simulate_and_aggregate).
#' @param compute_first Logical; if TRUE, also return first-order indices
#' @return list: matrix[p params x |vars|],  rownames=colnames(X1), colnames=vars, value=indices
#' @export
run_sensi_sobol <- function(
    settings,
    X1,
    X2,
    sim_and_aggre,
    vars,
    compute_first = FALSE
) {
  stopifnot(is.list(settings),
            is.matrix(X1) || is.data.frame(X1),
            is.matrix(X2) || is.data.frame(X2),
            nrow(X1) == nrow(X2),
            ncol(X1) == ncol(X2),
            is.function(simulate_and_aggregate),
            length(vars) >= 1)
  
  sj <- soboljansen(model = NULL, X1 = as.data.frame(X1), X2 = as.data.frame(X2))
  U  <- sj$X                               # (2n + p*n) x p evaluation matrix
  run_ids <- sprintf("ens_%d", seq_len(nrow(U)))
  
  # Run model and reformat
  Y_wide <- sim_and_aggre(U = as.data.frame(U), settings = settings)
  
  if (!("run_id" %in% names(Y_wide)))
    stop("sim_and_aggre must return a data.frame with a 'run_id' column.")
  # Keep original order as expected by soboljansen
  Y_wide <- Y_wide[match(run_ids, Y_wide$run_id), , drop = FALSE]
  if (nrow(Y_wide) != nrow(U))
    stop("Row count mismatch: returned ", nrow(Y_wide), " rows, expected ", nrow(U), ".")
  
  # Compute indices per output variable
  p <- ncol(X1)
  total_mat <- matrix(NA_real_, nrow = p, ncol = length(vars),
                      dimnames = list(colnames(X1), vars))
  first_mat <- if (compute_first) matrix(NA_real_, nrow = p, ncol = length(vars),
                                         dimnames = list(colnames(X1), vars)) else NULL
  sobol_objs <- vector("list", length(vars)); names(sobol_objs) <- vars
  
  for (i in seq_along(vars)) {
    v <- vars[i]
    if (!(v %in% names(Y_wide))) stop("Variable '", v, "' not found in sim_and_aggre output.")
    y <- Y_wide[[v]]
    sji <- soboljansen(model = NULL, X1 = as.data.frame(X1), X2 = as.data.frame(X2))
    sji <- tell(sji, y)
    
    # Total-order 
    total_mat[, i] <- sji$T$original
    
    # First-order
    if (compute_first && !is.null(sji$S)) {
      first_vals <- if (!is.null(sji$S$original)) sji$S$original else sji$S$`bias corrected`
      first_mat[, i] <- first_vals
    }
    sobol_objs[[i]] <- sji
  }
  
  list(total = total_mat, first = first_mat, sobol_objs = sobol_objs)
}
