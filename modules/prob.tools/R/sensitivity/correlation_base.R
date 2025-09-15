#' Correlation-based sensitivity 
#'
#' @param settings  PEcAn settings object (used by sim_and_aggre)
#' @param all_params data.frame; rows = ensemble members, cols = parameter names
#' @param sim_and_aggre function(U, settings) -> data.frame with columns: run_id 
#'        and one column per variable used in PRCC
#' @param selector function(U, Y_wide, already_fixed) -> list(next_param, scores)
#'        Default uses PRCC (rank=TRUE) and picks the smallest mean |PRCC|.
#' @param vars    character vector of output variable names to analyze (must be columns in the
#'        returned data from simulate_and_aggregate).
#' @param n_fixed_max integer; maximum number of parameters to select
#' @param init_fixed character; parameters considered already fixed
#' @param verbose logical; whether to print intime progress in log
#' @return list(order = character vector of parameter names in selection order,
#'              history = per-iter scores)
#' @export
run_sensi_correlation <- function(
    settings,
    all_params,
    sim_and_aggre,
    selector = prcc_selector,
    n_fixed_max = 25L,
    init_fixed = character(0),
    verbose = TRUE
) {
  stopifnot(is.list(settings),
            is.data.frame(all_params), nrow(all_params) > 0,
            is.function(sim_and_aggre),
            is.function(selector))
  
  fixed_pars <- unique(init_fixed[init_fixed %in% names(all_params)])
  max_iters  <- min(n_fixed_max, ncol(all_params))
  history    <- list()
  
  for (iter in 0:max_iters) {
    varied <- setdiff(names(all_params), fixed_pars)
    if (length(varied)==0) break
    
    # Build U: vary `varied`, fix current fixed_pars at ensemble medians
    var_df <- all_params[, varied, drop = FALSE]
    if (length(fixed_pars)>0) {
      fixed_vals <- apply(all_params[, fixed_pars, drop = FALSE], 2, stats::median)
      fixed_df <- as.data.frame(
        matrix(rep(fixed_vals, each = nrow(var_df)),
               nrow = nrow(var_df), byrow = FALSE,
               dimnames = list(NULL, names(fixed_vals)))
      )
      U <- cbind(var_df, fixed_df)
    } else {
      U <- var_df
    }
    # Format model output through user-defined arg, 
    # our default: runs fwd + obs mapping + aggregation → wide scores 
    Y_wide <- sim_and_aggre(U = U, setting = settings)
    
    if (nrow(Y_wide) != nrow(U))
      stop("Row count mismatch between U and Y_wide. Need for correlation.")
    
    # Choose next parameter to fix using selector: default is PRCC
    sel <- selector(U = U, Y_wide = Y_wide, already_fixed = fixed_pars)
    next_par <- sel$next_param
    
    if (verbose) message(sprintf("[iter %d] fix: %s", iter, next_par))
    if (is.na(next_par) || next_par %in% fixed_pars) break
    
    fixed_pars <- c(fixed_pars, next_par)
    history[[sprintf("iter_%02d", iter)]] <- list(chosen = next_par, scores = sel$scores)
  }
  
  list(order = fixed_pars, history = history)
}

# Default selector: PRCC with rank=TRUE, pick smallest mean |PRCC|

#' PRCC-based parameter selector
#' @param U data.frame of parameters used for this iteration: rows = ensemble members
#' @param Y_wide data.frame with 'run_id' + one column per variable
#' @param run_ids character vector aligned to rows of U
#' @param already_fixed character vector of parameters already fixed
#' @param rank logical; rank-based PRCC default = TRUE
#' @return list(next_param=character(1), scores=data.frame(parameter, mean_abs_prcc))
prcc_selector <- function(U, Y_wide, run_ids, already_fixed = character(0), rank = TRUE) {
  if (!requireNamespace("sensitivity", quietly = TRUE))
    stop("Package 'sensitivity' is required for PRCC selector.")
  
  # Alignment by run_id
  U2 <- U
  U2$run_id <- run_ids
  merged <- merge(U2, Y_wide, by = "run_id")
  
  vars <- setdiff(colnames(Y_wide), "run_id")
  X <- merged[, colnames(U), drop = FALSE]
  
  # Compute PRCC per variable
  pcc_list <- lapply(vars, function(v) {
    y <- merged[[v]]
    sensitivity::pcc(X, y, rank = rank)
  })
  
  prcc_mat <- do.call(cbind, lapply(pcc_list, `[[`, "PRCC"))
  rownames(prcc_mat) <- colnames(X)
  colnames(prcc_mat) <- vars
  
  # Pick next to fix: least average absolute PRCC across variables
  score <- rowMeans(abs(prcc_mat))
  score_df <- data.frame(parameter = names(score), mean_abs_prcc = as.numeric(score))
  score_df <- score_df[order(score_df$mean_abs_prcc, decreasing = FALSE), ]
  
  remaining <- setdiff(score_df$parameter, already_fixed)
  next_param <- if (length(remaining)) remaining[[1]] else NA_character_
  
  list(next_param = next_param, scores = score_df)
}


# TODO: 
# 1. provide default for sim_and_aggre()

sim_and_aggre <- function(){
  
}