# mcmc/mcmc_base.r

run_mcmc_chain <- function(init_state, kernel, n_iter, adapt=TRUE) {
  # normalize kernel: wrap function into MarkovKernel if needed
  kernel <- make_kernel(kernel)
  
  # storage
  chain <- vector("list", n_iter)
  state <- init_state
  
  for (i in seq_len(n_iter)) {
    # propose new state
    state <- kernel$step(state)
    chain[[i]] <- state
    
    # allow adaptation, e.g. update proposal covariance
    if (adapt) {
      kernel$update(state = state, iter = i)
    }
  }
  
  return(chain)
}
