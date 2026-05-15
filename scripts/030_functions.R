###########################################################################
# User Defined Functions
###########################################################################


### Optimisation functions

# ____________________________
#
## - Given a seq (vector of change points) get covar
# ____________________________

fun_get_covar <- function(given_seq){
  
  z_seq_l <- length(given_seq)
  
  # widen COVAR beyond N to cope with following
  # https://kingaa.github.io/pomp/FAQ.html#extrapolation-warnings
  # can do if needed
  given_seq[1] <- 0
  given_seq[z_seq_l] <- given_seq[z_seq_l] + 1
  
  # Define the step function
  step_function <- stepfun(given_seq[-z_seq_l], 1:z_seq_l - 1)
  
  # Generate the 'epoch' vector
  epoch <- step_function(min(given_seq):max(given_seq))
  
  # Create the data frame
  df_covar <- data.frame(day=min(given_seq):max(given_seq), epoch=epoch)
  
  # Return covar
  return(df_covar)
}



# ____________________________
#
## -  custom slice design optimisation
# ____________________________


fun_slice_design_beta <- function(fn, lower, upper, 
                                  tol = 1e-3, n_slices = 11, maxit = 200, verbose = FALSE, ...) {
  i <- 0
  while ((abs(upper - lower) > tol) & (i < maxit)) {
    interval <- seq(lower, upper, length.out = n_slices)
    f_values <- sapply(interval, fn, ...)
    
    f_values[is.nan(f_values) | is.infinite(f_values)] <- 1e+05
    min_index <- which.min(f_values)
    
    if (min_index == 1) {
      lower <- interval[min_index]
      upper <- interval[min_index + 1]
    } else if (min_index == length(interval)) {
      lower <- interval[min_index - 1]
      upper <- interval[min_index]
    } else {
      lower <- interval[min_index - 1]
      upper <- interval[min_index + 1]
    }
    
    i <- i + 1
    if (verbose) cat("Iteration", i, ": min_index =", min_index, "\n")
  }
  
  return(list(par = (lower + upper) / 2, value = min(f_values), iterations = i))
}


# ____________________________
#
## -  optim caller
# ____________________________

# Calls the slide design for each epoch (each beta)

fun_slice_design_beta_caller <- function(pomp_model,
                                         lower = 0.01, upper = 4, z_seq, ...) {
  local_theta <- coef(pomp_model)
  
  f_df_covar <- fun_get_covar(given_seq = z_seq)
  f_pomp_model <- pomp_model %>%
    pomp(covar = covariate_table(f_df_covar, order = "constant", times = "day"))
  
  z_seq_l <- length(z_seq)
  out_betas <- out_values <- out_itts <- NULL
  
  for (i in 1:(z_seq_l - 1)) {
    local_theta["epoch_test"] <- i
    beta_name <- paste0("beta", i)
    
    f_per_epoch <- f_pomp_model %>%
      traj_objfun(est = beta_name, params = local_theta)
    
    out <- fun_slice_design_beta(fn = f_per_epoch, lower = lower, upper = upper, ...)
    
    out_betas <- c(out_betas, out$par)
    out_values <- c(out_values, out$value)
    out_itts <- c(out_itts, out$iterations)
    
    local_theta <- coef(f_per_epoch)
  }
  
  last_ll <- sum(out_values)
  
  return(list(
    traj_object = f_per_epoch,
    betas = out_betas,
    values = out_values,
    iterations = out_itts,
    last_ll = last_ll
  ))
}



# Orchestrate several seqs using parallel ops

# ____________________________
#
## -  optim orchestration - max (desktop, MS)
# ____________________________

# max one gives both items (full traj and info)

fun_max_optim_beta_multi_runner <- function(df_seqs, pomp_model, df_data, verbose = FALSE,...) {
  
  # If df_seqs is a sequence, convert to data frame
  if (!is.data.frame(df_seqs)) {
    df_seqs <- data.frame(run = 0)
    df_seqs$seq <- list(df_seqs)
  }
  
  z_l <- max(df_seqs$seq[[1]])
  n_candidates <- length(df_seqs$run)
  df_runs <- NULL
  df_seqs$beta <- 0
  df_seqs$ll <- 0
  df_seqs$last_ll <- 0
  
  for (i in 1:n_candidates) {
    df_covar <- fun_get_covar(given_seq = df_seqs$seq[[i]])
    
    df_data_local <- df_data %>%
      left_join(df_covar, by = join_by(day))
    
    out_optim <- fun_slice_design_beta_caller(
      pomp_model = pomp_model,
      z_seq = df_seqs$seq[[i]],
      #df_data = df_data_local,
      ...
    )
    
    df_seqs$beta[i] <- list(out_optim$betas)
    df_seqs$ll[i] <- list(out_optim$values)
    df_seqs$last_ll[i] <- out_optim$last_ll
    
    z_index <- rep(0, z_l)
    z_index[df_seqs$seq[[i]]] <- 1
    
    # have to assume df_covar has 0 and z_l + 1 so index better
    df_covar <- df_covar[df_covar$day %in% 1:z_l, ]
    df_covar$is_changepoint <- z_index
    
    df_covar$traj <- out_optim$traj_object %>%
      trajectory(format = "data.frame") %>%
      mutate(traj = C * coef(pomp_model)[["rf"]]) %>%
      pull(traj)
    
    df_covar$run <- df_seqs$run[i]
    df_runs <- rbind(df_runs, df_covar)
    
    if (verbose) cat("Iteration", i, ": seq =", df_seqs$seq[[i]], "\n")
  }
  
  df_seqs$ll_rank <- rank(df_seqs$last_ll)
  
  return(list(df_covar_traj = df_runs, df_info = df_seqs))
}


# ____________________________
#
## -  optim orchestration - min (Posit server) 
# ____________________________

# min one gives both items (parallel) ...fast

fun_min_optim_beta_multi_runner <- function(df_seqs, pomp_model, df_data, ...) {
  
  plan(multisession)
  
  if (!is.data.frame(df_seqs)) {
    df_seqs <- data.frame(run = 0)
    df_seqs$seq <- list(df_seqs)
  }
  
  z_l <- max(df_seqs$seq[[1]])
  
  n_candidates <- length(df_seqs$run)
  
  df_seqs$beta <- 0
  df_seqs$ll <- 0
  df_seqs$last_ll <- 0
  
  results <- future_lapply(1:n_candidates, function(i) {
    z_seq_i <- df_seqs$seq[[i]]
    df_covar <- fun_get_covar(given_seq = z_seq_i)
    
    df_data_local <- df_data %>%
      left_join(df_covar, by = join_by(day))
    
    out_optim <- fun_slice_design_beta_caller(
      pomp_model = pomp_model,
      z_seq = z_seq_i,
      #df_data = df_data_local,
      ...
    )
    
    z_index <- rep(0, z_l)
    z_index[z_seq_i] <- 1
    
    # have to assume df_covar has 0 and z_l + 1 so index better
    df_covar <- df_covar[df_covar$day %in% 1:z_l, ]
    df_covar$is_changepoint <- z_index
    
    df_covar$traj <- out_optim$traj_object %>%
      trajectory(format = "data.frame") %>%
      mutate(traj = C * coef(pomp_model)[["rf"]]) %>%
      pull(traj)
    
    df_covar$run <- df_seqs$run[i]
    
    list(
      covar_traj = df_covar,
      beta = out_optim$betas,
      ll = out_optim$values,
      last_ll = out_optim$last_ll
    )
  })
  
  # Unpack results
  df_runs <- do.call(rbind, lapply(results, `[[`, "covar_traj"))
  df_seqs$beta <- lapply(results, `[[`, "beta")
  df_seqs$ll <- lapply(results, `[[`, "ll")
  df_seqs$last_ll <- sapply(results, `[[`, "last_ll")
  df_seqs$ll_rank <- rank(df_seqs$last_ll)
  
  #return(list(df_info = df_seqs))
  return(list(df_covar_traj = df_runs, df_info = df_seqs))
}


