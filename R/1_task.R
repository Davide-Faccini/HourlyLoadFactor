# Formatting data in list for JAGS
jags_data <- list(
  y = y,
  N = N
)

# Lists to store the results
waic_list <- list()
models_mcmc <- list()

# Given data and amount of mixture components H, it returns the samples of the chain
# Samples for H = 1 trace alpha and beta
# Samples for H > 1 trace alpha[i], beta[i] and pi[i]
fit_beta_mixture <- function(H, data, n_iter = 500, n_burnin = 500) {
  
  if (H == 1) {
    # Single Beta model
    model_string <- "
    model {
      # Likelihood
      for (i in 1:N) {
        y[i] ~ dbeta(alpha, beta)
      }
      
      # Priors (weakly informative)
      alpha ~ dgamma(0.1, 0.1)
      beta ~ dgamma(0.1, 0.1)
    }
    "
    params_to_monitor <- c("alpha", "beta")
    
  } else {
    # Mixture of H Betas
    model_string <- "
    model {
      # Likelihood
      for (i in 1:N) {
        y[i] ~ dbeta(alpha[z[i]], beta[z[i]])
        z[i] ~ dcat(pi[1:H])
      }
      
      # Priors for mixing weights
      pi[1:H] ~ ddirich(rep(1, H))
      
      # Priors for Beta parameters using mu and precision
      for (h in 1:H) {
        mu_raw[h] ~ dunif(0, 1)
        kappa[h] ~ dgamma(0.1, 0.1)
      }
      
      # Sort mu_raw to ensure mu[1] < mu[2] < ... < mu[H] and prevent label switching
      mu[1:H] <- sort(mu_raw)
      
      # Conversion to shape parameters alpha and beta
      for (h in 1:H) {
        alpha[h] <- mu[h] * kappa[h]
        beta[h] <- (1 - mu[h]) * kappa[h]
      }
    }
    "
    
    # Update data list to include H
    data$H <- H
    params_to_monitor <- c("alpha", "beta", "pi")
  }
  
  # Write model to temporary file for JAGS
  model_file <- tempfile()
  writeLines(model_string, con = model_file)
  
  # Initialize and run JAGS
  cat(paste("\nFitting model with H =", H, "...\n"))
  jags_model <- jags.model(file = model_file, data = data, n.chains = 2, n.adapt = n_burnin, quiet = TRUE)
  
  # Burn-in
  update(jags_model, n_burnin)
  
  # Sample
  samples <- coda.samples(jags_model, variable.names = params_to_monitor, n.iter = n_iter)
  
  return(samples)
}

# Run models for H = 1, 2, 3, 4, 5
for (H in 1:5) {
  # Global parameter setting (or fallback in case they are missing)
  n_it <- if(exists("GLOBAL_N_ITER")) GLOBAL_N_ITER else 500
  n_bu <- if(exists("GLOBAL_N_BURNIN")) GLOBAL_N_BURNIN else 500
  
  # Generate and store the samples
  samples <- fit_beta_mixture(H, jags_data, n_iter = n_it, n_burnin = n_bu)
  models_mcmc[[H]] <- samples
  
  
  samples_mat <- as.matrix(samples)               # Transform samples list to matrix
  S <- nrow(samples_mat)                          # S = #samples
  log_lik_mat <- matrix(0, nrow = S, ncol = N)
  
  # Computing the WAIC values
  for (s in 1:S) { #Iterating over all samples
    
    if (H == 1) {
      # Log-likelihood of the single beta at sample s for all observations (N)
      log_lik_mat[s, ] <- dbeta(y, samples_mat[s, "alpha"], samples_mat[s, "beta"], log = TRUE)
      
    } else {
      # For H > 1 take the likelihoods of the individual components and compute
      # the weighted mixture distribution likelihood
      lik_i <- rep(0, N)
      for (h in 1:H) {
        pi_h <- samples_mat[s, paste0("pi[", h, "]")]
        alpha_h <- samples_mat[s, paste0("alpha[", h, "]")]
        beta_h <- samples_mat[s, paste0("beta[", h, "]")]
        lik_i <- lik_i + pi_h * dbeta(y, alpha_h, beta_h, log = FALSE)
      }
      #Take the logarithm of the complete likelihood of sample s 
      log_lik_mat[s, ] <- log(lik_i)
    }
  }
  
  # Calculate WAIC using loo package
  waic_res <- waic(log_lik_mat)
  waic_list[[H]] <- waic_res
  
  cat(sprintf("WAIC for H = %d: %.2f\n", H, waic_res$estimates["waic", "Estimate"]))
}

# Select best model based on WAIC
waic_values <- sapply(waic_list, function(w) w$estimates["waic", "Estimate"])
best_H <- which.min(waic_values)

cat(sprintf("\nBest model selected by WAIC has H = %d components (WAIC = %.2f).\n", best_H, waic_values[best_H]))

# Save results for subsequent tasks
best_model <- best_H
mcmc_samples <- models_mcmc[[best_H]]
