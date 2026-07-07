# ==============================================================================
# TASK 1: Beta Mixture Models and WAIC Selection
# ==============================================================================

# JAGS requires data in a list format
jags_data <- list(
  y = y,
  N = N
)

# Lists to store the results
waic_list <- list()
models_mcmc <- list()

# Function to run JAGS model for a given H
# Riduco temporaneamente le iterazioni per il loop di selezione per abbattere i tempi
fit_beta_mixture <- function(H, data, n_iter = 500, n_burnin = 500) {
  
  if (H == 1) {
    # Single Beta model
    model_string <- "
    model {
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
    # Mixture of H Betas with ordering constraint on means to prevent label switching
    model_string <- paste0("
    model {
      for (i in 1:N) {
        y[i] ~ dbeta(alpha[z[i]], beta[z[i]])
        z[i] ~ dcat(pi[1:H])
      }
      
      # Priors for mixing weights
      pi[1:H] ~ ddirich(rep(1, H))
      
      # Priors for Beta parameters using mu and precision (kappa) 
      # with ordering on mu to prevent label switching
      for (h in 1:H) {
        mu_raw[h] ~ dunif(0, 1)
        kappa[h] ~ dgamma(0.1, 0.1)
      }
      
      # Sort mu_raw to ensure mu[1] < mu[2] < ... < mu[H]
      mu[1:H] <- sort(mu_raw)
      
      for (h in 1:H) {
        alpha[h] <- mu[h] * kappa[h]
        beta[h] <- (1 - mu[h]) * kappa[h]
      }
    }
    ")
    
    # Update data list to include H
    data$H <- H
    params_to_monitor <- c("alpha", "beta", "pi")
  }
  
  # Write model to temporary file
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
  samples <- fit_beta_mixture(H, jags_data)
  models_mcmc[[H]] <- samples
  
  # Extract and compute log_lik for WAIC in R (MUCH faster than doing it inside JAGS)
  samples_mat <- as.matrix(samples)
  S <- nrow(samples_mat)
  log_lik_mat <- matrix(0, nrow = S, ncol = N)
  
  cat("Computing WAIC...\n")
  for (s in 1:S) {
    if (H == 1) {
      log_lik_mat[s, ] <- dbeta(y, samples_mat[s, "alpha"], samples_mat[s, "beta"], log = TRUE)
    } else {
      lik_i <- rep(0, N)
      for (h in 1:H) {
        pi_h <- samples_mat[s, paste0("pi[", h, "]")]
        alpha_h <- samples_mat[s, paste0("alpha[", h, "]")]
        beta_h <- samples_mat[s, paste0("beta[", h, "]")]
        lik_i <- lik_i + pi_h * dbeta(y, alpha_h, beta_h, log = FALSE)
      }
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
waic_results <- waic_list
mcmc_samples <- models_mcmc[[best_H]]
