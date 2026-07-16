# Extract MCMC samples from the best model and the number of samples S
samples_mat <- as.matrix(mcmc_samples)
S <- nrow(samples_mat)

# Define a grid of values for y (Load factor is between 10^-3 and 1 - 10^-3)
y_grid <- seq(0.001, 0.999, length.out = 500)
pred_density <- rep(0, length(y_grid))

# Matrix to store the densities of individual components
comp_density <- matrix(0, nrow = length(y_grid), ncol = best_model)

# Iterate over all samples
for (s in 1:S) {
  # For each sample, compute the mixture density on the grid
  mix_dens_s <- rep(0, length(y_grid))
  # Iterate over all components
  for (h in 1:best_model) {
    #Retrieve the mixing weight and the shape parameters of that component of the sample
    pi_h <- samples_mat[s, paste0("pi[", h, "]")]
    alpha_h <- samples_mat[s, paste0("alpha[", h, "]")]
    beta_h <- samples_mat[s, paste0("beta[", h, "]")]
    
    # Add weighted Beta density for component h
    comp_dens_h <- pi_h * dbeta(y_grid, alpha_h, beta_h)
    mix_dens_s <- mix_dens_s + comp_dens_h
    comp_density[, h] <- comp_density[, h] + comp_dens_h
  }
  # Accumulate all complete distributions over all samples
  pred_density <- pred_density + mix_dens_s
}

# Average over S samples to get the final predictive densities
pred_density <- pred_density / S
comp_density <- comp_density / S

# Create a data frame for plotting the curve
df_pred <- data.frame(
  y_grid = y_grid,
  density = pred_density
)

# Convert component density matrix to a long format data frame for ggplot
df_comp <- as.data.frame(comp_density)
colnames(df_comp) <- paste0("Component_", 1:best_model)
df_comp$y_grid <- y_grid
df_comp_long <- pivot_longer(df_comp, cols = starts_with("Component"), names_to = "Component", values_to = "density")

# Posterior predictive density
p_predictive <- ggplot(df, aes(x = load_factor)) +
  # Empirical histogram
  geom_histogram(aes(y = after_stat(density)), bins = 40, fill = "tomato", color = "white", alpha = 0.8) +
  # Posterior distribution curve
  geom_line(data = df_pred, aes(x = y_grid, y = density), color = "darkblue", linewidth = 1.2) +
  theme_minimal() +
  labs(
    title = paste("Posterior Predictive Density vs Empirical Histogram (H =", best_model, ")"),
    x = "Load factor (hourly / daily peak)",
    y = "Density"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold")
  )

# Display the overall plot
print(p_predictive)
ggsave("images/posterior_predictive.png", plot = p_predictive, width = 8, height = 5, dpi = 300)

# Individual components graphic
p_components <- ggplot() +
  geom_line(data = df_comp_long, aes(x = y_grid, y = density, color = Component), linewidth = 1) +
  theme_minimal() +
  labs(
    title = paste("Individual Mixture Components (H =", best_model, ")"),
    x = "Load factor",
    y = "Density (Weighted)",
    color = "Component"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold")
  )

# Display the components plot
print(p_components)
ggsave("images/components_only.png", plot = p_components, width = 8, height = 5, dpi = 300)
