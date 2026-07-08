# ==============================================================================
# TASK 3: Post-hoc Exploration and MAP Assignment (Clustering)
# ==============================================================================

cat("\nExecuting Task 3: MAP clustering and empirical fractions...\n")

# To compute the MAP assignment, we need the posterior means of our parameters
# from the best model (H = 5)
samples_mat <- as.matrix(mcmc_samples)

# Calculate posterior means for pi, alpha, and beta
pi_mean <- rep(0, best_model)
alpha_mean <- rep(0, best_model)
beta_mean <- rep(0, best_model)

for (h in 1:best_model) {
  pi_mean[h] <- mean(samples_mat[, paste0("pi[", h, "]")])
  alpha_mean[h] <- mean(samples_mat[, paste0("alpha[", h, "]")])
  beta_mean[h] <- mean(samples_mat[, paste0("beta[", h, "]")])
}

# Calculate the probability of each observation belonging to each component
# P(Z_i = h | y_i) proportional to pi_h * Beta(y_i | alpha_h, beta_h)
N <- nrow(df)
prob_matrix <- matrix(0, nrow = N, ncol = best_model)

for (i in 1:N) {
  for (h in 1:best_model) {
    prob_matrix[i, h] <- pi_mean[h] * dbeta(df$load_factor[i], alpha_mean[h], beta_mean[h])
  }
  # Normalize to sum to 1
  prob_matrix[i, ] <- prob_matrix[i, ] / sum(prob_matrix[i, ])
}

# Assign each observation to the component with the maximum a posteriori (MAP) probability
df$map_cluster <- apply(prob_matrix, 1, which.max)

# Convert map_cluster to a factor for plotting
df$map_cluster <- as.factor(df$map_cluster)

# Calculate the empirical fraction of observations in each component as a function of hour
cat("Calculating empirical fractions and generating plot...\n")

# Use dplyr to group and summarize
library(dplyr)
fractions_df <- df %>%
  group_by(hour, map_cluster) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(hour) %>%
  mutate(fraction = count / sum(count))

# Create the plot
p_fractions <- ggplot(fractions_df, aes(x = hour, y = fraction, fill = map_cluster)) +
  # Usiamo geom_col (bar chart) che gestisce nativamente i cluster mancanti in certe ore senza sballare le somme
  geom_col(width = 1, position = "fill", alpha = 0.85) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, 23, by = 2)) +
  labs(
    title = "Empirical Fraction of MAP Clusters by Hour",
    x = "Hour of the Day (0-23)",
    y = "Fraction of Observations",
    fill = "Cluster"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

# Display the plot
print(p_fractions)

# Save the plot explicitly
ggsave("images/cluster_fractions_hourly.png", plot = p_fractions, width = 8, height = 5, dpi = 300)
cat("\nPlot salvato con successo come 'images/cluster_fractions_hourly.png' nella cartella del progetto.\n")
