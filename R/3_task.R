# Extract MCMC samples from the best model
samples_mat <- as.matrix(mcmc_samples)

# Arrays for posterior means of pi, alpha, and beta
pi_mean <- rep(0, best_model)
alpha_mean <- rep(0, best_model)
beta_mean <- rep(0, best_model)

# Calculate posterior means of the three parameters
for (h in 1:best_model) {
  if (best_model > 1) {
    pi_mean[h] <- mean(samples_mat[, paste0("pi[", h, "]")]) 
  } else {
    pi_mean[h] = 1
  }
  alpha_mean[h] <- mean(samples_mat[, paste0("alpha[", h, "]")])
  beta_mean[h] <- mean(samples_mat[, paste0("beta[", h, "]")])
}

N <- nrow(df)
prob_matrix <- matrix(0, nrow = N, ncol = best_model)

# Calculate the probability of each observation belonging to each component
# Iterate over all observations
for (i in 1:N) {
  # Iterate over all components
  for (h in 1:best_model) {
    # Probability calculation
    prob_matrix[i, h] <- pi_mean[h] * dbeta(df$load_factor[i], alpha_mean[h], beta_mean[h])
  }
  # Normalize the probability sum to 1 over all observations
  prob_matrix[i, ] <- prob_matrix[i, ] / sum(prob_matrix[i, ])
}

# Assign each observation to the component with the maximum a posteriori probability
df$map_cluster <- apply(prob_matrix, 1, which.max)

# Convert map_cluster to a factor for plotting (to avoid continuous space number sets)
df$map_cluster <- as.factor(df$map_cluster)

# Pipe instructions to compute the fraction of observations belonging to each cluster for each hour
library(dplyr)
fractions_df <- df %>%
  group_by(hour, map_cluster) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(hour) %>%
  mutate(fraction = count / sum(count))

# Create the plot
p_fractions <- ggplot(fractions_df, aes(x = hour, y = fraction, fill = map_cluster)) +
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
