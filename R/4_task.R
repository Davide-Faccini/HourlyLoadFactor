# Apply Fourier Transforms for circularity of time
df$sin24 <- sin(2 * pi * df$hour / 24)
df$cos24 <- cos(2 * pi * df$hour / 24)
# For multiple mode recognition and better modeling we introduce a second harmonic
# with doubled frequency
df$sin12 <- sin(2 * pi * 2 * df$hour / 24)
df$cos12 <- cos(2 * pi * 2 * df$hour / 24)

# Model fitting
multinom_model <- multinom(map_cluster ~ sin24 + cos24 + sin12 + cos12, data = df, trace = FALSE)

# x-axis point grid
hour_grid <- seq(0, 23, length.out = 200)

grid_df <- data.frame(
  hour = hour_grid,
  sin24 = sin(2 * pi * hour_grid / 24),
  cos24 = cos(2 * pi * hour_grid / 24),
  sin12 = sin(2 * pi * 2 * hour_grid / 24),
  cos12 = cos(2 * pi * 2 * hour_grid / 24)
)

# Compute the probabilities of each cluster representing an hour
pred_probs <- predict(multinom_model, newdata = grid_df, type = "probs")

prob_df <- as.data.frame(pred_probs)

# When H = 2 we get a vector, so we turn it into a df
if (best_model == 2) {
  prob_df <- data.frame(`1` = 1 - pred_probs, `2` = pred_probs)
}
colnames(prob_df) <- paste0("Cluster_", 1:ncol(prob_df))
prob_df$hour <- hour_grid

# Format data for ggplot
library(tidyr)
prob_long <- pivot_longer(prob_df, cols = starts_with("Cluster"), names_to = "Cluster", values_to = "Probability")
prob_long$Cluster <- sub("Cluster_", "", prob_long$Cluster)
prob_long$Cluster <- as.factor(prob_long$Cluster)

# Graph plotting
p_multinom <- ggplot(prob_long, aes(x = hour, y = Probability, fill = Cluster)) +
  geom_area(alpha = 0.85, color = "white", linewidth = 0.2) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, 23, by = 2)) +
  labs(
    title = "Multinomial Regression: Predicted Probability of Clusters",
    subtitle = "Continuous model based on Fourier-transformed hours",
    x = "Hour of the Day (0-23)",
    y = "Predicted Probability",
    fill = "Cluster"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

print(p_multinom)
ggsave("images/multinomial_probabilities.png", plot = p_multinom, width = 8, height = 5, dpi = 300)
