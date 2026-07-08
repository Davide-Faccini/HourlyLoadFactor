# ==============================================================================
# TASK 4: Advanced Classification Model (Multinomial Logistic Regression)
# ==============================================================================

cat("\nExecuting Task 4: Fitting multinomial model with Fourier-transformed hours...\n")

# Assicuriamoci che la colonna map_cluster sia un fattore
df$map_cluster <- as.factor(df$map_cluster)

# 1. Feature Engineering: Trasformata di Fourier per l'ora (circolare)
# Aggiungiamo l'armonica principale (24h) e la secondaria (12h) per catturare eventuali doppi picchi giornalieri
df$sin24 <- sin(2 * pi * df$hour / 24)
df$cos24 <- cos(2 * pi * df$hour / 24)
df$sin12 <- sin(2 * pi * 2 * df$hour / 24)
df$cos12 <- cos(2 * pi * 2 * df$hour / 24)

# 2. Addestramento del modello logistico multinomiale
cat("Fitting multinomial logistic regression...\n")
# Usiamo la funzione multinom del pacchetto nnet (caricato nel setup)
# trace = FALSE per nascondere l'output di ottimizzazione a schermo
multinom_model <- multinom(map_cluster ~ sin24 + cos24 + sin12 + cos12, data = df, trace = FALSE)

# 3. Previsione su una griglia continua di ore
cat("Predicting probabilities over a continuous 24h grid...\n")
# Creiamo 200 punti per avere una curva molto morbida
hour_grid <- seq(0, 23, length.out = 200)

grid_df <- data.frame(
  hour = hour_grid,
  sin24 = sin(2 * pi * hour_grid / 24),
  cos24 = cos(2 * pi * hour_grid / 24),
  sin12 = sin(2 * pi * 2 * hour_grid / 24),
  cos12 = cos(2 * pi * 2 * hour_grid / 24)
)

# Calcoliamo le probabilità predette (restituisce una matrice N_grid x H)
pred_probs <- predict(multinom_model, newdata = grid_df, type = "probs")

# Convertiamo in dataframe e formattiamo i dati in formato 'long' per ggplot
prob_df <- as.data.frame(pred_probs)

# (Se per puro caso la miscela ottimale avesse solo H=2, predict() restituisce un vettore anziché una matrice)
if (best_model == 2) {
  prob_df <- data.frame(`1` = 1 - pred_probs, `2` = pred_probs)
}
colnames(prob_df) <- paste0("Cluster_", 1:ncol(prob_df))
prob_df$hour <- hour_grid

# Usa pivot_longer da tidyr (caricato nel setup) per preparare i dati per ggplot
library(tidyr)
prob_long <- pivot_longer(prob_df, cols = starts_with("Cluster"), names_to = "Cluster", values_to = "Probability")
# Pulisce i nomi per corrispondere visivamente ai cluster di df (es. "1", "2")
prob_long$Cluster <- sub("Cluster_", "", prob_long$Cluster)
prob_long$Cluster <- as.factor(prob_long$Cluster)

# 4. Generazione del grafico
# Ho scelto un grafico ad area (geom_area). Essendo le probabilità a somma 1, un grafico
# ad area impilata è perfetto perché combacia visivamente con il grafico a barre impilate 
# (position="fill") generato empiricamente nel Task 3, rendendo il confronto immediato.
cat("Generating probability plot...\n")
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

# Salvataggio del grafico
ggsave("images/multinomial_probabilities.png", plot = p_multinom, width = 8, height = 5, dpi = 300)
cat("\nPlot salvato con successo come 'images/multinomial_probabilities.png' nella cartella del progetto.\n")
