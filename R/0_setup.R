# 0. LIBRERIE E SETUP
# ------------------------------------------------------------------------------
library(rjags)    # Per l'inferenza MCMC
library(loo)      # Per il calcolo del WAIC
library(ggplot2)  # Per i grafici
library(dplyr)    # Per la manipolazione dati
library(nnet)     # Per la regressione logistica multinomiale (Task 4)

# Caricamento del dataset
# Il dataset contiene 'date', 'hour' e 'load factor' tra (10^-3, 1 - 10^-3)
df <- read.csv("R/hourly_load_factor.csv")

# Estrazione della variabile di risposta
y <- df$load_factor
N <- length(y)