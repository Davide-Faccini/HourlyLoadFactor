# ==============================================================================
# MAIN SCRIPT: Beta Mixture Modeling of Tetouan Hourly Load Factors
# ==============================================================================
# This script coordinates the execution of all project phases.
# Please ensure the working directory is set to the folder containing these files.

cat("Starting project execution...\n")

# ------------------------------------------------------------------------------
# GLOBAL PARAMETERS
# ------------------------------------------------------------------------------
# Imposta il seme per garantire che l'algoritmo stocastico MCMC dia risultati 
# sempre riproducibili (es. scelta identica del WAIC migliore)
set.seed(42)

# Modifica questi parametri per aumentare o diminuire i tempi e la precisione
GLOBAL_N_ITER <- 3000         # Numero di iterazioni di campionamento
GLOBAL_N_BURNIN <- 1000       # Numero di iterazioni di burn-in
cat("Global parameters set: n_iter =", GLOBAL_N_ITER, ", n_burnin =", GLOBAL_N_BURNIN, "\n\n")

# ------------------------------------------------------------------------------
# 0. SETUP AND DATA PREPARATION
# ------------------------------------------------------------------------------
cat("Loading libraries and dataset...\n")
source("R/0_setup.R") 
# The environment now contains the 'df' dataframe, the response variable 'y', and 'N'.

# ------------------------------------------------------------------------------
# TASK 1: Beta Mixture Models and WAIC Selection
# ------------------------------------------------------------------------------
cat("Executing Task 1: Fitting Beta mixtures (H = 1 to 5) and calculating WAIC...\n")
source("R/1_task.R")
# The environment now contains 'best_model', 'waic_results', and 'mcmc_samples'.

# ------------------------------------------------------------------------------
# TASK 2: Posterior Predictive Check
# ------------------------------------------------------------------------------
cat("Executing Task 2: Generating Posterior Predictive Check...\n")
source("R/2_task.R")
# Generates and displays the plot overlaying predictive density on the empirical histogram.
# The goal is to verify that the modes are accurately recovered.

# ------------------------------------------------------------------------------
# TASK 3: Post-hoc Exploration and MAP Assignment (Clustering)
# ------------------------------------------------------------------------------
cat("Executing Task 3: MAP clustering and empirical fractions...\n")
source("R/3_task.R")
# Updates 'df' with the 'map_cluster' column.
# Displays the plot of the empirical fraction of observations in each component by hour.

# ------------------------------------------------------------------------------
# TASK 4: Advanced Classification Model (Multinomial Logistic Regression)
# ------------------------------------------------------------------------------
cat("Executing Task 4: Fitting multinomial model with Fourier-transformed hours...\n")
source("R/4_task.R")
# Fits the classification model on hour using component assignments.
# Plots predicted probabilities to compare with the empirical fractions from Task 3.

cat("Execution completed successfully!\n")