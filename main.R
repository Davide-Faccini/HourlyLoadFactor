
#Global Parameters
GLOBAL_N_ITER <- 3000
GLOBAL_N_BURNIN <- 1000
set.seed(42)

# ------------------------------------------------------------------------------
# 0. SETUP AND DATA PREPARATION
# ------------------------------------------------------------------------------
cat("Loading libraries and dataset...\n")
source("R/0_setup.R") 

# ------------------------------------------------------------------------------
# TASK 1: Beta Mixture Models and WAIC Selection
# ------------------------------------------------------------------------------
cat("Executing Task 1: Fitting Beta mixtures (H = 1 to 5) and calculating WAIC...\n")
source("R/1_task.R")

# ------------------------------------------------------------------------------
# TASK 2: Posterior Predictive Check
# ------------------------------------------------------------------------------
cat("Executing Task 2: Generating Posterior Predictive Check...\n")
source("R/2_task.R")

# ------------------------------------------------------------------------------
# TASK 3: Post-hoc Exploration and MAP Assignment (Clustering)
# ------------------------------------------------------------------------------
cat("Executing Task 3: MAP clustering and empirical fractions...\n")
source("R/3_task.R")

# ------------------------------------------------------------------------------
# TASK 4: Advanced Classification Model (Multinomial Logistic Regression)
# ------------------------------------------------------------------------------
cat("Executing Task 4: Fitting multinomial model with Fourier-transformed hours...\n")
source("R/4_task.R")