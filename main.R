
#Global Parameters
GLOBAL_N_ITER <- 2000
GLOBAL_N_BURNIN <- 700

source("R/0_setup.R") 

cat("Executing Task 1: Fitting Beta mixtures (H = 1 to 5) and calculating WAIC...\n")
source("R/1_task.R")

cat("Executing Task 2: Generating Posterior Predictive Check...\n")
source("R/2_task.R")

cat("Executing Task 3: MAP clustering and empirical fractions...\n")
source("R/3_task.R")

cat("Executing Task 4: Fitting multinomial model with Fourier-transformed hours...\n")
source("R/4_task.R")