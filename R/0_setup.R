library(rjags)    # MCMC
library(loo)      # WAIC calculation
library(ggplot2)  # Plotting graphs
library(dplyr)    # Pipe and manage data
library(tidyr)    # Reshape data
library(nnet)     # Multinomial regression

if (!dir.exists("images")) {
  dir.create("images")
}

# Load dataset
df <- read.csv("R/hourly_load_factor.csv")

# Extract the needed variables
y <- df$load_factor
N <- length(y)