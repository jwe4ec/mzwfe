# ---------------------------------------------------------------------------- #
# Run "dlvm1" Model (SKIP)
# Author: Jeremy W. Eberle
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Notes ----
# ---------------------------------------------------------------------------- #

# This script attempts to follow Epskamp's (2020) panel data example, with the 
# exception that it treats all variables as observed rather than latent. However,
# after results were obtained by running this script and "dlvm1_results.R", it
# became clear that although the present script set the factor loadings to zero,
# it did not set all residual variances to zero (see Epskamp, 2020). Thus, this
# script and "dlvm1_results.R" will be supplanted by "panelgvar" scripts that
# use the "panelgvar" wrapper function for "dlvm1" to achieve this.

# ---------------------------------------------------------------------------- #
# Set working directory ----
# ---------------------------------------------------------------------------- #

# Use setwd function to set working directory to the folder containing files
# downloaded from the Open Science Framework (https://osf.io/mzwfe).

# ---------------------------------------------------------------------------- #
# Store working directory, check correct R version, load packages ----
# ---------------------------------------------------------------------------- #

# Store working directory

wd_dir <- getwd()

# Load custom functions

source("./GitHub Repo/mzwfe/code/00_define_functions.R")

# Check correct R version, load groundhog package, and specify groundhog_day

groundhog_day <- version_control()

# Load packages

pkgs <- c("qgraph", "dplyr", "psych", "graphicalVAR", "GPArotation", "psychonetrics")
groundhog.library(pkgs, groundhog_day)

# ---------------------------------------------------------------------------- #
# Import data ----
# ---------------------------------------------------------------------------- #

net_dat_com_to_s6_wide <- read.csv(file = "./data/intermediate/net_dat_com_to_s6_wide.csv")

# ---------------------------------------------------------------------------- #
# Notes ----
# ---------------------------------------------------------------------------- #



# ---------------------------------------------------------------------------- #
# Specify model ----
# ---------------------------------------------------------------------------- #

# TODO: Specify varMat. We have "oa" vars at every wave but "rr" vars only at 
# "PRE", "SESSION3", "SESSION6", and "SESSION8". Should "oa" vars be included
# for every wave and "rr" vars be specified as NA where they were not assessed?
# Stick with shared waves for now.

nodes <- c("anxious_freq", "anxious_sev", "avoid", "interfere", "interfere_social",
           "rr_ns_mean", "rr_ps_mean")
waves <- c("PRE", paste0("SESSION", 1:6))

varMat_r34 <- matrix(c(paste0(nodes, ".PRE"),
                       rep(NA, length(nodes)),
                       rep(NA, length(nodes)),
                       paste0(nodes, ".SESSION3"),
                       rep(NA, length(nodes)),
                       rep(NA, length(nodes)),
                       paste0(nodes, ".SESSION6")),
                     nrow = length(nodes), 
                     ncol = length(waves),
                     dimnames = list(nodes, waves))





# Set lambda (factor loadings) to identity matrix given that all variables are
# observed (vs. latent; see Epskamp, 2020, p. 228)

lambda_r34 <- diag(nrow = length(nodes),
                   ncol = length(nodes))

# TODO: Set all residual variances to zero given that all variables are observed
# (vs. latent; see Epskamp, 2020, p. 228)





# TODO: Specify model using same arguments as Epskamp (2020), except using raw 
# data (instead of covariance matrix) and "FIML" estimator given missing data.
# Given that "FIML" model yields warnings (see below), also try "ML".

model_r34_fiml <- dlvm1(data = net_dat_com_to_s6_wide,
                        vars = varMat_r34,
                        lambda = lambda_r34,
                        within_latent = "ggm",     # TODO: Check (unchanged from ex.)
                        within_residual = "chol",  # TODO: Check (unchanged from ex.)
                        between_latent = "ggm",    # TODO: Check (unchanged from ex.)
                        between_residual = "chol", # TODO: Check (unchanged from ex.)
                        estimator = "FIML")

model_r34_ml <- dlvm1(data = net_dat_com_to_s6_wide,
                      vars = varMat_r34,
                      lambda = lambda_r34,
                      within_latent = "ggm",     # TODO: Check (unchanged from ex.)
                      within_residual = "chol",  # TODO: Check (unchanged from ex.)
                      between_latent = "ggm",    # TODO: Check (unchanged from ex.)
                      between_residual = "chol", # TODO: Check (unchanged from ex.)
                      estimator = "ML")





# ---------------------------------------------------------------------------- #
# Run model ----
# ---------------------------------------------------------------------------- #

# TODO: Run models

model_r34_fiml <- model_r34_fiml %>% runmodel    # Warnings
model_r34_ml <- model_r34_ml %>% runmodel        # Fewer warnings





# TODO: Inspect parameters. Any numeric issues?

model_r34_fiml %>% parameters
model_r34_ml %>% parameters





# TODO: Check fit and maybe try different optimizers

model_r34_fiml %>% print # Says "relative convergence", so use "FIML" for now
model_r34_fiml %>% fit

model_r34_ml %>% print   # Says "iteration limit reached without convergence"
model_r34_ml %>% fit




# Specify estimation algorithm settings

alpha <- 0.01
adjust <- "none"
searchstrategy <- "modelsearch"

# TODO: Estimation algorithm (prune step)

model_r34_fiml_prune <- model_r34_fiml %>% 
  runmodel %>% 
  prune(alpha = alpha, adjust = adjust, recursive = FALSE) # Warnings





# Search strategy

if (searchstrategy == "stepup") {
  model_r34_fiml_prune <- model_r34_fiml_prune %>%  
    stepup(alpha = alpha, criterion = "bic")
} else if (searchstrategy == "modelsearch") {
  model_r34_fiml_prune <- model_r34_fiml_prune %>%  
    modelsearch(prunealpha = alpha, addalpha = alpha)
}

# TODO: Compare original and pruned models

comp_r34 <- compare(original = model_r34_fiml,
                    pruned = model_r34_fiml_prune)

comp_r34$AIC[1] - comp_r34$AIC[2]
comp_r34$BIC[1] - comp_r34$BIC[2]





# TODO: Check fit of pruned model

model_r34_fiml_prune %>% print
model_r34_fiml_prune %>% fit




# Rename model

dlvm1_model_r34_fiml_prune <- model_r34_fiml_prune

# Save model

saveRDS(dlvm1_model_r34_fiml_prune, file="./output/dlvm1_model_r34_fiml_prune_results.RDS")