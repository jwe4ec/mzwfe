# ---------------------------------------------------------------------------- #
# Run "panelgvar" Model with All Time Points to Session 6
# Author: Jeremy W. Eberle
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Notes ----
# ---------------------------------------------------------------------------- #

# This script uses the "panelgvar" wrapper of "dvlm1" to run a multilevel GVAR 
# model (with random intercepts and fixed network parameters), which is what the
# panel-lvgvar model reduces to if all variables are treated as observed without
# measurement error (see Epskamp, 2020, p. 227). It draws on the following code 
# examples from Sacha Epskamp.

# http://psychonetrics.org/files/PNAWS2020lecture.html#panel-data-gvar
# https://github.com/SachaEpskamp/SEM-code-examples/blob/master/psychonetrics/SHARE%20panel%20example/shareAnalysis.R

# ---------------------------------------------------------------------------- #
# Set working directory ----
# ---------------------------------------------------------------------------- #

# Use setwd function to set working directory to the folder containing files
# downloaded from the Open Science Framework (https://osf.io/mzwfe).

# ---------------------------------------------------------------------------- #
# Store working directory, check correct R version, load packages, set seed ----
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

# Set seed

set.seed(1234)

# ---------------------------------------------------------------------------- #
# Import data ----
# ---------------------------------------------------------------------------- #

net_dat_all_to_s6_wide <- read.csv(file = "./data/intermediate/net_dat_all_to_s6_wide.csv")

# ---------------------------------------------------------------------------- #
# Specify model ----
# ---------------------------------------------------------------------------- #

# TODO: Specify varMat. We have "oa" vars at every wave but "rr" vars only at 
# "PRE", "SESSION3", "SESSION6", and "SESSION8". Try including all waves.

nodes <- c("anxious_freq", "anxious_sev", "avoid", "interfere", "interfere_social",
           "rr_ns_mean", "rr_ps_mean")
waves <- c("PRE", paste0("SESSION", 1:6))

varMat_r34 <- matrix(c(paste0(nodes, ".PRE"),
                       c(paste0(nodes[1:5], ".SESSION1"), NA, NA),
                       c(paste0(nodes[1:5], ".SESSION2"), NA, NA),
                       paste0(nodes, ".SESSION3"),
                       c(paste0(nodes[1:5], ".SESSION4"), NA, NA),
                       c(paste0(nodes[1:5], ".SESSION5"), NA, NA),
                       paste0(nodes, ".SESSION6")),
                     nrow = length(nodes), 
                     ncol = length(waves),
                     dimnames = list(nodes, waves))




# Specify model using raw data (instead of covariance matrix) and "FIML" 
# estimator given missing data

model_r34 <- panelgvar(net_dat_all_to_s6_wide, vars = varMat_r34, estimator = "FIML")

# ---------------------------------------------------------------------------- #
# Run model ----
# ---------------------------------------------------------------------------- #

# TODO: Estimate saturated model, which yields the following warning

model_r34 <- model_r34 %>% runmodel

# Warning message:
#   In runmodel(.) :
#   The optimizer encountered at least one non-positive definite matrix and 
#   used a pseudoinverse in parameter estimation. Results may not be accurate. 
#   You could try to check for consistency with a different optimizer using 'setoptimizer'





# TODO: Inspect parameters

model_r34 %>% parameters





# TODO: Check fit and maybe try different optimizers

model_r34 %>% print    # Says relative convergence (4)
model_r34 %>% fit




# TODO: Specify estimation algorithm settings, using same as used for "dlvm1"
# model. Check that these are appropriate for "panelgvar" wrapper too.

alpha <- 0.01
adjust <- "none"
searchstrategy <- "modelsearch"

# TODO: Estimation algorithm (prune step)

model_r34_prune <- model_r34 %>% 
  prune(alpha = alpha, adjust = adjust, recursive = FALSE)
  




# Search strategy. Yielded warnings below.

if (searchstrategy == "stepup") {
  model_r34_prune <- model_r34_prune %>%  
    stepup(alpha = alpha, criterion = "bic")
} else if (searchstrategy == "modelsearch") {
  model_r34_prune <- model_r34_prune %>%  
    modelsearch(prunealpha = alpha, addalpha = alpha, bounded = FALSE)
}

# Warning messages:
# 1: In runmodel(., verbose = FALSE, addMIs = FALSE, return_improper = TRUE,  :
#   The optimizer encountered at least one non-positive definite matrix and 
#   used a pseudoinverse in parameter estimation. Results may not be accurate.
#   You could try to check for consistency with a different optimizer using 'setoptimizer'
# 2: In runmodel(., verbose = FALSE, addMIs = FALSE, return_improper = TRUE,  :
#   The optimizer encountered at least one non-positive definite matrix and
#   used a pseudoinverse in parameter estimation. Results may not be accurate. 
#   You could try to check for consistency with a different optimizer using 'setoptimizer'
# 3: In runmodel(., verbose = FALSE, addMIs = FALSE, return_improper = TRUE,  :
#   The optimizer encountered at least one non-positive definite matrix and 
#   used a pseudoinverse in parameter estimation. Results may not be accurate. 
#   You could try to check for consistency with a different optimizer using 'setoptimizer'

# TODO: Compare original and pruned models. Original model fits better. How is 
# that possible if the purpose of the model search is to optimize BIC?

comp_r34 <- compare(original = model_r34,
                    pruned = model_r34_prune)

mod_all_to_s6$comp_r34$AIC[1] - mod_all_to_s6$comp_r34$AIC[2] # 42.25
mod_all_to_s6$comp_r34$BIC[1] - mod_all_to_s6$comp_r34$BIC[2] # 221.32




# TODO: Check fit of pruned model. Use original model.

model_r34_prune %>% print
model_r34_prune %>% fit





# Rename model

panelgvar_model_r34_all_to_s6 <- model_r34

# Save model

saveRDS(panelgvar_model_r34_all_to_s6, file = "./output/panelgvar_model_r34_all_to_s6_results.RDS")

# Save other objects in list

mod_all_to_s6 <- list(model_r34 = model_r34,
                      comp_r34 = comp_r34)

save(mod_all_to_s6, file = "./output/mod_all_to_s6.RData")
