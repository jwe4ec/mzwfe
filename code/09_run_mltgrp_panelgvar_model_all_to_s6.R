# ---------------------------------------------------------------------------- #
# Run Multigroup "panelgvar" Model with All Time Points to Session 6
# Author: Jeremy W. Eberle
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Notes ----
# ---------------------------------------------------------------------------- #

# This script extends the approach in "run_panelgvar_model_all_to_s6.R" to a
# multigroup model

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

# Specify varMat. We have "oa" vars at every wave but "rr" vars only at 
# "PRE", "SESSION3", "SESSION6", and "SESSION8". Include all waves.

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

model_r34 <- panelgvar(net_dat_all_to_s6_wide, 
                       vars = varMat_r34, 
                       estimator = "FIML",
                       groups = "cbmCondition")

# ---------------------------------------------------------------------------- #
# Run model ----
# ---------------------------------------------------------------------------- #

# TODO: Estimate saturated model, which yields the following warning using default
# optimizer "nlminb"

model_r34_nlminb <- model_r34 %>% 
  runmodel # Message = Relative convergence (4). Model Fit Test Statistic = 917.86.

# Warning messages:
# 1: In runmodel(x@baseline_saturated$saturated, addfit = FALSE, addMIs = FALSE,  :
#   Model might not have converged properly: mean(abs(gradient)) > 1.
# 2: In (function (start, objective, gradient = NULL, hessian = NULL,  :
#   NA/NaN function evaluation
# 3: In runmodel(.) :
#   Information matrix or implied variance-covariance matrix was not positive semi-
#   definite. This can happen because the model is not identified, or because the 
#   optimizer encountered problems. Try running the model with a different optimizer 
#   using setoptimizer(...).





# TODO: Inspect parameters

model_r34_nlminb %>% parameters





# TODO: Check fit

model_r34_nlminb %>% print    # Says relative convergence (4)
model_r34_nlminb %>% fit      # RMSEA is ~ 0?




# TODO: Specify estimation algorithm settings, using same as used for "dlvm1"
# model. Check that these are appropriate for "panelgvar" wrapper too.

alpha <- 0.01
adjust <- "none"
searchstrategy <- "modelsearch"

# TODO: Estimation algorithm (prune step)

model_r34_nlminb_prune <- model_r34_nlminb %>% 
  prune(alpha = alpha, adjust = adjust, recursive = FALSE)
  




# TODO: Search strategy. Error says that "'modelsearch' is only implemented for 
# single group models at the moment".

if (searchstrategy == "stepup") {
  model_r34_nlminb_prune <- model_r34_nlminb_prune %>%  
    stepup(alpha = alpha, criterion = "bic")
} else if (searchstrategy == "modelsearch") {
  model_r34_nlminb_prune <- model_r34_nlminb_prune %>%  
    modelsearch(prunealpha = alpha, addalpha = alpha)
}





# TODO: Compare original and pruned models. Original model fits better

comp_r34_nlminb <- compare(original = model_r34_nlminb,
                           pruned = model_r34_nlminb_prune)

comp_r34_nlminb$AIC[1] - comp_r34_nlminb$AIC[2] # -1173.54
comp_r34_nlminb$BIC[1] - comp_r34_nlminb$BIC[2] # -255.20




# TODO: Check fit of pruned model. Use original model.

model_r34_nlminb_prune %>% print
model_r34_nlminb_prune %>% fit





# Rename model

mltgrp_panelgvar_model_r34_all_to_s6 <- model_r34_nlminb

# Save model

saveRDS(mltgrp_panelgvar_model_r34_all_to_s6, 
        file = "./output/mltgrp_panelgvar_model_r34_all_to_s6_results.RDS")

# Save objects in list

mltgrp_mod_all_to_s6 <- list(model_r34_nlminb = model_r34_nlminb,
                             model_r34_nlminb_prune = model_r34_nlminb_prune,
                             comp_r34_nlminb = comp_r34_nlminb)

save(mltgrp_mod_all_to_s6, file = "./output/mltgrp_mod_all_to_s6.RData")