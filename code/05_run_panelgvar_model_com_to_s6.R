# ---------------------------------------------------------------------------- #
# Run "panelgvar" Model with Common Time Points to Session 6
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

net_dat_com_to_s6_wide <- read.csv(file = "./data/intermediate/net_dat_com_to_s6_wide.csv")

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





# Specify model using raw data (instead of covariance matrix) and "FIML" 
# estimator given missing data

model_r34 <- panelgvar(net_dat_com_to_s6_wide, vars = varMat_r34, estimator = "FIML")

# ---------------------------------------------------------------------------- #
# Run model ----
# ---------------------------------------------------------------------------- #

# TODO: Estimate saturated model, which yields the following warnings

model_r34 <- model_r34 %>% runmodel

# Warning messages:
# 1: In (function (start, objective, gradient = NULL, hessian = NULL,  :
#   NA/NaN function evaluation
# 2: In (function (start, objective, gradient = NULL, hessian = NULL,  :
#   NA/NaN function evaluation
# 3: In (function (start, objective, gradient = NULL, hessian = NULL,  :
#   NA/NaN function evaluation
# 4: In (function (start, objective, gradient = NULL, hessian = NULL,  :
#   NA/NaN function evaluation
# 5: In runmodel(.) :
#   Information matrix or implied variance-covariance matrix was not 
#   positive semi-definite. This can happen because the model is not 
#   identified, or because the optimizer encountered problems. 
#   Try running the model with a different optimizer using setoptimizer(...).
# 6: In runmodel(.) :
#   One or more parameters were estimated to be near its bounds. This may 
#   be indicative of, for example, a Heywood case, but also of an optimization 
#   problem. Interpret results and fit with great care. For unconstrained 
#   estimation, set bounded = FALSE.
# 7: In runmodel(.) :
#   Model might not have converged properly: mean(abs(gradient)) > 1.

# Try unconstrained estimation (e.g., allowing variances to be negative), which
# yields the following warnings

model_r34_un <- model_r34 %>% runmodel(bounded = FALSE)

# Warning messages:
# 1: In (function (start, objective, gradient = NULL, hessian = NULL,  :
#   NA/NaN function evaluation
# 2: In (function (start, objective, gradient = NULL, hessian = NULL,  :
#   NA/NaN function evaluation
# 3: In (function (start, objective, gradient = NULL, hessian = NULL,  :
#   NA/NaN function evaluation
# 4: In runmodel(., bounded = FALSE) :
#   The optimizer encountered at least one non-positive definite matrix 
#   and used a pseudoinverse in parameter estimation. Results may not be 
#   accurate. You could try to check for consistency with a different 
#   optimizer using 'setoptimizer'




# TODO: Inspect parameters. Use the unbounded model for now because it has fewer
# estimates of ~ 0 and no negative variances seem to have emerged

model_r34 %>% parameters
model_r34_un %>% parameters



# TODO: Check fit and maybe try different optimizers

model_r34 %>% print    # Says false convergence (8)
model_r34 %>% fit

model_r34_un %>% print # Says relative convergence (4). Keep using this.
model_r34_un %>% fit




# TODO: Specify estimation algorithm settings, using same as used for "dlvm1"
# model. Check that these are appropriate for "panelgvar" wrapper too.

alpha <- 0.01
adjust <- "none"
searchstrategy <- "modelsearch"

# TODO: Estimation algorithm (prune step)

model_r34_un_prune <- model_r34_un %>% 
  prune(alpha = alpha, adjust = adjust, recursive = FALSE)
  




# Search strategy. Yielded 50 or more warnings such as those below.

if (searchstrategy == "stepup") {
  model_r34_un_prune <- model_r34_un_prune %>%  
    stepup(alpha = alpha, criterion = "bic")
} else if (searchstrategy == "modelsearch") {
  model_r34_un_prune <- model_r34_un_prune %>%  
    modelsearch(prunealpha = alpha, addalpha = alpha, bounded = FALSE)
}

# Warning messages:
# 1: In runmodel(., verbose = FALSE, addMIs = FALSE, return_improper = TRUE,  ... :
#   Information matrix or implied variance-covariance matrix was not positive 
#   semi-definite. This can happen because the model is not identified, or because 
#   the optimizer encountered problems. Try running the model with a different 
#   optimizer using setoptimizer(...).
# 2: In runmodel(., verbose = FALSE, addMIs = FALSE, return_improper = TRUE,  ... :
#   Model might not have converged properly: mean(abs(gradient)) > 1.
# 8: In runmodel(., verbose = FALSE, addMIs = FALSE, return_improper = TRUE,  ... :
#   The optimizer encountered at least one non-positive definite matrix and used 
#   a pseudoinverse in parameter estimation. Results may not be accurate. You could 
#   try to check for consistency with a different optimizer using 'setoptimizer'

# Compare original and pruned models. Pruned model fits better.

comp_r34 <- compare(original = model_r34_un,
                    pruned = model_r34_un_prune)

comp_r34$AIC[1] - comp_r34$AIC[2] # 42.25
comp_r34$BIC[1] - comp_r34$BIC[2] # 221.32

# TODO: Check fit of pruned model

model_r34_un_prune %>% print
model_r34_un_prune %>% fit




# Rename model

panelgvar_model_r34_un_prune_com_to_s6 <- model_r34_un_prune

# Save model

saveRDS(panelgvar_model_r34_un_prune_com_to_s6, file = "./output/panelgvar_model_r34_un_prune_com_to_s6_results.RDS")

# Save other objects in list

mod_com_to_s6 <- list(model_r34 = model_r34,
                      model_r34_un = model_r34_un,
                      model_r34_un_prune = model_r34_un_prune,
                      comp_r34_un = comp_r34)

save(mod_com_to_s6, file = "./output/mod_com_to_s6.RData")