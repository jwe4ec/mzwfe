# ---------------------------------------------------------------------------- #
# Prepare Network Data and Run Model
# Author: Jeremy W. Eberle
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Set working directory ----
# ---------------------------------------------------------------------------- #

# Use setwd function to set working directory to the folder containing files
# downloaded from the Open Science Framework (https://osf.io/mzwfe).

# ---------------------------------------------------------------------------- #
# Notes ----
# ---------------------------------------------------------------------------- #

# None

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

mrg_oa <- read.csv("./data/intermediate/merge_oa_rest2.csv")
participant <- read.csv("./data/intermediate/participant_raw_rest2.csv")
mrg_rr <- read.csv("./data/intermediate/merge_rr_rest2.csv")

mrg_dat <- list(oa = mrg_oa,
                participant = participant,
                rr = mrg_rr)

# ---------------------------------------------------------------------------- #
# Restrict data for analysis ----
# ---------------------------------------------------------------------------- #

# Restrict time points to "PRE", "SESSION3", and "SESSION6"

anl_dat <- vector("list", length(mrg_dat))
names(anl_dat) <- names(mrg_dat)

for (i in 1:length(mrg_dat)) {
  if (names(mrg_dat[i]) == "participant") {
    anl_dat[[i]] <- mrg_dat[[i]]
  } else {
    anl_dat[[i]] <- mrg_dat[[i]][mrg_dat[[i]][, "session_only"] %in%
                                   c("PRE", "SESSION3", "SESSION6"), ]
  }
}

# Identify columns that will be manifest nodes. Note: "rr_ns_mean" and "rr_ps_mean" 
# are mean negative and mean positive threat-related interpretation ratings

oa_node_cols <- c("anxious_freq", "anxious_sev", "avoid", "interfere", "interfere_social")
rr_node_cols <- c("rr_ns_mean", "rr_ps_mean")

# TODO: Confirm clean data loaded in prior script contains only ITT participants





# Use full outer join to  all node columns into one table

net_dat <- merge(anl_dat$oa[, c("participant_id", "session_only", oa_node_cols)],
                 anl_dat$rr[, c("participant_id", "session_only", rr_node_cols)],
                 by = c("participant_id", "session_only"),
                 all = TRUE)

row_all_nodes_na_participant_ids <- 
  net_dat[rowSums(is.na(net_dat[, c(oa_node_cols, rr_node_cols)])) == 
              ncol(net_dat[, c(oa_node_cols, rr_node_cols)]), "participant_id"]

View(net_dat[net_dat$participant_id %in% row_all_nodes_na_participant_ids, ])

# TODO: Remove participant 583, who has no data on any nodes

net_dat <- net_dat[net_dat$participant_id != 583, ]





# Add condition

net_dat <- merge(net_dat, 
                 anl_dat$participant[, c("participant_id", "cbmCondition", "prime")],
                 by = "participant_id",
                 all.x = TRUE)

# TODO: For now, remove 40 participants with no condition

table(net_dat$cbmCondition, useNA = "always")
table(net_dat$prime, useNA = "always")

net_dat <- net_dat[!is.na(net_dat$cbmCondition), ]





# ---------------------------------------------------------------------------- #
# Restructure data ----
# ---------------------------------------------------------------------------- #

# Convert to wide format

net_dat_wide <- reshape(net_dat, 
                        direction = "wide",
                        idvar = "participant_id",
                        timevar = "session_only",
                        v.names = c("anxious_freq", "anxious_sev", "avoid", 
                                    "interfere", "interfere_social", 
                                    "rr_ns_mean", "rr_ps_mean"))

# ---------------------------------------------------------------------------- #
# Specify model ----
# ---------------------------------------------------------------------------- #

# TODO: Specify varMat. We have "oa" vars at every wave but "rr" vars only at 
# "PRE", "SESSION3", "SESSION6", and "SESSION8". Should "oa" vars be included
# for every wave and "rr" vars be specified as NA where they were not assessed?
# Stick with shared waves for now.

nodes <- c(oa_node_cols, rr_node_cols)
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





# TODO: Specify lambda as identity matrix

lambda_r34 <- diag(nrow = length(nodes),
                   ncol = length(nodes))





# TODO: Specify model using same arguments as example, except using raw data
# (instead of covariance matrix) and FIML estimator (instead of "ML")

model_r34_fiml <- dlvm1(data = net_dat_wide,
                        vars = varMat_r34,
                        lambda = lambda_r34,
                        within_latent = "ggm",     # TODO: Check (unchanged from ex.)
                        within_residual = "chol",  # TODO: Check (unchanged from ex.)
                        between_latent = "ggm",    # TODO: Check (unchanged from ex.)
                        between_residual = "chol", # TODO: Check (unchanged from ex.)
                        estimator = "FIML")

model_r34_ml <- dlvm1(data = net_dat_wide,
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

model_r34_fiml %>% print # Says "relative convergence", so use this for now
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





# Save model

dir.create("./output")

saveRDS(model_r34_fiml_prune, file="./output/model_r34_fiml_prune_results.RDS")