# ---------------------------------------------------------------------------- #
# Create "dlvm1" Plots and Tables (SKIP)
# Author: Jeremy W. Eberle
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Notes ----
# ---------------------------------------------------------------------------- #

# The present script plots networks and creates tables for the "dvlm1" model,
# following procedures in Epskamp (2020). However, given that all residual 
# variances were not set to zero in this model despite its using observed (vs.
# latent) variables, these results should not be trusted.

# ---------------------------------------------------------------------------- #
# Set working directory ----
# ---------------------------------------------------------------------------- #

# Use setwd function to set working directory to the folder containing files
# downloaded from the Open Science Framework (https://osf.io/mzwfe)

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

pkgs <- c("semPlot", "qgraph", "psychonetrics", "xtable", "ggplot2", "dplyr", "tidyr")
groundhog.library(pkgs, groundhog_day)

# ---------------------------------------------------------------------------- #
# Import results ----
# ---------------------------------------------------------------------------- #

analysis_r34 <- readRDS("./output/dlvm1_model_r34_fiml_prune_results.RDS")

# TODO: Locate and import bootstrap results




# ---------------------------------------------------------------------------- #
# Standardize factor loadings ----
# ---------------------------------------------------------------------------- #

# TODO: Is this even needed given that we are using manifest (vs. latent) nodes?






# Unstandardized loadings

lambda_r34 <- getmatrix(analysis_r34, "lambda")

# Residual variances

theta_r34 <- getmatrix(analysis_r34, "sigma_epsilon_within")

# Latent variance-covariance

psi_r34 <- getmatrix(analysis_r34, "sigma_zeta_within")

# Use semPlot to standardize

semPlot_mod_r34 <- lisrelModel(LY = lambda_r34, TE = theta_r34, PS = psi_r34)
modMats_r34 <- modelMatrices(semPlot_mod_r34, "Mplus")
lambdastd_r34 <- modMats_r34$Lambda[[1]]$std

# ---------------------------------------------------------------------------- #
# Plot factor loadings ----
# ---------------------------------------------------------------------------- #

# TODO: Is this even needed given that we are using manifest (vs. latent) nodes?





# TODO: Number of latents. Is this needed?

nLat_r34 <- ncol(lambdastd_r34)





# Number of observed

nObs_r34 <- nrow(lambdastd_r34)

# TODO: Edgelist for graph. Is this correct?

Edgelist_r34 <- cbind(c(col(lambdastd_r34)),
                      c(row(lambdastd_r34)) + ncol(lambdastd_r34),
                      c(lambdastd_r34))





# Shape

shape_r34 <- c(rep("ellipse", nLat_r34), rep("rectangle", nObs_r34))

# Size1

size1_r34 <-  c(rep(10, nLat_r34), rep(5, nObs_r34))

# Size2

size2_r34 <-  c(rep(10, nLat_r34), rep(5, nObs_r34))

# Edge connect points

ECP_r34 <- Edgelist_r34
ECP_r34[, 1] <- NA # 0.5*pi
ECP_r34[, 2] <- 0

# Labels

latLabels_r34 <-  c("anxious_freq\n(AF)", "anxious_sev\n(AS)", "avoid\n(SA)", 
                    "interfere\n(WI)", "interfere_social\n(SI)", 
                    "rr_ns_mean\n(NB)", "rr_ps_mean\n(PB)")

# Manifest labels

manLabels_r34 <- c("anxious_freq", "anxious_sev", "avoid", 
                   "interfere", "interfere_social", 
                   "rr_ns_mean", "rr_ps_mean")

# Size of labels

labelCex_r34 <- c(rep(1.5, nLat_r34),
                  rep(1, nObs_r34))

# Starting layout

Layout_r34 <- rbind(cbind(seq(-1, 1, length = nLat_r34 + 2)[-c(1, nLat_r34 + 2)],
                          1),
                    cbind(seq(-1, 1, length = nObs_r34 + 2)[-c(1, nObs_r34 + 2)],
                          0))

# Plot and save to PDF

qgraph(Edgelist_r34,
       shape = shape_r34,
       vsize = size1_r34,
       vsize2 = size2_r34,
       layout = Layout_r34,
       mar = c(4, 1, 7, 1),
       edgeConnectPoints = ECP_r34,
       labels = c(latLabels_r34, manLabels_r34),
       label.scale = FALSE,
       label.cex = labelCex_r34,
       asize = 5,
       theme = "colorblind",
       filetype = "pdf",
       filename = "panelloadings_r34",
       width = 15,
       height = 3,
       cut = 0)

# ---------------------------------------------------------------------------- #
# Plot networks ----
# ---------------------------------------------------------------------------- #

# Extract network results

temporal_r34 <- getmatrix(analysis_r34, "PDC")
contemporaneous_r34 <- getmatrix(analysis_r34, "omega_zeta_within")
between_r34 <- getmatrix(analysis_r34, "omega_zeta_between")





# TODO: Locate code for generating bootstraps (not included in example)





# Specify node labels

latLabels_r34 <-  c("AF", "AS", "SA", "WI", "SI","NB", "PB")

# Specify layout and adjust to prevent overlaps

Layout_r34 <- averageLayout(temporal_r34, contemporaneous_r34, between_r34,
                            repulsion = 0.9)
rownames(Layout_r34) <- latLabels_r34
colnames(Layout_r34) <- c("x", "y")

Layout_r34["SA", "x"] <- 0.1
Layout_r34["SI", "y"] <- -1

# Loop rotation not needed (no self-loops)

loopRotation_r34 <- rep(NA, 7)

# TODO: Plot graphs

max_r34 <- max(c(abs(temporal_r34), abs(contemporaneous_r34), abs(between_r34)))

qgraph(temporal_r34, 
       layout = Layout_r34,
       theme = "colorblind",  
       vsize = 13, 
       mar = rep(5,4), 
       asize = 8,
       directed = TRUE, 
       labels = latLabels_r34, 
       maximum = max_r34, 
       esize = 10,                  # TODO: Check this in all graphs (from ex.)
       edge.labels = FALSE,
       filetype = "pdf", 
       filename = "./output/plots/dlvm1_panelPDC_r34",
       vTrans = 254, 
       loopRotation = loopRotation_r34,
       label.scale.equal = TRUE)

qgraph(contemporaneous_r34, 
       layout = Layout_r34,
       theme = "colorblind", 
       vsize = 13, 
       mar = rep(5,4), 
       labels = latLabels_r34, 
       maximum = max_r34, 
       esize = 10, 
       edge.labels = FALSE,
       filetype = "pdf", 
       filename = "./output/plots/dlvm1_panelPCC_r34",
       vTrans = 254, 
       loopRotation = loopRotation_r34,
       label.scale.equal = TRUE)

qgraph(between_r34, 
       layout = Layout_r34,
       theme = "colorblind", 
       vsize = 13, 
       mar = rep(5,4), 
       labels = latLabels_r34, 
       maximum = max_r34, 
       esize = 10, 
       edge.labels = FALSE,
       filetype = "pdf", 
       filename = "./output/plots/dlvm1_panelBetween_r34",
       vTrans = 254, 
       loopRotation = loopRotation_r34,
       label.scale.equal = TRUE)





# ---------------------------------------------------------------------------- #
# Create tables ----
# ---------------------------------------------------------------------------- #

# Temporal

temporal_table_r34 <- temporal_r34
temporal_table_r34[temporal_table_r34 == 0] <- NA
rownames(temporal_table_r34) <- colnames(temporal_table_r34) <- latLabels_r34

# Contemporaneous, with marginal correlations in upper tri

contemporaneous_table_r34 <- contemporaneous_r34

contemporaneous_cors_r34 <- cov2cor(getmatrix(analysis_r34, "sigma_zeta_within"))

contemporaneous_table_r34[upper.tri(contemporaneous_table_r34)] <- 
  contemporaneous_cors_r34[upper.tri(contemporaneous_cors_r34)]

contemporaneous_table_r34[contemporaneous_table_r34 == 0] <- NA

rownames(contemporaneous_table_r34) <- colnames(contemporaneous_table_r34) <- latLabels_r34

# Between, with marginal correlations in upper tri

between_table_r34 <- between_r34

between_cors_r34 <- cov2cor(getmatrix(analysis_r34, "sigma_zeta_between"))

between_table_r34[upper.tri(between_table_r34)] <- 
  between_cors_r34[upper.tri(between_cors_r34)]

between_table_r34[between_table_r34 == 0] <- NA

rownames(between_table_r34) <- colnames(between_table_r34) <- latLabels_r34