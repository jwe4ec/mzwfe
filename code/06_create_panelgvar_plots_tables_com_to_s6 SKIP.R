# ---------------------------------------------------------------------------- #
# Create "panelgvar" Plots and Tables with Common Time Points to Session 6 (SKIP)
# Author: Jeremy W. Eberle
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Notes ----
# ---------------------------------------------------------------------------- #

# The present script plots networks and creates tables for the "panelgvar" model,
# following procedures in Epskamp (2020).

# This script will be supplanted by a script that uses all time points to Session
# 6, which is more appropriate than using only Baseline, Session 3, and Session 6.

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

analysis_r34 <- readRDS("./output/panelgvar_model_r34_un_prune_com_to_s6_results.RDS")

# TODO: Locate and import bootstrap results




# ---------------------------------------------------------------------------- #
# Check factor loadings and residual variances ----
# ---------------------------------------------------------------------------- #

# Confirm that factor loadings are set to identity matrix and that residual
# variances are set to 0

# Factor loadings

lambda_r34 <- getmatrix(analysis_r34, "lambda")

# Residual variances

theta_r34 <- getmatrix(analysis_r34, "sigma_epsilon_within")

# Latent variance-covariance

psi_r34 <- getmatrix(analysis_r34, "sigma_zeta_within")

# ---------------------------------------------------------------------------- #
# Plot networks ----
# ---------------------------------------------------------------------------- #

# Extract network results

temporal_r34 <- getmatrix(analysis_r34, "PDC")
contemporaneous_r34 <- getmatrix(analysis_r34, "omega_zeta_within")
between_r34 <- getmatrix(analysis_r34, "omega_zeta_between")

# TODO: Locate code for generating bootstraps (not included in example)





# Specify node labels

Labels_r34 <-  c("AF", "AS", "SA", "WI", "SI","NB", "PB")

# Specify layout and adjust to prevent overlaps

Layout_r34 <- averageLayout(temporal_r34, contemporaneous_r34, between_r34,
                            repulsion = 0.9)
rownames(Layout_r34) <- Labels_r34
colnames(Layout_r34) <- c("x", "y")

# Loop rotation

loopRotation_r34 <- rep(NA, 7)

# Plot graphs

max_r34 <- max(c(abs(temporal_r34), abs(contemporaneous_r34), abs(between_r34)))

qgraph(temporal_r34, 
       layout = Layout_r34,
       theme = "colorblind",  
       vsize = 13, 
       mar = rep(5,4), 
       asize = 8,
       directed = TRUE, 
       labels = Labels_r34, 
       maximum = max_r34,
       esize = 10,
       edge.labels = FALSE,
       filetype = "pdf", 
       filename = "./output/plots/panelgvar_com_to_s6_panelPDC_r34",
       vTrans = 254, 
       loopRotation = loopRotation_r34,
       label.scale.equal = TRUE)

qgraph(contemporaneous_r34, 
       layout = Layout_r34,
       theme = "colorblind", 
       vsize = 13, 
       mar = rep(5,4), 
       labels = Labels_r34, 
       maximum = max_r34,
       esize = 10,
       edge.labels = FALSE,
       filetype = "pdf", 
       filename = "./output/plots/panelgvar_com_to_s6_panelPCC_r34",
       vTrans = 254, 
       loopRotation = loopRotation_r34,
       label.scale.equal = TRUE)

qgraph(between_r34, 
       layout = Layout_r34,
       theme = "colorblind", 
       vsize = 13, 
       mar = rep(5,4), 
       labels = Labels_r34, 
       maximum = max_r34,
       esize = 10,
       edge.labels = FALSE,
       filetype = "pdf", 
       filename = "./output/plots/panelgvar_com_to_s6_panelBetween_r34",
       vTrans = 254, 
       loopRotation = loopRotation_r34,
       label.scale.equal = TRUE)

# Plot circle graphs

Labels_r34 <-  c("Anx.\nFreq.", "Anx.\nSev.", "Sit.\nAvoid", "Work\nImp.", 
                 "Soc.\nImp.","Neg.\nBias", "Pos.\nBias")

tem_plot <- qgraph(temporal_r34, 
                   layout = "circle", 
                   labels = Labels_r34, 
                   theme = "colorblind",
                   asize = 7, 
                   vsize = 10, 
                   label.cex = 1.1, 
                   mar = c(8,8,8,8), 
                   title = "Temporal", 
                   label.scale = FALSE,
                   maximum = max_r34,
                   esize = 10)

con_plot <- qgraph(contemporaneous_r34, 
                   layout = "circle", 
                   labels = Labels_r34, 
                   theme = "colorblind",
                   vsize =  10, 
                   label.cex = 1.1, 
                   mar = c(8,8,8,8), 
                   title = "Contemporaneous", 
                   label.scale = FALSE,
                   maximum = max_r34,
                   esize = 10)

btw_plot <- qgraph(between_r34, 
                   layout = "circle", 
                   labels = Labels_r34, 
                   theme = "colorblind",
                   vsize = 10, 
                   label.cex = 1.1, 
                   mar = c(8,8,8,8), 
                   title = "Between-Subjects", 
                   label.scale = FALSE,
                   maximum = max_r34,
                   esize = 10)

qgraph(tem_plot,
       filetype = "pdf",
       filename = "./output/plots/panelgvar_circle_com_to_s6_panelPDC_r34")
qgraph(con_plot,
       filetype = "pdf",
       filename = "./output/plots/panelgvar_circle_com_to_s6_panelPCC_r34")
qgraph(btw_plot,
       filetype = "pdf",
       filename = "./output/plots/panelgvar_circle_com_to_s6_panelBetween_r34")

pdf("./output/plots/panelgvar_circle_com_to_s6_multipanel.pdf", width = 12, height = 4)
layout(t(1:3))
qgraph(tem_plot)
box("figure")
qgraph(con_plot)
box("figure")
qgraph(btw_plot)
box("figure")
dev.off()

# ---------------------------------------------------------------------------- #
# Create tables ----
# ---------------------------------------------------------------------------- #

Labels_r34 <-  c("Anx. Freq.", "Anx. Sev.", "Sit. Avoid", "Work Imp.", 
                 "Soc. Imp.","Neg. Bias", "Pos. Bias")

# Temporal

temporal_table_r34 <- temporal_r34
temporal_table_r34[temporal_table_r34 == 0] <- NA
rownames(temporal_table_r34) <- colnames(temporal_table_r34) <- Labels_r34
temporal_table_r34 <- round(temporal_table_r34, 2)

# Contemporaneous, with marginal correlations in upper tri

contemporaneous_table_r34 <- contemporaneous_r34

contemporaneous_cors_r34 <- cov2cor(getmatrix(analysis_r34, "sigma_zeta_within"))

contemporaneous_table_r34[upper.tri(contemporaneous_table_r34)] <- 
  contemporaneous_cors_r34[upper.tri(contemporaneous_cors_r34)]

contemporaneous_table_r34[contemporaneous_table_r34 == 0] <- NA

rownames(contemporaneous_table_r34) <- colnames(contemporaneous_table_r34) <- Labels_r34
contemporaneous_table_r34 <- round(contemporaneous_table_r34, 2)

# Between, with marginal correlations in upper tri

between_table_r34 <- between_r34

between_cors_r34 <- cov2cor(getmatrix(analysis_r34, "sigma_zeta_between"))

between_table_r34[upper.tri(between_table_r34)] <- 
  between_cors_r34[upper.tri(between_cors_r34)]

between_table_r34[between_table_r34 == 0] <- NA

rownames(between_table_r34) <- colnames(between_table_r34) <- Labels_r34
between_table_r34 <- round(between_table_r34, 2)