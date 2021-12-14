# ---------------------------------------------------------------------------- #
# Create Multigroup "panelgvar" Plots and Tables with All Time Points to Session 6
# Author: Jeremy W. Eberle
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Notes ----
# ---------------------------------------------------------------------------- #

# The present script plots networks and creates tables for the "panelgvar" model,
# following procedures in Epskamp (2020) but extending to a multigroup model

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

analysis_r34 <- readRDS("./output/mltgrp_panelgvar_model_r34_all_to_s6_results.RDS")

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

Labels_r34 <-  c("AF", "AS", "SA", "WI", "SI", "NB", "PB")

# Specify layout and adjust to prevent overlaps

Layout_r34 <- averageLayout(temporal_r34, contemporaneous_r34, between_r34,
                            repulsion = 0.9)
rownames(Layout_r34) <- Labels_r34
colnames(Layout_r34) <- c("x", "y")

# Loop rotation

loopRotation_r34 <- rep(NA, 7)

# Compute max value per condition

max_r34 <- list(POSITIVE = max(c(abs(temporal_r34$POSITIVE), 
                                 abs(contemporaneous_r34$POSITIVE), 
                                 abs(between_r34$POSITIVE))),
                FIFTY_FIFTY = max(c(abs(temporal_r34$FIFTY_FIFTY), 
                                    abs(contemporaneous_r34$FIFTY_FIFTY), 
                                    abs(between_r34$FIFTY_FIFTY))),
                NEUTRAL = max(c(abs(temporal_r34$NEUTRAL), 
                                abs(contemporaneous_r34$NEUTRAL), 
                                abs(between_r34$NEUTRAL))))

# Define function to plot circle graphs per condition

plot_cir <- function(temporal_r34, contemporaneous_r34, between_r34, max_r34, cond) {
  Labels_r34 <-  c("Anx.\nFreq.", "Anx.\nSev.", "Sit.\nAvoid", "Work\nImp.", 
                   "Soc.\nImp.","Neg.\nBias", "Pos.\nBias")
  
  tem_plot <- qgraph(temporal_r34[[cond]], 
                     layout = "circle", 
                     labels = Labels_r34, 
                     theme = "colorblind",
                     asize = 7, 
                     vsize = 14, 
                     label.cex = 1.1, 
                     mar = c(8,8,8,8), 
                     title = paste0(cond, ": Temporal"),
                     label.scale = FALSE,
                     maximum = max_r34[[cond]],
                     esize = 10)
  
  con_plot <- qgraph(contemporaneous_r34[[cond]], 
                     layout = "circle", 
                     labels = Labels_r34, 
                     theme = "colorblind",
                     vsize = 14, 
                     label.cex = 1.1, 
                     mar = c(8,8,8,8), 
                     title = paste0(cond, ": Contemporaneous"), 
                     label.scale = FALSE,
                     maximum = max_r34[[cond]],
                     esize = 10)
  
  btw_plot <- qgraph(between_r34[[cond]], 
                     layout = "circle", 
                     labels = Labels_r34, 
                     theme = "colorblind",
                     vsize = 14, 
                     label.cex = 1.1, 
                     mar = c(8,8,8,8), 
                     title = paste0(cond, ": Between-Subjects"), 
                     label.scale = FALSE,
                     maximum = max_r34[[cond]],
                     esize = 10)
  
  qgraph(tem_plot,
         filetype = "pdf",
         filename = paste0("./output/plots/mltgrp_panelgvar_circle_all_to_s6_", cond, "_panelPDC_r34"))
  qgraph(con_plot,
         filetype = "pdf",
         filename = paste0("./output/plots/mltgrp_panelgvar_circle_all_to_s6_", cond, "_panelPCC_r34"))
  qgraph(btw_plot,
         filetype = "pdf",
         filename = paste0("./output/plots/mltgrp_panelgvar_circle_all_to_s6_", cond, "_panelBetween_r34"))
  
  pdf(paste0("./output/plots/mltgrp_panelgvar_circle_all_to_s6_", cond, "_multipanel.pdf"), 
      width = 12, 
      height = 4)
  layout(t(1:3))
  qgraph(tem_plot)
  box("figure")
  qgraph(con_plot)
  box("figure")
  qgraph(btw_plot)
  box("figure")
  dev.off()
}

# Run function for each condition

plot_cir(temporal_r34, contemporaneous_r34, between_r34, max_r34, "POSITIVE")
plot_cir(temporal_r34, contemporaneous_r34, between_r34, max_r34, "FIFTY_FIFTY")
plot_cir(temporal_r34, contemporaneous_r34, between_r34, max_r34, "NEUTRAL")

# ---------------------------------------------------------------------------- #
# Create tables ----
# ---------------------------------------------------------------------------- #

# TODO: Define function to create and write tables per condition

make_tbls <- function(temporal_r34, contemporaneous_r34, between_r34, cond) {
  Labels_r34 <-  c("Anx. Freq.", "Anx. Sev.", "Sit. Avoid", "Work Imp.", 
                   "Soc. Imp.","Neg. Bias", "Pos. Bias")
  
  # Temporal
  
  temporal_table_r34 <- temporal_r34[[cond]]
  temporal_table_r34[temporal_table_r34 == 0] <- NA
  rownames(temporal_table_r34) <- colnames(temporal_table_r34) <- Labels_r34
  temporal_table_r34 <- round(temporal_table_r34, 2)
  
  # Contemporaneous, with marginal correlations in upper tri
  
  contemporaneous_table_r34 <- contemporaneous_r34[[cond]]
  
  contemporaneous_cors_r34 <- cov2cor(getmatrix(analysis_r34, 
                                                "sigma_zeta_within",
                                                group = cond))
  
  contemporaneous_table_r34[upper.tri(contemporaneous_table_r34)] <- 
    contemporaneous_cors_r34[upper.tri(contemporaneous_cors_r34)]
  
  contemporaneous_table_r34[contemporaneous_table_r34 == 0] <- NA
  
  rownames(contemporaneous_table_r34) <- colnames(contemporaneous_table_r34) <- Labels_r34
  contemporaneous_table_r34 <- round(contemporaneous_table_r34, 2)
  
  # Between, with marginal correlations in upper tri
  
  between_table_r34 <- between_r34[[cond]]
  
  between_cors_r34 <- cov2cor(getmatrix(analysis_r34, 
                                        "sigma_zeta_between",
                                        group = cond))
  
  between_table_r34[upper.tri(between_table_r34)] <- 
    between_cors_r34[upper.tri(between_cors_r34)]
  
  between_table_r34[between_table_r34 == 0] <- NA
  
  rownames(between_table_r34) <- colnames(between_table_r34) <- Labels_r34
  between_table_r34 <- round(between_table_r34, 2)
  
  # Export tables to CSV
  
  sink(file = paste0("./output/tables/table_mltgrp_panelgvar_all_to_s6_", cond, ".csv"))
  
  print(paste("Temporal:"))
  write.csv(temporal_table_r34)
  
  print(paste("Contemporaneous:"))
  write.csv(contemporaneous_table_r34)
  
  print(paste("Between-Subjects:"))
  write.csv(between_table_r34)
  
  sink()
}

# Run function for each condition

make_tbls(temporal_r34, contemporaneous_r34, between_r34, "POSITIVE")
make_tbls(temporal_r34, contemporaneous_r34, between_r34, "FIFTY_FIFTY")
make_tbls(temporal_r34, contemporaneous_r34, between_r34, "NEUTRAL")