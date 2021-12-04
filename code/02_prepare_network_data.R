# ---------------------------------------------------------------------------- #
# Prepare Network Data
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

# No packages loaded

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
# Restrict time points ----
# ---------------------------------------------------------------------------- #

# TODO




