# ---------------------------------------------------------------------------- #
# Prepare Item-Level Data
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

# The present script reads a clean data file "R34_FinalData_New_v02.csv" obtained 
# from the OSF project (https://osf.io/3b67v) for the MindTrails Managing Anxiety 
# Study main outcomes paper (https://doi.org/g62s) and raw data files obtained from 
# Sonia Baee, who obtained them from Claudia Calicho-Mamani. The clean data file
# is generated from "FinalData-28Feb20_v02.csv" using "Script1_DataPrep.R" from
# the OSF project. "FinalData-28Feb20_v02.csv", which is not on the OSF project,
# is a newer version of the data file "FinalData-28Feb20.csv" outputted by the
# script "R34.ipynb" in the study's Data Cleaning folder (https://bit.ly/3CLi5It) 
# on GitHub. Sonia Baee indicated that the files in the Data Cleaning folder and 
# the Main Outcomes folder (https://bit.ly/3FHRz4G) on GitHub not up to date given
# file losses when switching laptops. Code to generate "FinalData-28Feb20_v02.csv"
# is not presently available as a result.

# Given that "R34_FinalData_New_v02.csv" is the most cleaned data file available
# but lacks item-level data, the present script uses it as a starting point for
# extracting relevant item-level data from the raw data files.

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

# Import cleaned data file

cln_dat <- read.csv("./data/clean/R34_FinalData_New_v02.csv")

# Obtain file names of raw CSV data files

raw_data_dir <- paste0(wd_dir, "/data/raw")
raw_filenames <- list.files(raw_data_dir, pattern = "*.csv", full.names = FALSE)

# Import raw data files and store them in list

raw_dat <- lapply(paste0(raw_data_dir, "/", raw_filenames), read.csv)

# Name each raw data file in list

split_char <- ".csv"
names(raw_dat) <- unlist(lapply(raw_filenames,
                                function(f) {
                                  unlist(strsplit(f,
                                                  split = split_char,
                                                  fixed = FALSE))[1]
                                }))

# ---------------------------------------------------------------------------- #
# Investigate "FIXED" tables ----
# ---------------------------------------------------------------------------- #

# Three raw data files have "FIXED" in filenames ("CIHS_Feb_02_2019_FIXED.csv",
# "DASS21_DS_recovered_Feb_02_2019_FIXED.csv", "TaskLog_final_FIXED.csv"). Only
# "CIHS_Feb_02_2019_FIXED.csv" has a corresponding file without "FIXED" in the
# filename: "CIHS_recovered_Feb_02_2019.csv".

names(raw_dat)[grep("FIXED", names(raw_dat))]

# Compare "CIHS_recovered_Feb_02_2019" with "CIHS_Feb_02_2019_FIXED". All their
# shared columns have the same values, but "CIHS_Feb_02_2019_FIXED" includes two
# additional columns ("datetime" and "corrected_session"). Thus, use the "FIXED"
# version and remove "CIHS_recovered_Feb_02_2019" from the list.

shared_cihs_cols <- intersect(names(raw_dat$CIHS_recovered_Feb_02_2019), 
                              names(raw_dat$CIHS_Feb_02_2019_FIXED))
all(raw_dat$CIHS_recovered_Feb_02_2019[, shared_cihs_cols] == 
      raw_dat$CIHS_Feb_02_2019_FIXED[, shared_cihs_cols])

added_cihs_cols <- setdiff(names(raw_dat$CIHS_Feb_02_2019_FIXED), 
                           names(raw_dat$CIHS_recovered_Feb_02_2019))

head(raw_dat$CIHS_Feb_02_2019_FIXED[, c("date", "datetime")])

table(raw_dat$CIHS_Feb_02_2019_FIXED$session)
table(raw_dat$CIHS_Feb_02_2019_FIXED$corrected_session)

raw_dat$CIHS_recovered_Feb_02_2019 <- NULL

# ---------------------------------------------------------------------------- #
# Investigate other repeated tables ----
# ---------------------------------------------------------------------------- #

# Compare "DD_recovered_Feb_02_2019" with "DD_FU_recovered_Feb_02_2019". The "FU"
# version does not contain rows where "session" is "PRE" and does not contain the
# columns "average_amount" and "average_freq". After recoding "True" and "False"
# to "TRUE" and "FALSE" for columns "q1_noAns" and "q2_noAns" in the "FU" version,
# all entries on shared columns for rows where "session" is not "PRE" are identical.
# Thus, use the full version and remove "DD_FU_recovered_Feb_02_2019" from the list.

table(raw_dat$DD_recovered_Feb_02_2019$session, useNA = "always")
table(raw_dat$DD_FU_recovered_Feb_02_2019$session, useNA = "always")

raw_dat$DD_FU_recovered_Feb_02_2019$q1_noAns[raw_dat$DD_FU_recovered_Feb_02_2019$q1_noAns == 
                                               "True"] <- "TRUE"
raw_dat$DD_FU_recovered_Feb_02_2019$q1_noAns[raw_dat$DD_FU_recovered_Feb_02_2019$q1_noAns == 
                                               "False"] <- "FALSE"
raw_dat$DD_FU_recovered_Feb_02_2019$q2_noAns[raw_dat$DD_FU_recovered_Feb_02_2019$q2_noAns == 
                                               "True"] <- "TRUE"
raw_dat$DD_FU_recovered_Feb_02_2019$q2_noAns[raw_dat$DD_FU_recovered_Feb_02_2019$q2_noAns == 
                                               "False"] <- "FALSE"

shared_dd_cols <- intersect(names(raw_dat$DD_recovered_Feb_02_2019),
                            names(raw_dat$DD_FU_recovered_Feb_02_2019))

all(raw_dat$DD_recovered_Feb_02_2019[raw_dat$DD_recovered_Feb_02_2019$session != "PRE", 
                                     shared_dd_cols] == 
      raw_dat$DD_FU_recovered_Feb_02_2019[, shared_dd_cols])

added_dd_cols <- setdiff(names(raw_dat$DD_recovered_Feb_02_2019), 
                           names(raw_dat$DD_FU_recovered_Feb_02_2019))

raw_dat$DD_FU_recovered_Feb_02_2019 <- NULL

# ---------------------------------------------------------------------------- #
# Shorten table names ----
# ---------------------------------------------------------------------------- #

names(raw_dat)[names(raw_dat) == "AnxietyTriggers_recovered_Feb_02_2019"] <- "anxiety_triggers"
names(raw_dat)[names(raw_dat) == "BBSIQ_recovered_Feb_02_2019"] <- "bbsiq"
names(raw_dat)[names(raw_dat) == "CC_recovered_Feb_02_2019"] <- "cc"
names(raw_dat)[names(raw_dat) == "CIHS_Feb_02_2019_FIXED"] <- "cihs"
names(raw_dat)[names(raw_dat) == "Credibility_recovered_Feb_02_2019"] <- "credibility"
names(raw_dat)[names(raw_dat) == "DASS21_AS_recovered_Feb_02_2019"] <- "dass21_as"
names(raw_dat)[names(raw_dat) == "DASS21_DS_recovered_Feb_02_2019_FIXED"] <- "dass21_ds"
names(raw_dat)[names(raw_dat) == "DD_recovered_Feb_02_2019"] <- "dd"
names(raw_dat)[names(raw_dat) == "Demographic_recovered_Feb_02_2019"] <- "demographic"
names(raw_dat)[names(raw_dat) == "EmailLogDAO_recovered_Feb_02_2019"] <- "email_log"
names(raw_dat)[names(raw_dat) == "GiftLogDAO_recovered_Feb_02_2019"] <- "gift_log"
names(raw_dat)[names(raw_dat) == "ImageryPrime_recovered_Feb_02_2019"] <- "imagery_prime"
names(raw_dat)[names(raw_dat) == "ImpactAnxiousImagery_recovered_Feb_02_2019"] <- "impact_anxious_imagery"
names(raw_dat)[names(raw_dat) == "MentalHealthHxTx_recovered_Feb_02_2019"] <- "mental_health_hx_tx"
names(raw_dat)[names(raw_dat) == "MultiUserExperience_recovered_Feb_02_2019"] <- "multi_user_experience"
names(raw_dat)[names(raw_dat) == "OA_recovered_Feb_02_2019"] <- "oa"
names(raw_dat)[names(raw_dat) == "ParticipantExportDAO_recovered_Feb_02_2019"] <- "participant_export_dao"
names(raw_dat)[names(raw_dat) == "QOL_recovered_Feb_02_2019"] <- "qol"
names(raw_dat)[names(raw_dat) == "ReturnIntention_recovered_Feb_02_2019"] <- "return_intention"
names(raw_dat)[names(raw_dat) == "RR_recovered_Feb_02_2019"] <- "rr"
names(raw_dat)[names(raw_dat) == "SUDS_recovered_Feb_02_2019"] <- "suds"
names(raw_dat)[names(raw_dat) == "TaskLog_final_FIXED"] <- "task_log"
names(raw_dat)[names(raw_dat) == "TrialDAO_recovered_Feb_02_2019"] <- "trial_dao"
names(raw_dat)[names(raw_dat) == "VisitDAO_recovered_Feb_02_2019"] <- "visit_dao"

# ---------------------------------------------------------------------------- #
# Restrict raw data tables to those relevant to present analysis ----
# ---------------------------------------------------------------------------- #

sel_tbls <- c("bbsiq", "dass21_as", "dass21_ds", "dd", "demographic", "oa",
              "participant_export_dao", "qol", "rr", "task_log")
sel_dat <- raw_dat[names(raw_dat) %in% sel_tbls]

# ---------------------------------------------------------------------------- #
# Identify potential columns that index participants ----
# ---------------------------------------------------------------------------- #

# Manually inspect column names to identify those that may index participants

lapply(sel_dat, identify_cols, grep_pattern = "")

potential_id_cols <- c("id", "participantDAO", "participantdao_id", "participantRSA",
                       "participantId", "participant")
pattern <- paste(potential_id_cols, collapse = "|")
lapply(sel_dat, identify_cols, grep_pattern = pattern)

# Table "oa" has "participantDAO" and "participantRSA". The values are identical, 
# with 6 exceptions where "participantRSA" has 344-character values. Given that 
# "participantDAO" is present in these cases, use this to index participants.

nrow(sel_dat$oa[sel_dat$oa$participantDAO != sel_dat$oa$participantRSA, ])
table(nchar(sel_dat$oa$participantRSA))

# In table "dass21_as", at eligibility screening eligible participants receive
# a "participantDAO" and ineligible participants do not. Neither eligible nor 
# ineligible participants receive "participantRSA" at screening. At other time 
# points, "participantDAO" is identical to "participantRSA" with 1 exception:
# "participantDAO" 29 at "session" "POST" has a 344-character "participantRSA".
# Use "participantDAO" to index participants.

dass21_as_eligibility <- 
  sel_dat$dass21_as[sel_dat$dass21_as$session %in% c("ELIGIBLE", ""), ]

nrow(dass21_as_eligibility) ==
  nrow(dass21_as_eligibility[dass21_as_eligibility$session == "ELIGIBLE" &
                             !is.na(dass21_as_eligibility$participantDAO), ]) +
  nrow(dass21_as_eligibility[dass21_as_eligibility$session == "" &
                             is.na(dass21_as_eligibility$participantDAO), ])

nrow(dass21_as_eligibility) ==
  nrow(dass21_as_eligibility[dass21_as_eligibility$participantRSA == "", ])

dass21_as_other <- 
  sel_dat$dass21_as[!(sel_dat$dass21_as$session %in% c("ELIGIBLE", "")), ]

nrow(dass21_as_other[dass21_as_other$participantDAO != 
                       dass21_as_other$participantRSA, ])
dass21_as_other[dass21_as_other$participantDAO != 
                  dass21_as_other$participantRSA, 
                "participantDAO"]

# Tables other than "oa" and "dass21_as" have some 344-character "participantRSA"
# values too. However, none of these 344-character values appears more than once 
# within or across these tables. It's unclear what the 344-character value means.

oa_participantRSA_chars <- 
  sel_dat$oa[nchar(sel_dat$oa$participantRSA) == 344, "participantRSA"]
dass21_as_participantRSA_chars <- 
  sel_dat$dass21_as[nchar(sel_dat$dass21_as$participantRSA) == 344, "participantRSA"]

bbsiq_participantRSA_chars <- 
  sel_dat$bbsiq[nchar(sel_dat$bbsiq$participantRSA) == 344, "participantRSA"]
dass21_ds_participantRSA_chars <- 
  sel_dat$dass21_ds[nchar(sel_dat$dass21_ds$participantRSA) == 344, "participantRSA"]
dd_participantRSA_chars <- 
  sel_dat$dd[nchar(sel_dat$dd$participantRSA) == 344, "participantRSA"]
demographic_participantRSA_chars <- 
  sel_dat$demographic[nchar(sel_dat$demographic$participantRSA) == 344, "participantRSA"]
qol_participantRSA_chars <- 
  sel_dat$qol[nchar(sel_dat$qol$participantRSA) == 344, "participantRSA"]
rr_participantRSA_chars <- 
  sel_dat$rr[nchar(sel_dat$rr$participantRSA) == 344, "participantRSA"]

sum(table(c(oa_participantRSA_chars,
            dass21_as_participantRSA_chars,
            bbsiq_participantRSA_chars,
            dass21_ds_participantRSA_chars,
            dd_participantRSA_chars,
            demographic_participantRSA_chars,
            qol_participantRSA_chars,
            rr_participantRSA_chars)) > 1)

# ---------------------------------------------------------------------------- #
# Correct selected "participantRSA" values ----
# ---------------------------------------------------------------------------- #

# Sonia Baee's code "R34_cleaning_script.R" corrects "participantRSA" for certain
# row numbers in some tables. Namely, it corrects "participantRSA" to 532 in row 
# 68 of "bbsiq", "dass_ds", and "rr" tables and in row 50 of "dd" table. It also
# corrects "participantRSA" to 534, 535, and 536 for rows 81, 88, and 99 of "qol"
# table. Obtain the original "participantRSA" for each of these row numbers.

bbsiq_row68_participantRSA <- sel_dat$bbsiq[68, ]$participantRSA
dass21_ds_row68_participantRSA <- sel_dat$dass21_ds[68, ]$participantRSA
rr_row68_participantRSA <- sel_dat$rr[68, ]$participantRSA
dd_row50_participantRSA <- sel_dat$dd[50, ]$participantRSA

qol_row81_participantRSA <- sel_dat$qol[81, ]$participantRSA
qol_row88_participantRSA <- sel_dat$qol[88, ]$participantRSA
qol_row89_participantRSA <- sel_dat$qol[89, ]$participantRSA

# Explicitly name the relevant "participantRSA" values and subsequently refer to
# these rather than to row numbers

bbsiq_row68_participantRSA == 
  paste0("bkUHY+xsxxIbBEF2CHPAiUooa8HWC/zDEYhXS1ktfHTmWPmkHl8Cdw+XCQ+7V+4V+lfnTq",
         "+UnxHtwpqZ9nvDOaCYujSb/xr5NXbMs+B8MEqkTFi8XmX46SCXwfEoteuxi5bnK9tIxcWZ",
         "C1mQkyfhphR3Hw+uZTfetH0qOZu1QlsHPnAMIjZDuipAEJCddUPZG+fFhWrsQKFz99YY1a",
         "d88g37Mdy8wMSFNxDHybkIFQbY6o9clvaAH+F5nfCBPgqx978yjkSeGP8cq58N3PViF/ED",
         "sT1WgZePkyylPdezs8PorJrLhtbHscCqL15QEIOd4t6DQf48sps9IR3GzFE1kg==")
dass21_ds_row68_participantRSA ==
  paste0("nmm8V5qydpeqiJK3opOxycYiXr2jYvMmvaF+sTc3FPXiz4HL1VrP/OBI7+eURNleliPsiv",
         "QpfdyHK9eX3ddpTJq8i25yjL+Isj17YyetkJ/C0DPYMloZDvK5LqCqoHYYA7bohUFHxc+f",
         "6wTgnuK7u+igZlI2N+b72M50P2sgxltVVgKZNxgYbahF3bJX8bvgANsSg40HD6DZTjvABd",
         "+6NeL/wRIwKMCCFnn870X8xeQnq4c3m9DcTP19dHPMj3uThFbe0VRPnBWqvO0LKDERwAzT",
         "Jn5Xk0mRFoU3fIKxBycy4oILofwK9TaTJaQbR1z+wkdkZGntkp3waNkgSJDXtQ==")
rr_row68_participantRSA ==
  paste0("j8xMRa/5b5fAWDc9k3XmKa5gc70yFtOEQeDmhbdhV0PVXbkastBIXfZNmJB5dC0NA+2q/p",
         "VkQ9nbyUCVXTb4K28ZT1X7msw7o/DDVJoDeUtnFY44UXaMKb8grRK3wqc/2A7sGI22IS0h",
         "ZxkhBPDz7PLvg7WRicUrf+joAJzoUcR96U8Da8TskUNeJKnx5ExsDCXSVDz7juecNg7mG9",
         "+f+8dSg7J8MK8I+P/AAVd6UlGhTi/Y63dmyV04tIA+tTWFjwsNNtQO6x65jhd+2EGAqeW3",
         "VNPcF9McqbKzteJEKW1i1hPpcA5wOfNpOwpCcddNgpwNUa5UQpS4vCWiBj/FtQ==")
dd_row50_participantRSA ==
  paste0("JxkDrxWqMNiIBetFR/Mjs+K0wIZBoYFXt8/urWIXZCj+09Z0Nc0VrHPzHq1gc1xYnA3gZg",
         "YEpDv+RHOx/foAFE04wj/XWtKt53Eu2rMNmpl5SnFHh0WZQadmrtyDqAeIsyh8Pi/9zpba",
         "sLulygiCAG10Vja0biESqixbOAaSLX7vXU4MBAAGOFGSPr5q2/bX3V7zbwqLYsG4Z7/RjS",
         "eR9jjzGFJeXG1SXyHVavwZWOKT2lyic7gnpp+WrMBbn66frS3ekE1cFrNv2VrWtRHTanlr",
         "78T6eCDLO+IykOo589YjtFJ1WKv6LbzdPct23tAiOFTyAFq56IrovojfH9ecgQ==")

qol_row81_participantRSA ==
  paste0("wbEBZcHlnsFt7G9Nery6IGcDyekYMRdfF9JY+86NsApN+xq72V9YVux2UoxOLHQ32GM+pf",
         "DdILnnXgUsCZ7/627m/HQcG5IDuJH5NzoRFO0Keg7+n8tIEG1oasegk4eUcSqts/Sottvc",
         "qZUd07bgy62kFtt/r0ANtUCIK/lFtJrwOD3GnsbUNSQPml2YvOD4QDDwN9jhLOzCaFN+n+",
         "2AEAkgMSV7wBqvQUv1Nq1zMM86h3+9+zfgNM5g1fBEtU05QnrmnkqO/EW4kGb72BfUEywJ",
         "HunLBwNqY71sTCkPlYnsWNtsdGm5hrrVlI+OXvbaIVa49ufK5YhGc3wcuDcuuA==")
qol_row88_participantRSA ==
  paste0("hfuLJcIoHPV/fo4cwqrI0O1bLuy/V8Me5gNXRpu3lKlg4m/BC0F0GfIV85jn7XIvCKDmcP",
         "RSzeE/nz7MDSo9oMMj+7JHcwPBqThTvWiF0MB4mkawa2BKWOOp77f0emHfhaYsU0hVcIYA",
         "IgQFx2yM3sb5xC8MCOB4bX31/q8mh7fpJFR3YGRKDsChafZdrLi5vA7jda9f1Xw8I5FZFY",
         "Vf/BjCadtETdliIrD/l33431Tjs4AGPs24aGd3XPGEh8DOCB3wlxjGTk8IK/msG2xbkTGf",
         "BGSzfCTjGiPJ/hE28PgGoJZhdhaD457wxGKPJc4hoc/zVXNYZllrhCuKKLh7Ew==")
qol_row89_participantRSA ==
  paste0("MPDLN5nIJels9xMMcfpKALfJCaZO0V0OTMwfPW+0e250c44/iTKXZeL2mL6rM3kTQfetnv",
         "FFp+tp+Dfa/XsArlxktjPq7N3oGMdZAPOAqvmUuSEvc2ooNH1y/GLxiJpr+oAwNIlqPpWE",
         "NVeWhYIJ6Mz4n4NvAqBtfebxso8CoSjxJNzt2tnncy884tlA9C7t0scOKHynrDN0mpaURI",
         "wj08vnuqodfz8ctojDvt9NByFLjY6qc/M9q+KnRTxgxwaFjWuimjwe5nsWGRJXiWrIaF5a",
         "6XEAxk2RkcqRP65Z8CQ+7FElWjVqws/770zdwQQce6Z0/fRM7qkTCYNOlxMurw==")

# Correct these "participantRSA" values

sel_dat$bbsiq[sel_dat$bbsiq$participantRSA == bbsiq_row68_participantRSA, 
              "participantRSA"] <- 532
sel_dat$dass21_ds[sel_dat$dass21_ds$participantRSA == dass21_ds_row68_participantRSA, 
                  "participantRSA"] <- 532
sel_dat$rr[sel_dat$rr$participantRSA == rr_row68_participantRSA, 
           "participantRSA"] <- 532
sel_dat$dd[sel_dat$dd$participantRSA == dd_row50_participantRSA, 
           "participantRSA"] <- 532

sel_dat$qol[sel_dat$qol$participantRSA == qol_row81_participantRSA,
            "participantRSA"] <- 534
sel_dat$qol[sel_dat$qol$participantRSA == qol_row88_participantRSA,
            "participantRSA"] <- 535
sel_dat$qol[sel_dat$qol$participantRSA == qol_row89_participantRSA,
            "participantRSA"] <- 536

# Correct other "participantRSA" values that are explicitly named in Sonia Baee's 
# code "R34_cleaning_script.R" for "demographic" table

demographic_participantRSA_NvorEv <-
  paste0("NvorEV/x1Rn9FC1yuqkfDjcDlSRj9b8Y3ji78LPNHI8vzrvOJdQjRTpAY+Mn/C8pToWmJ",
         "mY4luS8P3Rfcfk/hcZGxUwzWpbkJjuKA0q8E4Y6IPxJmLdiO2zFYh8BJKEr4j8pidKyMo",
         "oi2REL5hcNeJKPpkId4R0iBpLgPXfapA+XnRqHQftVZo+JL1TD8eP362JX7A0L90Oxpqk",
         "uOp3lnEce2ox6vcc4fwTsvoWy+utocR+Fp7Pgct/wBIdjvKhSLJvabMK7Ffglk9OrAIXu",
         "xj77+n+sbsW4PoGc9a2UZu5D18WKGrUW6kdLTBCDqE/DyWjcLXEGWCwCwlTdRtPtjQ==")
demographic_participantRSA_j5J6cs <-
  paste0("j5J6cs0Yromqio+zSNruW1qA1athdsLV4AQf6AnWnhVS2M+SYJ3bEgBG9GPQDZMLN4Ttq",
         "yk76HO+x1QXEdL/FtgkyyYy82aR4j5lYJ0IeamTIK5atLtT0MZJQqhhehLp5y97TvJjoo",
         "tnTj4OV+l0iaxAMKxzcIqadlZ7FzD5uWDTqJCzxX/Bny7V01irJfqHgEEpxNwwLmJttjg",
         "fr3ifWWv7JseWIgPPIH1spGFUWN127vCvKmZ3bmbYCLq119q5Nn11jKe+Oq2oSvv7HPcz",
         "Db/knU3Uu7Y8Js01nK+VtX6kFYAACzwZcu6GN+KGuUUexlxMJXCaLBQvEdU3Iy7lVQ==")
demographic_participantRSA_VS9LPK <-
  paste0("VS9LPKcqvwakVj2Orq/tFLN+JB9nmbaZ5kqg0gDXF7CQ4NKpey6kMSnEErlvr3UrnJKYg",
         "KGEAk3wi0zijUZDNS5+Nzys2L7ynDGXQoE61RO07m2dqMsxZGHgv8qadNtfIyKxpjYaNo",
         "blp0d3VJ8sSce08XBmVxMfr7nLbDwC60Zm1vsBajDqX073gc3U35cq+n4vNG5v1t1zPQF",
         "qeRvXsTHGc4naet1I8PeQEcLtQJN2daxdfgZplToaOzEW7KGhdzgYYRyTwWCvwh8wHUcd",
         "mV4dW7VG3oGLvz35wLYVkOlpZMgZ+5eAr9an4JDaXwR1e3CeHYJHRDxSGibqK3rl/Q==")

sel_dat$demographic[sel_dat$demographic$participantRSA == demographic_participantRSA_NvorEv,
                    "participantRSA"] <- 534
sel_dat$demographic[sel_dat$demographic$participantRSA == demographic_participantRSA_j5J6cs,
                    "participantRSA"] <- 535
sel_dat$demographic[sel_dat$demographic$participantRSA == demographic_participantRSA_VS9LPK,
                    "participantRSA"] <- 536

# Note that even after correcting these "participantRSA" values, some 344-character
# values remain in all of these tables except "demographic"

# ---------------------------------------------------------------------------- #
# Define "participant_id" across tables ----
# ---------------------------------------------------------------------------- #

# In "participant_export_dao" table, rename "id" as "participant_id", which is
# the only column that can index participants

names(sel_dat$participant_export_dao)[names(sel_dat$participant_export_dao) ==
                                        "id"] <- "participant_id"

# In "oa" and "dass21_as" tables, rename "participantDAO" as "participant_id"
# and remove "participantRSA"

names(sel_dat$oa)[names(sel_dat$oa) == "participantDAO"] <- "participant_id"
names(sel_dat$dass21_as)[names(sel_dat$dass21_as) == "participantDAO"] <- "participant_id"

sel_dat$oa$participantRSA <- NULL
sel_dat$dass21_as$participantRSA <- NULL

# In "task_log" table, rename "participantdao_id" as "participant_id", which is
# the only column that can index participants

names(sel_dat$task_log)[names(sel_dat$task_log) == "participantdao_id"] <- "participant_id"

# Rename "participantRSA" as "participant_id" in tables where "participantRSA" is
# the only column that can index participants

participantRSA_index_tbls <- c("bbsiq", "dass21_ds", "dd", "demographic",
                               "qol", "rr")

for (i in 1:length(sel_dat)) {
  if (names(sel_dat)[i] %in% participantRSA_index_tbls) {
    names(sel_dat[[i]])[names(sel_dat[[i]]) == "participantRSA"] <- "participant_id" 
  }
}

# ---------------------------------------------------------------------------- #
# Identify and recode time stamp and date columns ----
# ---------------------------------------------------------------------------- #

# Identify columns containing "date" in each table

lapply(sel_dat, identify_cols, grep_pattern = "date")

# Note: "task_log" table has multiple columns containing "date". "date_completed" 
# is identical to "datetime" but different from "corrected_datetime".

all(sel_dat$task_log$date_completed == sel_dat$task_log$datetime)
all(sel_dat$task_log$date_completed == sel_dat$task_log$corrected_datetime)
nrow(sel_dat$task_log[sel_dat$task_log$date_completed != 
                        sel_dat$task_log$corrected_datetime, ])

# View structure of columns containing "date" in each table

view_date_str <- function(df, df_name) {
  print(paste0("Table: ", df_name))
  cat("\n")
  
  df_colnames <- colnames(df)
  date_cols <- grep("date", df_colnames)
  
  if (length(date_cols) != 0) {
    for (i in date_cols) {
      print(paste0(df_colnames[i]))
      str(df[, i])
      print(paste0("Number NA: ", sum(is.na(df[, i]))))
      print(paste0("Number blank: ", sum(df[, i] == "")))
      print(paste0("Number 555: ", sum(df[, i] == 555, na.rm = TRUE)))
      print("Number of characters: ")
      print(table(nchar(df[, i])))
    }
  } else {
    print('No columns containing "date" found.')
  }
  
  cat("----------")
  cat("\n")
}

invisible(mapply(view_date_str, df = sel_dat, df_name = names(sel_dat)))

# TODO: The following columns across tables are system-generated date and time 
# stamps. For now, assume all of these are in EST time zone (note: EST, or UTC - 
# 5, all year, not "America/New York", which switches between EST and EDT). But
# check this with Sonia/Dan, especially for "task_log" columns.

system_date_time_cols <- c("date", "date_completed", "datetime", "corrected_datetime")





# Define function to reformat system-generated time stamps and add time zone

recode_date_time_timezone <- function(dat) {
  for (i in 1:length(dat)) {
    table_name <- names(dat[i])
    colnames <- names(dat[[i]])
    target_colnames <- colnames[colnames %in% system_date_time_cols]
    
    if (length(target_colnames) != 0) {
      for (j in 1:length(target_colnames)) {
        # Create new variable for POSIXct values. Recode blanks as NA.
        
        POSIXct_colname <- paste0(target_colnames[j], "_as_POSIXct")
        
        dat[[i]][, POSIXct_colname] <- dat[[i]][, target_colnames[j]]
        dat[[i]][dat[[i]][, POSIXct_colname] == "", POSIXct_colname] <- NA
        
        # Specify time zone as "EST" for all system-generated time stamps. Specify 
        # nonstandard formats to parse columns, which are not in standard formats.
        
        if (table_name == "task_log" & target_colnames[j] %in% 
              c("date_completed", "datetime", "corrected_datetime")) {
          dat[[i]][, POSIXct_colname] <- 
            as.POSIXct(dat[[i]][, POSIXct_colname],
                       tz = "EST", 
                       format = "%m/%d/%Y %H:%M")
        } else {
          dat[[i]][, POSIXct_colname] <-
          as.POSIXct(dat[[i]][, POSIXct_colname], 
                     tz = "EST",
                     format = "%a, %d %b %Y %H:%M:%S %z")
        }
      }
    }
  }
  
  return(dat)
}

# Run function

sel_dat <- recode_date_time_timezone(sel_dat)

# ---------------------------------------------------------------------------- #
# Identify and rename session-related columns ----
# ---------------------------------------------------------------------------- #

# Identify columns containing "session" in each table

lapply(sel_dat, identify_cols, grep_pattern = "session")

# View structure of columns containing "session" in each table

view_session_str <- function(dat) {
  for (i in 1:length(dat)) {
    print(paste0("Table: ", names(dat[i])))
    cat("\n")
    
    colnames <- names(dat[[i]])
    session_colnames <- colnames[grep("session", colnames, ignore.case = TRUE)]
    
    if (length(session_colnames) != 0) {
      for (j in 1:length(session_colnames)) {
        session_colname <- session_colnames[j]
        session_colname_class <- class(dat[[i]][, session_colname])
        
        print(paste0(session_colname))
        print(paste0("Class: ", session_colname_class))
        
        if (length(unique(dat[[i]][, session_colname])) > 20) {
          print("First 20 unique levels: ")
          print(unique(dat[[i]][, session_colname])[1:20])
        } else {
          print("All unique levels: ")
          print(unique(dat[[i]][, session_colname]))
        }
        
        print(paste0("Number NA: ", sum(is.na(dat[[i]][, session_colname]))))
        
        if (!("POSIXct" %in% session_colname_class)) {
          print(paste0("Number blank: ", sum(dat[[i]][, session_colname] == "")))
          print(paste0("Number 555: ", sum(dat[[i]][, session_colname] == 555,
                                           na.rm = TRUE)))
        }
        
        cat("\n")
      }
    } else {
      print('No columns containing "session" found.')
      cat("\n")
    }
    
    cat("----------")
    cat("\n", "\n")
  }
}

view_session_str(sel_dat)

# Rename selected session-related columns to clarify conflated content of some
# columns and to enable consistent naming (i.e., "session_only") across tables
# for columns that contain only session information

# Given that "session" column in "dass21_as" table contains both session 
# information and eligibility status, rename column to reflect this.
# Also create new column "session_only" with "ELIGIBLE" and "" entries of
# original "session" column recoded as "Eligibility" (to reflect that these
# entries were collected at the eligibility screener time point.

table(sel_dat$dass21_as$session)

# Rename remaining "session_name" columns (in "task_log" table) and remaining 
# "session" columns to "session_only" to reflect that they contain only session 
# information. Do not rename "currentSession" column of "participant_export_dao" 
# table because "currentSession" does not index entries within participants; 
# rather, it reflects participants' current sessions.

for (i in 1:length(sel_dat)) {
  if (names(sel_dat[i]) %in% "dass21_as") {
    names(sel_dat[[i]])[names(sel_dat[[i]]) == "session"] <- "session_and_eligibility_status"
    
    sel_dat[[i]][, "session_only"] <- sel_dat[[i]][, "session_and_eligibility_status"]
    sel_dat[[i]][sel_dat[[i]][, "session_only"] %in% c("ELIGIBLE", ""), 
                 "session_only"] <- "Eligibility"
  } else if (names(sel_dat[i]) %in% "task_log") {
    names(sel_dat[[i]])[names(sel_dat[[i]]) == "session_name"] <- "session_only"
  } else if ("session" %in% names(sel_dat[[i]])) {
    names(sel_dat[[i]])[names(sel_dat[[i]]) == "session"] <- "session_only"
  }
}

# ---------------------------------------------------------------------------- #
# Filter raw data ----
# ---------------------------------------------------------------------------- #

# Identify participant IDs in clean data

cln_participant_ids <- cln_dat$participantID

# TODO: 36 participants are missing. Asked Sonia about this on 11/24/21.

length(setdiff(cln_participant_ids, sel_dat$participant_export_dao$participant_id))





# Define function to filter raw data based on participantIDs in clean data

filter_all_data <- function(dat, participant_ids) {
  output <- vector("list", length(dat))
  
  for (i in 1:length(dat)) {
    if ("participant_id" %in% names(dat[[i]])) {
      output[[i]] <- dat[[i]][dat[[i]][, "participant_id"] %in% participant_ids, ]
    } else {
      output[[i]] <- dat[[i]]
    }
  }
  
  names(output) <- names(dat)
  return(output)
}

# Run function

flt_dat <- filter_all_data(sel_dat, cln_participant_ids)

# ---------------------------------------------------------------------------- #
# Check session and date values ----
# ---------------------------------------------------------------------------- #

# TODO: Seem to be discrepancies between "session" and date-related values in 
# "task_log" table and "session" and "date" in other tables (e.g., "oa"). For
# example, for participant 623 "task_log" contains "OA" at "SESSION1" but this
# data is not in "oa" table. In addition, "task_log" says "OA" at "SESSION2"
# was completed on 9/12/16, but "oa" says "SESSION2" was done on 9/10/16.

# View(flt_dat$oa[flt_dat$oa$participant_id == 623, ])
# View(flt_dat$task_log[flt_dat$task_log$participant_id == 623 &
#                         flt_dat$task_log$task_name == "OA", ])





# ---------------------------------------------------------------------------- #
# Check for multiple entries ----
# ---------------------------------------------------------------------------- #

# For rows that have duplicated values on every meaningful column (i.e., every
# column except "X" and "id"), keep only the last row after sorting by "id" for
# tables that contain "id" (throw error if "participant_export_dao", which lacks
# "id", contains multiple rows per "participant_id", in which case it will need 
# to be sorted and have its rows consolidated).

for (i in 1:length(flt_dat)) {
  meaningful_cols <- names(flt_dat[[i]])[!(names(flt_dat[[i]]) %in% c("X", "id"))]
  
  if (names(flt_dat[i]) %in% "participant_export_dao") {
    if (nrow(flt_dat[[i]]) != length(unique(flt_dat[[i]][, "participant_id"]))) {
      error(paste0("Unexpectedly, table ", names(flt_dat[i]), 
                   "contains multiple rows for at least one participant_id"))
    }
  } else if ("id" %in% names(flt_dat[[i]])) {
    flt_dat[[i]] <- flt_dat[[i]][order(flt_dat[[i]][, "id"]), ]
    
    flt_dat[[i]] <- flt_dat[[i]][!duplicated(flt_dat[[i]][, meaningful_cols],
                                             fromLast = TRUE), ]
  } else {
    stop(paste0("Table ", names(flt_dat[i]), "needs to be checked for duplicates"))
  }
}

# Define functions to report duplicated rows on target columns. "report_dups_df"
# is used within "report_dups_list".

report_dups_df <- function(df, df_name, target_cols, index_col) {
  duplicated_rows <- df[duplicated(df[, target_cols]), ]
  if (nrow(duplicated_rows) > 0) {
    cat(nrow(duplicated_rows), "duplicated rows for table:", df_name)
    cat("\n")
    cat("With these '", index_col, "': ", duplicated_rows[, index_col])
    cat("\n-------------------------\n")
  } else {
    cat("No duplicated rows for table:", df_name)
    cat("\n-------------------------\n")
  }
}

report_dups_list <- function(dat) {
  for (i in 1:length(dat)) {
    if (names(dat[i]) %in% "participant_export_dao") {
      report_dups_df(dat[[i]], 
                     names(dat[i]), 
                     "participant_id", 
                     "participant_id")
    } else if (names(dat[i]) == "dass21_as") {
      duplicated_rows_eligibility <- 
        dat[[i]][dat[[i]][, "session_only"] == "Eligibility" &
                   (duplicated(dat[[i]][, c("participant_id",
                                            "session_only",
                                            "sessionId")])), ]
      duplicated_rows_other <-
        dat[[i]][dat[[i]][, "session_only"] != "Eligibility" &
                   (duplicated(dat[[i]][, c("participant_id",
                                            "session_only")])), ]
      duplicated_rows <- rbind(duplicated_rows_eligibility, duplicated_rows_other)
      
      if (nrow(duplicated_rows) > 0) {
        p_ids <- duplicated_rows_eligibility[!is.na(duplicated_rows_eligibility$participant_id),
                                             "participant_id"]
        s_ids <- duplicated_rows_eligibility[is.na(duplicated_rows_eligibility$participant_id),
                                             "sessionId"]
        
        cat(nrow(duplicated_rows_eligibility), 
            "duplicated rows at Eligibility for table:", names(dat[i]))
        cat("\n")
        cat("With these ", length(p_ids), "'participant_id' (where available): ", p_ids)
        cat("\n")
        cat("And with ", length(s_ids), "'sessionId' (where 'participant_id' unavailable)")
        cat("\n")
        cat(nrow(duplicated_rows_other), 
            "duplicated rows at other time points for table:", names(dat[i]))
        if (nrow(duplicated_rows_other) > 0) {
          cat("\n")
          cat("With these 'participant_id': ", duplicated_rows_other$participant_id)
        }
        cat("\n-------------------------\n")
      } else {
        cat("No duplicated rows for table:", names(dat[i]))
        cat("\n-------------------------\n")
      }
    } else if (names(dat[i]) == "task_log") {
      report_dups_df(dat[[i]], 
                     names(dat[i]), 
                     c("participant_id", 
                       "session_only", 
                       "task_name"), 
                     "participant_id")
      
      duplicated_rows_dass21_as_eligibility <- 
        dat[[i]][duplicated(dat[[i]][, c("participant_id", 
                                         "session_only", 
                                         "task_name")]) &
                   dat[[i]][, "session_only"] == "Eligibility" &
                   dat[[i]][, "task_name"] == "DASS21_AS", ]
      duplicated_rows_other_suds <- 
        dat[[i]][duplicated(dat[[i]][, c("participant_id", 
                                         "session_only", 
                                         "task_name")]) &
                   !(dat[[i]][, "session_only"] == "Eligibility" &
                       dat[[i]][, "task_name"] == "DASS21_AS") &
                   (dat[[i]][, "task_name"] == "SUDS"), ]
      duplicated_rows_other_nonsuds <- 
        dat[[i]][duplicated(dat[[i]][, c("participant_id", 
                                         "session_only", 
                                         "task_name")]) &
                   !(dat[[i]][, "session_only"] == "Eligibility" &
                       dat[[i]][, "task_name"] == "DASS21_AS") &
                   !(dat[[i]][, "task_name"] == "SUDS"), ]
      if (nrow(duplicated_rows_dass21_as_eligibility) > 0 |
          nrow(duplicated_rows_other_suds) > 0 |
          nrow(duplicated_rows_other_nonsuds) > 0) {
        cat(nrow(duplicated_rows_dass21_as_eligibility),
            "duplicated rows for DASS21_AS at Eligibility in table:", names(dat[i]))
        cat("\n")
        cat("With these 'participant_id': ", duplicated_rows_dass21_as_eligibility$participant_id)
        cat("\n", "\n")
        cat(nrow(duplicated_rows_other_suds),
            "duplicated rows for other, SUDS tasks in table:", names(dat[i]))
        cat("\n")
        cat("With these 'participant_id': ", duplicated_rows_other_suds$participant_id)
        cat("\n", "\n")
        cat(nrow(duplicated_rows_other_nonsuds),
            "duplicated rows for other, non-SUDS tasks in table:", names(dat[i]))
        cat("\n")
        cat("With these 'participant_id': ", duplicated_rows_other_nonsuds$participant_id)
        cat("\n-------------------------\n")
      }
    } else {
      report_dups_df(dat[[i]],
                     names(dat[i]), 
                     c("participant_id", 
                       "session_only"), 
                     "participant_id")
    }
  }
}

# Run function

report_dups_list(flt_dat)

# ---------------------------------------------------------------------------- #
# Resolve multiple entries ----
# ---------------------------------------------------------------------------- #

# TODO: Describe what was done in R34 main outcomes paper. Script "R34.ipynb"
# seems to inadequately sort by "date" when keeping most recent entry, but see
# if it's an issue once the scale scores are regenerated (not an issue for the
# participant below).

# test <- flt_dat$oa[flt_dat$oa$participant_id == 620, ]
# View(test[order(test$id), ])
# 
# View(cln_dat[cln_dat$participantID == 620, ])





# dass21_as:
#   - R34_cleaning_script says all duplicates are multiple screening attempts, assumes entries are in order by date, and keeps the last row
# - Other script says "get the latest entry for each participant" (does so by most recent date for each session)
# 
# oa:
#   41 duplicated rows for table: oa
# With these ' participant_id ':  8 14 17 432 425 16 421 445 485 539 532 623 620 683 598 712 674 582 669 745 625 684 723 644 627 659 590 731 708 701 727 687 662 541 719 597 710 640 552 435 600
# 
# - R34_cleaning_script says all duplicates are for scam/test accounts, but none of these are test accounts based on Sonia's manually identified list of test accounts
# - Other script says "get the latest entry for each participant" (does so by most recent date for each session) and notes that 1767 has duplicated values at PRE (not above) and then takes the sum to generate the score
# 
# ## Get the latest entry for each participant
# oasis_analysis = oasis_analysis.sort_values(by="date").groupby(['participantID','session']).tail(1)
# 
# task_log:
# - Multiple SUDS at Sessions 1, 3, 6, 8 with no "tag"
# - Other duplicates (likely due to repeating training)





# Sort tables chronologically. Given that "dd" table lacks unique "id" for each row, 
# sort tables on "date_as_POSIXct". Given that "task_log" table lacks "date_as_POSIXct", 
# sort it on "corrected_datetime_as_POSIXct". Given that "participant_export_dao" 
# table lacks date-related columns, sort it by "participant_id".

for (i in 1:length(flt_dat)) {
  if (names(flt_dat[i]) == "participant_export_dao") {
    flt_dat[[i]] <- flt_dat[[i]][order(flt_dat[[i]][, "participant_id"]), ]
  } else if (names(flt_dat[i]) == "task_log") {
    flt_dat[[i]] <- flt_dat[[i]][order(flt_dat[[i]][, "corrected_datetime_as_POSIXct"]), ]
  } else {
    flt_dat[[i]] <- flt_dat[[i]][order(flt_dat[[i]][, "date_as_POSIXct"]), ]
  }
}

# Define functions to keep the most recent entry where duplicated rows exist on 
# target columns. "keep_recent_entry_df" is used within "keep_recent_entry_list".

keep_recent_entry_df <- function(df, target_cols) {
  output <- df[!duplicated(df[, target_cols], fromLast = TRUE), ]
  
  return(output)
}

keep_recent_entry_list <- function(dat) {
  output <- vector("list", length(dat))
  
  for (i in 1:length(dat)) {
    if (names(dat[i]) %in% "participant_export_dao") {
      output[[i]] <- keep_recent_entry_df(dat[[i]],
                                          "participant_id")
    } else if (names(dat[i]) == "dass21_as") {
      recent_entry_eligibility <- 
        keep_recent_entry_df(dat[[i]][dat[[i]][, "session_only"] == "Eligibility", ],
                             c("participant_id",
                               "session_only",
                               "sessionId"))
      recent_entry_other <-
        keep_recent_entry_df(dat[[i]][dat[[i]][, "session_only"] != "Eligibility", ],
                             c("participant_id",
                               "session_only"))
      output[[i]] <- rbind(recent_entry_eligibility, recent_entry_other)
    } else if (names(dat[i]) == "task_log") {
      # Do not remove multiple entries from "task_log" table
      
      output[[i]] <- dat[[i]]
    } else {
      output[[i]] <- keep_recent_entry_df(dat[[i]],
                                          c("participant_id", 
                                            "session_only"))
    }
    rownames(output[[i]]) <- 1:nrow(output[[i]])
  }
  names(output) <- names(dat)

  return(output)
}

# Run function

flt_dat <- keep_recent_entry_list(flt_dat)

# ---------------------------------------------------------------------------- #
# Compute selected scores ----
# ---------------------------------------------------------------------------- #

# For now, compute scores only for "oa" and "rr" tables

# Define scale items

oa_items <- c("anxious_freq", "anxious_sev", "avoid", "interfere", "interfere_social")

rr_nf_items <- names(flt_dat$rr)[grep("NF", names(flt_dat$rr))]
rr_ns_items <- names(flt_dat$rr)[grep("NS", names(flt_dat$rr))]
rr_pf_items <- names(flt_dat$rr)[grep("PF", names(flt_dat$rr))]
rr_ps_items <- names(flt_dat$rr)[grep("PS", names(flt_dat$rr))]
rr_items <- c(rr_nf_items, rr_ns_items, rr_pf_items, rr_ps_items)

# Recode 555 and -1 ("prefer not to answer") as NA

sum(flt_dat$oa[, oa_items] == 555)
sum(flt_dat$rr[, rr_items] == -1)

flt_dat$oa[, oa_items][flt_dat$oa[, oa_items] == 555] <- NA
flt_dat$rr[, rr_items][flt_dat$rr[, rr_items] == -1] <- NA

# TODO: Compute scale scores. Note: The summation used for "oa_total" below is 
# incorrect because it treats an item-level responses of NA as responses of 0,
# but this seems to be what was done in "R34.ipynb" cleaning script.

# View(flt_dat$oa[rowSums(is.na(flt_dat$oa[, oa_items])) > 0, ])
sum(rowSums(is.na(flt_dat$oa[, oa_items])) == ncol(flt_dat$oa[, oa_items]))

flt_dat$oa$oa_total <- rowSums(flt_dat$oa[ , oa_items], na.rm = TRUE)

flt_dat$rr$rr_nf_mean <- rowMeans(flt_dat$rr[, rr_nf_items], na.rm = TRUE)
flt_dat$rr$rr_ns_mean <- rowMeans(flt_dat$rr[, rr_ns_items], na.rm = TRUE)
flt_dat$rr$rr_pf_mean <- rowMeans(flt_dat$rr[, rr_pf_items], na.rm = TRUE)
flt_dat$rr$rr_ps_mean <- rowMeans(flt_dat$rr[, rr_ps_items], na.rm = TRUE)





# ---------------------------------------------------------------------------- #
# Extract clean data into separate tables ----
# ---------------------------------------------------------------------------- #

# Remove renamed columns, computed columns (besides "score"-related columns), and 
# dummy-coded condition columns, which were all defined in "Script1_DataPrep.R", 
# which is on the OSF project for the main outcomes paper (https://osf.io/5wscm/)

renamed_cols <- c("RRNegative_PRE", "RRNegative_SESSION3", "RRNegative_SESSION6",
                  "RRPositive_PRE", "RRPositive_SESSION3", "RRPositive_SESSION6",
                  "OASISScore_PRE", "OASISScore_SESSION1", "OASISScore_SESSION2",
                  "OASISScore_SESSION3", "OASISScore_SESSION4", "OASISScore_SESSION5",
                  "OASISScore_SESSION6")
computed_cols <- c("negativeBBSIQ_PRE", "negativeBBSIQ_SESSION3", "negativeBBSIQ_SESSION6")
dummy_cols <- c("ANXIETY", "POSITIVE", "FIFTY_FIFTY", "POSITIVEANXIETY",
                "POSITIVENEUTRAL", "FIFTY_FIFTYANXIETY", "FIFTY_FIFTYNEUTRAL",
                "NONEANXIETY")

cln_dat <- cln_dat[, names(cln_dat)[!(names(cln_dat) %in% c(renamed_cols,
                                                            computed_cols,
                                                            dummy_cols))]]

# Remove dropout-related columns, which seem to have been created in some version
# of "R34.ipynb", which, albeit outdated, is in the Data Cleaning folder on GitHub
# (https://github.com/jwe4ec/MT-Data-ManagingAnxietyStudy/tree/master/Data%20Cleaning)

dropout_cols <- names(cln_dat)[grep("_dropout", names(cln_dat))]

cln_dat <- cln_dat[, names(cln_dat)[!(names(cln_dat) %in% dropout_cols)]]

# Rename "participantID" as "participant_id"

names(cln_dat)[names(cln_dat) == "participantID"] <- "participant_id"

# Extract column names for separate tables

bbsiq_cols <- names(cln_dat)[grep("bbsiq_", names(cln_dat), ignore.case = TRUE)]
dass_as_cols <- names(cln_dat)[grep("dass_as_", names(cln_dat))]
dass_ds_cols <- names(cln_dat)[grep("dass_ds_", names(cln_dat))]
demographic_cols <- names(cln_dat)[grep("demographic_", names(cln_dat))]
oa_cols <- names(cln_dat)[grep("oasis_", names(cln_dat), ignore.case = TRUE)]
participant_cols <- names(cln_dat[grep("participant", names(cln_dat))])
rr_cols <- names(cln_dat)[grep("RR_", names(cln_dat), ignore.case = TRUE)]

setdiff(names(cln_dat), c(bbsiq_cols, dass_as_cols, dass_ds_cols, demographic_cols,
                          oa_cols, participant_cols, rr_cols))

# Extract data into separate tables

table_cols <- list(bbsiq = bbsiq_cols,
                   dass_as = dass_as_cols,
                   dass_ds = dass_ds_cols,
                   demographic = demographic_cols,
                   oa = oa_cols,
                   participant = participant_cols,
                   rr = rr_cols)

sep_dat_wide <- vector("list", length = length(table_cols))

for (i in 1:length(table_cols)) {
  if ("participant_id" %in% table_cols[[i]]) {
    sep_dat_wide[[i]] <- cln_dat[, table_cols[[i]]]
  } else {
    sep_dat_wide[[i]] <- cln_dat[, c("participant_id", table_cols[[i]])]
  }
}

names(sep_dat_wide) <- names(table_cols)

# ---------------------------------------------------------------------------- #
# Convert clean data tables into long format ----
# ---------------------------------------------------------------------------- #

# Alphabetize columns of each table and then sort them by time point, retaining
# "participant_id" as the first column

for (i in 1:length(sep_dat_wide)) {
  sep_dat_wide[[i]] <- sep_dat_wide[[i]][, sort(names(sep_dat_wide[[i]]))]

  if (names(sep_dat_wide[i]) %in% c("demographic", "participant")) {
    sep_dat_wide[[i]] <- sep_dat_wide[[i]]
  } else {
    sep_dat_wide[[i]] <- 
      sep_dat_wide[[i]][, c("participant_id",
                            names(sep_dat_wide[[i]])[grep("ELIGIBLE", names(sep_dat_wide[[i]]))],
                            names(sep_dat_wide[[i]])[grep("PRE",      names(sep_dat_wide[[i]]))],
                            names(sep_dat_wide[[i]])[grep("SESSION1", names(sep_dat_wide[[i]]))],
                            names(sep_dat_wide[[i]])[grep("SESSION2", names(sep_dat_wide[[i]]))],
                            names(sep_dat_wide[[i]])[grep("SESSION3", names(sep_dat_wide[[i]]))],
                            names(sep_dat_wide[[i]])[grep("SESSION4", names(sep_dat_wide[[i]]))],
                            names(sep_dat_wide[[i]])[grep("SESSION5", names(sep_dat_wide[[i]]))],
                            names(sep_dat_wide[[i]])[grep("SESSION6", names(sep_dat_wide[[i]]))],
                            names(sep_dat_wide[[i]])[grep("SESSION7", names(sep_dat_wide[[i]]))],
                            names(sep_dat_wide[[i]])[grep("SESSION8", names(sep_dat_wide[[i]]))],
                            names(sep_dat_wide[[i]])[grep("POST",     names(sep_dat_wide[[i]]))])]
  }
}

# Identify repeated-measures columns for each score

bbsiq_physical_score_cols <- names(sep_dat_wide$bbsiq)[grep("bbsiq_physical_score", 
                                                            names(sep_dat_wide$bbsiq))]
bbsiq_threat_score_cols <- names(sep_dat_wide$bbsiq)[grep("bbsiq_threat_score", 
                                                          names(sep_dat_wide$bbsiq))]

dass_as_score_cols <- names(sep_dat_wide$dass_as)[grep("dass_as_score", 
                                                       names(sep_dat_wide$dass_as))]

dass_ds_score_cols <- names(sep_dat_wide$dass_ds)[grep("dass_ds_score", 
                                                       names(sep_dat_wide$dass_ds))]

oasis_score_cols <- names(sep_dat_wide$oa)[grep("oasis_score", 
                                                names(sep_dat_wide$oa))]

RR_negative_nf_score_cols <- names(sep_dat_wide$rr)[grep("RR_negative_nf_score", 
                                                         names(sep_dat_wide$rr))]
RR_negative_ns_score_cols <- names(sep_dat_wide$rr)[grep("RR_negative_ns_score", 
                                                         names(sep_dat_wide$rr))]
RR_positive_pf_score_cols <- names(sep_dat_wide$rr)[grep("RR_positive_pf_score", 
                                                         names(sep_dat_wide$rr))]
RR_positive_ps_score_cols <- names(sep_dat_wide$rr)[grep("RR_positive_ps_score", 
                                                         names(sep_dat_wide$rr))]

# Convert repeated-measures tables to long format

sep_dat <- vector("list", length = length(sep_dat_wide))
names(sep_dat) <- names(sep_dat_wide)

sep_dat$demographic <- sep_dat_wide$demographic
sep_dat$participant <- sep_dat_wide$participant

sep_dat$bbsiq <- reshape(sep_dat_wide$bbsiq, 
                         direction = "long",
                         idvar = "participant_id",
                         timevar = "session",
                         varying = list(bbsiq_physical_score = bbsiq_physical_score_cols,
                                        bbsiq_threat_score = bbsiq_threat_score_cols),
                         v.names = c("bbsiq_physical_score", "bbsiq_threat_score"),
                         times = c("PRE", "SESSION3", "SESSION6", "SESSION8", "POST"))

sep_dat$dass_as <- reshape(sep_dat_wide$dass_as, 
                           direction = "long",
                           idvar = "participant_id",
                           timevar = "session",
                           varying = list(dass_as_score = dass_as_score_cols),
                           v.names = "dass_as_score",
                           times = c("ELIGIBLE", "SESSION8", "POST"))

sep_dat$dass_ds <- reshape(sep_dat_wide$dass_ds, 
                           direction = "long",
                           idvar = "participant_id",
                           timevar = "session",
                           varying = list(dass_ds_score = dass_ds_score_cols),
                           v.names = "dass_ds_score",
                           times = c("PRE", "SESSION3", "SESSION6", "SESSION8", "POST"))

sep_dat$oa <- reshape(sep_dat_wide$oa, 
                      direction = "long",
                      idvar = "participant_id",
                      timevar = "session",
                      varying = list(oasis_score = oasis_score_cols),
                      v.names = "oasis_score",
                      times = c("PRE", "SESSION1", "SESSION2", "SESSION3", "SESSION4",
                                "SESSION5", "SESSION6", "SESSION7", "SESSION8", "POST"))

sep_dat$rr <- reshape(sep_dat_wide$rr, 
                      direction = "long",
                      idvar = "participant_id",
                      timevar = "session",
                      varying = list(RR_negative_nf_score = RR_negative_nf_score_cols,
                                     RR_negative_ns_score = RR_negative_ns_score_cols,
                                     RR_positive_pf_score = RR_positive_pf_score_cols,
                                     RR_positive_ps_score = RR_positive_ps_score_cols),
                      v.names = c("RR_negative_nf_score", "RR_negative_ns_score",
                                  "RR_positive_pf_score", "RR_positive_ps_score"),
                      times = c("PRE", "SESSION3", "SESSION6", "SESSION8", "POST"))

# Remove rows that contain only NAs for scores

for (i in 1:length(sep_dat)) {
  if (names(sep_dat[i]) %in% c("demographic", "participant")) {
    sep_dat[[i]] <- sep_dat[[i]]
  } else {
    score_cols <- names(sep_dat[[i]])[grep("_score", names(sep_dat[[i]]))]
    
    sep_dat[[i]] <- sep_dat[[i]][complete.cases(sep_dat[[i]][, score_cols]), ]
  }
}

# ---------------------------------------------------------------------------- #
# Compare datasets ----
# ---------------------------------------------------------------------------- #

# Define lists with corresponding tables

flt_dat_comp <- flt_dat[c("bbsiq", "dass21_as", "dass21_ds", "demographic", 
                          "oa", "participant_export_dao", "rr")]
sep_dat_comp <- sep_dat[c("bbsiq", "dass_as", "dass_ds", "demographic", 
                          "oa", "participant", "rr")]

# Compare participant_ids for each table. "bbsiq" table in "flt_dat" list has
# 1 more "participant_id" than that in "sep_dat" list. By contrast, "dass_as",
# "oa", and "participant" tables in "sep_dat" list have 36, 37, and 36 more
# participant_ids than their corresponding tables in "flt_dat" list.

diff_participant_ids <- function(x, y) {
  setdiff(x$participant_id, y$participant_id)
}
mapply(diff_participant_ids, flt_dat_comp, sep_dat_comp)
mapply(diff_participant_ids, sep_dat_comp, flt_dat_comp)

length_diff_participant_ids <- function(x, y) {
  length(setdiff(x$participant_id, y$participant_id))
}
mapply(length_diff_participant_ids, flt_dat_comp, sep_dat_comp)
mapply(length_diff_participant_ids, sep_dat_comp, flt_dat_comp)

# Restrict to shared participant_ids in each table and confirm that is so

flt_dat_comp_rest <- vector("list", length(flt_dat_comp))
sep_dat_comp_rest <- vector("list", length(sep_dat_comp))

names(flt_dat_comp_rest) <- names(flt_dat_comp)
names(sep_dat_comp_rest) <- names(sep_dat_comp)

for (i in 1:length(flt_dat_comp)) {
  shared_participant_ids <- intersect(flt_dat_comp[[i]][, "participant_id"], 
                                      sep_dat_comp[[i]][, "participant_id"])
  flt_dat_comp_rest[[i]] <- flt_dat_comp[[i]][flt_dat_comp[[i]][, "participant_id"] %in% 
                                                shared_participant_ids, ]
  sep_dat_comp_rest[[i]] <- sep_dat_comp[[i]][sep_dat_comp[[i]][, "participant_id"] %in% 
                                                shared_participant_ids, ]
}

all(mapply(length_diff_participant_ids, flt_dat_comp_rest, sep_dat_comp_rest) == 0)
all(mapply(length_diff_participant_ids, sep_dat_comp_rest, flt_dat_comp_rest) == 0)

# TODO: Sort tables by "participant_id" and "session"

for (i in 1:length(flt_dat_comp_rest)) {
  if ("session_only" %in% names(flt_dat_comp_rest[[i]])) {
    flt_dat_comp_rest[[i]] <- 
      flt_dat_comp_rest[[i]][order(flt_dat_comp_rest[[i]][, "participant_id"],
                                   flt_dat_comp_rest[[i]][, "session_only"]), ]
  } else {
    flt_dat_comp_rest[[i]] <- 
      flt_dat_comp_rest[[i]][order(flt_dat_comp_rest[[i]][, "participant_id"]), ]
  }
}

for (i in 1:length(sep_dat_comp_rest)) {
  if ("session" %in% names(sep_dat_comp_rest[[i]])) {
    sep_dat_comp_rest[[i]] <- 
      sep_dat_comp_rest[[i]][order(sep_dat_comp_rest[[i]][, "participant_id"],
                                   sep_dat_comp_rest[[i]][, "session"]), ]
  } else {
    sep_dat_comp_rest[[i]] <- 
      sep_dat_comp_rest[[i]][order(sep_dat_comp_rest[[i]][, "participant_id"]), ]
  }
}

# Compare numbers of observations. They differ between datasets.

for (i in 1:length(flt_dat_comp_rest)) {
  print(paste0(names(flt_dat_comp_rest[i]), " has ", 
               nrow(flt_dat_comp_rest[[i]]), " rows in raw data and ",
               nrow(sep_dat_comp_rest[[i]]), " rows in clean data"))
}

# Compute "session_only" in clean dataset

for (i in 1:length(sep_dat_comp_rest)) {
  if (!(names(sep_dat_comp_rest[i]) %in% c("demographic", "participant"))) {
    if (names(sep_dat_comp_rest[i]) == "dass_as") {
      names(sep_dat_comp_rest[[i]])[names(sep_dat_comp_rest[[i]]) == "session"] <-
            "session_and_eligibility_status"
      
      sep_dat_comp_rest[[i]][, "session_only"] <- 
        sep_dat_comp_rest[[i]][, "session_and_eligibility_status"]
      sep_dat_comp_rest[[i]][sep_dat_comp_rest[[i]][, "session_only"] == "ELIGIBLE", 
                             "session_only"] <- "Eligibility"
    } else {
      names(sep_dat_comp_rest[[i]])[names(sep_dat_comp_rest[[i]]) == "session"] <-
        "session_only"
    }
  }
}

# TODO: Use natural join to restrict to shared time points for "oa" table. The
# discrepancies for 19 participants seem due to potential recoding of "session"
# to resolve multiple entries in clean data (vs. keeping the most recent entry).
# Asked Sonia on 12/3/21 to confirm how she resolved multiple entries.

merge_oa <- merge(flt_dat_comp_rest$oa, 
                  sep_dat_comp_rest$oa,
                  by = c("participant_id", "session_only"),
                  all = FALSE)

sum(merge_oa$oa_total == merge_oa$oasis_score)
sum(merge_oa$oa_total != merge_oa$oasis_score)

discrep_ids <- merge_oa[merge_oa$oa_total != merge_oa$oasis_score, ]$participant_id
length(unique(discrep_ids))

# View(sel_dat$oa[sel_dat$oa$participant_id == 14, ]) # Missing S5 but has two S8
# View(flt_dat$oa[flt_dat$oa$participant_id == 14, ])
# View(sep_dat$oa[sep_dat$oa$participant_id == 14, ])
# View(merge_oa[merge_oa$participant_id     == 14, ])
# 
# View(sel_dat$oa[sel_dat$oa$participant_id == 16, ]) # Missing S5 but has two S8
# View(flt_dat$oa[flt_dat$oa$participant_id == 16, ])
# View(sep_dat$oa[sep_dat$oa$participant_id == 16, ])
# View(merge_oa[merge_oa$participant_id     == 16, ])
# 
# View(sel_dat$oa[sel_dat$oa$participant_id == 17, ]) # Missing S4 but has two S8
# View(flt_dat$oa[flt_dat$oa$participant_id == 17, ])
# View(sep_dat$oa[sep_dat$oa$participant_id == 17, ])
# View(merge_oa[merge_oa$participant_id     == 17, ])
# 
# View(sel_dat$oa[sel_dat$oa$participant_id == 421, ]) # Missing S1 but has two S8
# View(flt_dat$oa[flt_dat$oa$participant_id == 421, ])
# View(sep_dat$oa[sep_dat$oa$participant_id == 421, ])
# View(merge_oa[merge_oa$participant_id     == 421, ])
# 
# View(sel_dat$oa[sel_dat$oa$participant_id == 425, ]) # Missing S1 but has two S8
# View(flt_dat$oa[flt_dat$oa$participant_id == 425, ])
# View(sep_dat$oa[sep_dat$oa$participant_id == 425, ])
# View(merge_oa[merge_oa$participant_id     == 425, ])
# 
# View(sel_dat$oa[sel_dat$oa$participant_id == 432, ]) # Missing S1 but has two S8
# View(flt_dat$oa[flt_dat$oa$participant_id == 432, ])
# View(sep_dat$oa[sep_dat$oa$participant_id == 432, ])
# View(merge_oa[merge_oa$participant_id     == 432, ])
# 
# View(sel_dat$oa[sel_dat$oa$participant_id == 445, ]) # Missing S1 but has two S8
# View(flt_dat$oa[flt_dat$oa$participant_id == 445, ])
# View(sep_dat$oa[sep_dat$oa$participant_id == 445, ])
# View(merge_oa[merge_oa$participant_id     == 445, ])
# 
# View(sel_dat$oa[sel_dat$oa$participant_id == 485, ]) # Missing S1 but has two S8
# View(flt_dat$oa[flt_dat$oa$participant_id == 485, ])
# View(sep_dat$oa[sep_dat$oa$participant_id == 485, ])
# View(merge_oa[merge_oa$participant_id     == 485, ])
# 
# View(sel_dat$oa[sel_dat$oa$participant_id == 532, ]) # Missing S1 but has two S8
# View(flt_dat$oa[flt_dat$oa$participant_id == 532, ])
# View(sep_dat$oa[sep_dat$oa$participant_id == 532, ])
# View(merge_oa[merge_oa$participant_id     == 532, ])
# 
# View(sel_dat$oa[sel_dat$oa$participant_id == 539, ]) # Missing S1 but has two S8
# View(flt_dat$oa[flt_dat$oa$participant_id == 539, ])
# View(sep_dat$oa[sep_dat$oa$participant_id == 539, ])
# View(merge_oa[merge_oa$participant_id     == 539, ])
# 
# View(sel_dat$oa[sel_dat$oa$participant_id == 541, ]) # Missing S1 but has two S8
# View(flt_dat$oa[flt_dat$oa$participant_id == 541, ])
# View(sep_dat$oa[sep_dat$oa$participant_id == 541, ])
# View(merge_oa[merge_oa$participant_id     == 541, ])
# 
# View(sel_dat$oa[sel_dat$oa$participant_id == 552, ]) # Missing S1 but has two S4
# View(flt_dat$oa[flt_dat$oa$participant_id == 552, ])
# View(sep_dat$oa[sep_dat$oa$participant_id == 552, ])
# View(merge_oa[merge_oa$participant_id     == 552, ])
# 
# View(sel_dat$oa[sel_dat$oa$participant_id == 582, ]) # Missing S1 but has two S7
# View(flt_dat$oa[flt_dat$oa$participant_id == 582, ])
# View(sep_dat$oa[sep_dat$oa$participant_id == 582, ])
# View(merge_oa[merge_oa$participant_id     == 582, ])
# 
# View(sel_dat$oa[sel_dat$oa$participant_id == 598, ]) # Missing S1 but has two S5
# View(flt_dat$oa[flt_dat$oa$participant_id == 598, ])
# View(sep_dat$oa[sep_dat$oa$participant_id == 598, ])
# View(merge_oa[merge_oa$participant_id     == 598, ])
# 
# View(sel_dat$oa[sel_dat$oa$participant_id == 625, ]) # Missing S1 but has two S4
# View(flt_dat$oa[flt_dat$oa$participant_id == 625, ])
# View(sep_dat$oa[sep_dat$oa$participant_id == 625, ])
# View(merge_oa[merge_oa$participant_id     == 625, ])
# 
# View(sel_dat$oa[sel_dat$oa$participant_id == 627, ]) # Missing S1 but has two S3
# View(flt_dat$oa[flt_dat$oa$participant_id == 627, ])
# View(sep_dat$oa[sep_dat$oa$participant_id == 627, ])
# View(merge_oa[merge_oa$participant_id     == 627, ])
# 
# View(sel_dat$oa[sel_dat$oa$participant_id == 644, ]) # Missing S1 but has two S3
# View(flt_dat$oa[flt_dat$oa$participant_id == 644, ])
# View(sep_dat$oa[sep_dat$oa$participant_id == 644, ])
# View(merge_oa[merge_oa$participant_id     == 644, ])
# 
# View(sel_dat$oa[sel_dat$oa$participant_id == 662, ]) # Missing S1 but has two S3
# View(flt_dat$oa[flt_dat$oa$participant_id == 662, ])
# View(sep_dat$oa[sep_dat$oa$participant_id == 662, ])
# View(merge_oa[merge_oa$participant_id     == 662, ])
# 
# View(sel_dat$oa[sel_dat$oa$participant_id == 684, ]) # Missing S1 but has two S3
# View(flt_dat$oa[flt_dat$oa$participant_id == 684, ])
# View(sep_dat$oa[sep_dat$oa$participant_id == 684, ])
# View(merge_oa[merge_oa$participant_id     == 684, ])

# 41 duplicated rows for table: oa
# With these ' participant_id ':  8, 14, 16, 17, 421, 425, 432, 435, 445, 485, 
# 532, 539, 541, 552, 582, 590, 597, 598, 600, 620, 623, 625, 627, 640, 644, 659, 
# 662, 669, 674, 683, 684, 687, 701, 708, 710, 712, 719, 723, 727, 731, 745

# View(sel_dat$oa[sel_dat$oa$participant_id == 8, ]) # Missing S7 but has two S8
# View(flt_dat$oa[flt_dat$oa$participant_id == 8, ])
# View(sep_dat$oa[sep_dat$oa$participant_id == 8, ])
# View(merge_oa[merge_oa$participant_id     == 8, ])
# 
# View(sel_dat$oa[sel_dat$oa$participant_id == 600, ]) # Missing S1 but has two S2
# View(flt_dat$oa[flt_dat$oa$participant_id == 600, ])
# View(sep_dat$oa[sep_dat$oa$participant_id == 600, ])
# View(merge_oa[merge_oa$participant_id     == 600, ])

# Likely more not shown above





# Use natural join to restrict to shared time points for "rr" table. All scores
# are the same when rounded to 4 decimal places.

merge_rr <- merge(flt_dat_comp_rest$rr, 
                  sep_dat_comp_rest$rr,
                  by = c("participant_id", "session_only"),
                  all = FALSE)

all(sum(round(merge_rr$rr_nf_mean, 4) != round(merge_rr$RR_negative_nf_score, 4)) == 0,
    sum(round(merge_rr$rr_ns_mean, 4) != round(merge_rr$RR_negative_ns_score, 4)) == 0,
    sum(round(merge_rr$rr_pf_mean, 4) != round(merge_rr$RR_positive_pf_score, 4)) == 0,
    sum(round(merge_rr$rr_ps_mean, 4) != round(merge_rr$RR_positive_ps_score, 4)) == 0)

# Until discrepancies with "oa" table are resolved, remove 41 participants with
# multiple entries in "oa" table. After doing so, all scores are the same.

multiple_oa_entry_participant_ids <- 
  c(8, 14, 16, 17, 421, 425, 432, 435, 445, 485, 532, 539, 541, 552, 582, 590, 
    597, 598, 600, 620, 623, 625, 627, 640, 644, 659, 662, 669, 674, 683, 684, 
    687, 701, 708, 710, 712, 719, 723, 727, 731, 745)

merge_oa_rest2 <- merge_oa[!(merge_oa$participant_id %in% 
                               multiple_oa_entry_participant_ids), ]
merge_rr_rest2 <- merge_rr[!(merge_rr$participant_id %in%
                               multiple_oa_entry_participant_ids), ]
participant_raw_rest2 <- 
  flt_dat_comp_rest$participant_export_dao[!(flt_dat_comp_rest$participant_export_dao %in%
                                               multiple_oa_entry_participant_ids), ]
participant_cln_rest2 <- 
  sep_dat_comp_rest$participant[!(sep_dat_comp_rest$participant %in%
                                    multiple_oa_entry_participant_ids), ]

sum(merge_oa_rest2$oa_total != merge_oa_rest2$oasis_score) == 0

all(sum(round(merge_rr_rest2$rr_nf_mean, 4) != round(merge_rr_rest2$RR_negative_nf_score, 4)) == 0,
    sum(round(merge_rr_rest2$rr_ns_mean, 4) != round(merge_rr_rest2$RR_negative_ns_score, 4)) == 0,
    sum(round(merge_rr_rest2$rr_pf_mean, 4) != round(merge_rr_rest2$RR_positive_pf_score, 4)) == 0,
    sum(round(merge_rr_rest2$rr_ps_mean, 4) != round(merge_rr_rest2$RR_positive_ps_score, 4)) == 0)

# Compare CBM condition

sum(participant_raw_rest2$cbmCondition != participant_cln_rest2$participant_cbm_condition)

# ---------------------------------------------------------------------------- #
# Export merged, restricted item-level data ----
# ---------------------------------------------------------------------------- #

write.csv(merge_oa_rest2, file = "./data/intermediate/merge_oa_rest2.csv", 
          row.names = FALSE)
write.csv(merge_rr_rest2, file = "./data/intermediate/merge_rr_rest2.csv", 
          row.names = FALSE)
write.csv(participant_raw_rest2, file = "./data/intermediate/participant_raw_rest2.csv", 
          row.names = FALSE)