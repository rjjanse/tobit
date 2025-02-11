#-----------------------------------------------------------------------------#
# Predicting HRQoL using Tobit regression                                     #
# Roemer J. Janse                                                             #
# Code for cohort derivation                                                  #
# Creation date: 2025-02-11 || Last update:  2025-02-11                       #
#-----------------------------------------------------------------------------#

# 0. Set-up ----
# Load packages
pacman::p_load("conflicted",     # Solve package conflicts
               "dplyr",          # Data wrangling
               "magrittr",       # Efficient pipelines
               "tidyr",          # Pivoting data
               "stringr",        # Character strings
               "rio",            # Load data
               "summarytools"    # Data summaries
)

# Solve conflicts
conflicts_prefer(dplyr::filter)    # Between dplyr & stats

# Set working directory
path <- "C:/users/rjjan/downloads/predictie data OCT2024.Rdata"

# Import data
dat_domestico <- import(path,
                        trust = TRUE)

# 1. Data exploration ---
# Keep only first visits
dat_first <- filter(dat_domestico, mtpnt == 0)

# Remove attributes
dat_first[] <- lapply(dat_first, \(x){attributes(x) <- NULL; x})

# Create data summary
lst_summary <- dfSummary(dat_first)

# 2. Clean up data ----
# Keep relevant columns
dat_cohort <- dat_domestico %>%
    # Variable selection
    select(studynr, mtpnt, mtpnt_dt, prdcat, prev_dial:Height, Weight, resid_diur,
           bpSyst, bpDiast, med_epo, med_iron, hb:DSI_last_30, IHD:CVD_dt2)

# We want to calculate SF-12 after imputation, so for now we need to store all SF-12 questions for
# baseline, 6 months, and 12 months in a single row per individual
dat_cohort <- 
