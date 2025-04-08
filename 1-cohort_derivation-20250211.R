#-----------------------------------------------------------------------------#
# Predicting HRQoL using Tobit regression                                     #
# Roemer J. Janse                                                             #
# Code for cohort derivation                                                  #
# Creation date: 2025-02-11 || Last update:  2025-04-07                       #
#-----------------------------------------------------------------------------#

# 0. Set-up ----
# Load packages
pacman::p_load("conflicted",     # Solve package conflicts
               "dplyr",          # Data wrangling
               "magrittr",       # Efficient pipelines
               "tidyr",          # Pivoting data
               "stringr",        # Character strings
               "purrr",          # Functional programming
               "here",           # Relative directories
               "rio",            # Load data
               "summarytools",   # Data summaries
               "mice",           # Impute data
               "miceadds"        # Adds 2l.pmm for longitudinal imputation
)

# Solve conflicts
conflicts_prefer(dplyr::filter)    # Between dplyr & stats

# Source relevant functions
walk(list.files(here("funs")), ~ source(here(paste0("funs/", .x))))

# Set working directory
path <- "C:/users/rjjanse.lumcnet/onedrive - lumc/research/projects/17. tobit_hrqol/data/"

# Import data
dat_domestico <- import(paste0(path, "predictie data MAR2025.Rdata"),
                        trust = TRUE) %>%
    # Set all columns to lowercase
    set_colnames(tolower(colnames(.)))

# Remove attributes
dat_domestico[] <- lapply(dat_domestico, \(x){attributes(x) <- NULL; x})

# 1. Clean up data ----
# Clean data, categories, and names
dat_cohort <- dat_domestico %>%
    # Fix data pecularities 
    mutate(# Data errors
           sex = if_else(studynr == 1236, 2, sex),
           year_of_birth_baseline = if_else(studynr == 1954, 1972, year_of_birth_baseline),
           # Weird heights of 100 (almost all localised to one centre)
           height = if_else(height == 100, NA, height)) %>%
    # Rename era_prd to pkd
    rename(pkd = era_prd) %>%
    # Arrange per individual
    arrange(studynr) %>%
    # Group per individual
    group_by(studynr) %>%
    # Fill downwards all values that are only collected at baseline
    fill(prev_dial:stop_red, pkd, burgstaat, woonsituatie, .direction = "down") %>%
    # Remove grouping structure again
    ungroup() %>%
    # Calculate age, reduce levels for logicals, and change PKD from character factor to numeric for imputation
    mutate(# Change birth year to a birth date (using half a year to set month and day)
           birth_dt = as.Date(paste0(year_of_birth_baseline, "-07-02")),
           # Calculate age
           age = round(as.numeric(as.Date(dial_dt) - birth_dt) / 365.25),
           # Change acute_pt from logical 1/2 to 0/1
           acute_pt = acute_pt - 1,
           # Change IHD to yes/no
           ihd_predial = if_else(ihd_predial == 2, 1, ihd_predial),
           # Change DM to yes/no
           dm = if_else(dm == 2, 1, dm),
           # Change iron to yes/no
           # We have 8 3's that do not have an assigned value, we put them to missing
           med_iron = case_match(med_iron,
                                 0 ~ 0,
                                 1:2 ~ 1,
                                 3 ~ NA),
           # Set unknowns to NA 
           lvd = if_else(lvd == 2, NA, lvd),                        # Left ventricular dystrophy
           dm_type = if_else(dm_type == 3, NA, dm_type) - 1,        # DM type
           scvd = if_else(scvd == 2, NA, scvd),                     # Systemic disease
           smoking = if_else(smoking == 3, NA, smoking),            # Smoking
           resid_diur = if_else(resid_diur == 2, NA, resid_diur),   # Residual diuresis
           # Set PKD to numeric
           pkd = case_match(pkd,
                            "Diabetes Mellitus" ~ 1,
                            "Familial / hereditary nephropathies" ~ 2,
                            "Glomerular disease" ~ 3,
                            "Hypertension / Renal vascular disease" ~ 4,
                            "Miscellaneous renal disorders" ~ 5,
                            "Other systemic diseases affecting the kidney" ~ 6,
                            "Tubulointerstitial disease" ~ 7,
                            .default = NA),
           # We incorporate values of the DSI into DSI burden
           across(all_of(paste0("dsi_last_", 1:30)), \(x) x = if_else(get(str_replace(cur_column(), "_last", "")) == 0, 0, x)))
    
# Data selection 
dat_cohort %<>%
    # Variable selection
    select(studynr, mtpnt, mtpnt_dt, age, pkd, prev_dial:stop_red, weight, therapie, resid_diur,
           bpsyst, bpdiast, med_epo, med_iron, hb:woonsituatie, sf12_1:dsi_last_30) %>%
    # Remove redundant variables
    # Start therap is inferior to therapie for baseline modality
    # DM type is only filled in for individuals with diabetes, would make imputation weird
    select(-start_therap, -all_of(paste0("dsi_", 1:30)), -dm_type, -year_of_birth_baseline) %>%
    # Rename variables for clarity
    rename(modality = therapie,     # Baseline dialysis modality
           ihd = ihd_predial,       # Ischemic heart disease
           pvd = pvd2,              # General peripheral vascular disease including aortic aneurysm
           cbd = pvd3,              # Cerebrovascular disease
           hmp = pvd4               # Hemiplegia
    )

# Create data summary over first visits
lst_summary_first <- dfSummary(filter(dat_cohort, mtpnt == 0))

# Create data summary over all data
lst_summary_all <- dfSummary(dat_cohort)

# 2. Impute data ----
# Get predictor matrix
mat_prd <- mice(dat_cohort, maxit = 0)[["predictorMatrix"]]

# Define variables that do not have to be imputed
vec_nimp <- c(# Auxiliary variables
              "prev_dial", "prev_ntx", "acute_pt", "malign_nometa", 
              "malign_meta", "lymphoma", "leukemia", "hmp", "scvd",
              "parathyr", "dementia", "pulm", "ulcer", "liver_mild", "liver_modsev",
              "aids", "psych", "resid_diur", "med_epo",
              "med_iron", "chol",
              # Dates
              "mtpnt_dt", "dial_dt", "stop_dt",
              # Meta information
              "studynr", "mtpnt", "stop_red")

# Define variables for longitudinal imputation
vec_limp <- c(# DSI variables
              paste0("dsi_", 1:30),
              # DSI burden variables
              paste0("dsi_last_", 1:30),
              # SF-12 variables
              paste0("sf12_", 1:12),
              # Laboratory values & vitals
              "bpsyst", "bpdiast", "hb", "na", "pth", 
              # Miscellaneous
              "weight", "resid_diur", "med_epo", "med_iron")

# Adjust predictor matrix accordingly
mat_prd[vec_nimp, ] <- 0

# Set the cluster variable for longitudinal imputation
mat_prd[, "studynr"] <- -2

# Get methods vector
vec_mtd <- mice(dat_cohort, maxit = 0)[["method"]]

# Change variables to be longitudinally imputed to longitudinal imputation
vec_mtd <- if_else(names(vec_mtd) %in% vec_limp, "2l.pmm", vec_mtd)

# Reset names of methods vector
names(vec_mtd) <- colnames(mat_prd)

# Start imputation
lst_imp <- mice(dat_cohort,
                m = 10,
                maxit = 50,
                method = vec_mtd,
                predictorMatrix = mat_prd,
                seed = 1)

# Save object
save(lst_imp, file = paste0(path, "dataframes/imputation_object.Rdata"))

# 3. Finalise imputed data ----
# Get complete data
dat_imputed <- complete(lst_imp, 
                        action = "long",
                        include = TRUE) %>%
    # Calculate extra variables
    mutate(# BMI
           bmi = weight / (height / 100) ^ 2,
           # Presence of partner
           partner = if_else(woonsituatie == 2 | burgstaat == 2, 1, 0))

# Update variable types
dat_imputed %<>% 
    # Update types
    mutate(# Factors
           # PKD
           pkd = factor(pkd, 
                        levels = c(1:4, 7, 6, 5),
                        labels = c("Diabetes Mellitus",
                                   "Familial / hereditary nephropathies",
                                   "Glomerular disease",
                                   "Hypertension / Renal vascular disease",
                                   "Tubulointerstitial disease",
                                   "Other systemic diseases affecting the kidney",
                                   "Miscellaneous renal disorders")),
           # Drop unused levels in PKD factor
           pkd = droplevels(pkd),
           # Sex (2 = female)
           sex = factor(sex, labels = c("Male", "Female")),
           # Modality
           modality = factor(modality, labels = c("HD", "PD")),
           # Smoking
           smoking = factor(smoking, labels = c("Never", "Current", "Quit")),
           # Reason for leaving study
           stop_red = factor(stop_red, labels = c("KTx", "Restored kidney function", "Stopping dialysis", "Death", "Withdraw informed consent", "Emigration")),
           # Civil status
           burgstaat = factor(burgstaat, labels = c("Single", "Married/cohabitating", "Divorced", "Widowed")),
           # Living status
           woonsituatie = factor(woonsituatie, labels = c("Alone", "With partner", "With parents", "With family", "Caring home")),
           # Dates
           across(contains("_dt"), \(x) x = as.Date(x, origin = "1970-01-01")))

# Calculate PROMs
dat_imputed %<>%
    # Creating new variables
    mutate(# PCS
           pcs = sf12_v1(sf12_1, sf12_2, sf12_3, sf12_4, sf12_5, sf12_6, 
                         sf12_7, sf12_8, sf12_9, sf12_10, sf12_11, sf12_12, "pcs"),
           # MCS
           mcs = sf12_v1(sf12_1, sf12_2, sf12_3, sf12_4, sf12_5, sf12_6, 
                         sf12_7, sf12_8, sf12_9, sf12_10, sf12_11, sf12_12, "mcs"),
           # Symptom indicators
           across(dsi_last_1:dsi_last_30, 
                  ~ if_else(.x == 0, 0, 1, missing = NA),
                  .names = "dsi_symp_{.col}")) %>%
    # Change DSI symptom column names
    set_colnames(str_replace(colnames(.), "(?<=dsi_symp_)dsi_last_", "")) %>%
    # Call row-wise operations to compute sum per individual
    rowwise() %>%
    # Calculate symptom burden and symptom count
    mutate(# Symptom count
           sc = sum(dsi_symp_1, dsi_symp_2, dsi_symp_3, dsi_symp_4, dsi_symp_5, dsi_symp_6, dsi_symp_7, dsi_symp_8, dsi_symp_9, 
                    dsi_symp_10, dsi_symp_11, dsi_symp_12, dsi_symp_13, dsi_symp_14, dsi_symp_15, dsi_symp_16, dsi_symp_17, 
                    dsi_symp_18, dsi_symp_19, dsi_symp_20, dsi_symp_21, dsi_symp_22, dsi_symp_23, dsi_symp_24, dsi_symp_25, 
                    dsi_symp_26, dsi_symp_27, dsi_symp_28, dsi_symp_29, dsi_symp_30),
           # Symptom burden
           sb = sum(dsi_last_1, dsi_last_2, dsi_last_3, dsi_last_4, dsi_last_5, dsi_last_6, dsi_last_7, dsi_last_8, dsi_last_9, 
                    dsi_last_10, dsi_last_11, dsi_last_12, dsi_last_13, dsi_last_14, dsi_last_15, dsi_last_16, dsi_last_17, 
                    dsi_last_18, dsi_last_19, dsi_last_20, dsi_last_21, dsi_last_22, dsi_last_23, dsi_last_24, dsi_last_25, 
                    dsi_last_26, dsi_last_27, dsi_last_28, dsi_last_29, dsi_last_30),
           # Pain burden to match KDQOL symptoms
           kdqol_last_pain = pmax(dsi_last_15, dsi_last_16, dsi_last_17),
           # Pain indicator to match KDQOL symptoms
           kdqol_symp_pain = pmax(dsi_symp_15, dsi_symp_16, dsi_symp_17),
           # Symptom count for only symptoms corresponding with symptoms in KDQOL (n = 15)
           sc_15 = sum(dsi_symp_2, dsi_symp_5, dsi_symp_6, dsi_symp_8, dsi_symp_9, dsi_symp_11, dsi_symp_12, kdqol_symp_pain,
                       dsi_symp_18, dsi_symp_19, dsi_symp_20, dsi_symp_21, dsi_symp_23, dsi_symp_29, dsi_symp_30),
           # Symptom burden for only symptoms corresponding with symptoms in KDQOL (n = 15)
           sb_15 = sum(dsi_last_2, dsi_last_5, dsi_last_6, dsi_last_8, dsi_last_9, dsi_last_11, dsi_last_12, kdqol_last_pain,
                       dsi_last_18, dsi_last_19, dsi_last_20, dsi_last_21, dsi_last_23, dsi_last_29, dsi_last_30)) %>%
    # Remove row-wise structure
    ungroup()

# Remove visit at 3 months as we do not need it and put outcomes (PCS and MCS in wide format
# Thus, we keep one row per individual with baseline information and their outcomes at 6 and 12 months
dat_imputed %<>%
    # Remove visit 3
    filter(mtpnt != 3) %>%
    # Pivot PCS and MCS to wide format
    pivot_wider(names_from = mtpnt,
                values_from = c(pcs, mcs)) %>%
    # Arrange for grouping
    arrange(.imp, studynr, mtpnt_dt) %>%
    # Group per individual within each imputation
    group_by(.imp, studynr) %>%
    # Fill MCS and PCS upwards
    fill(pcs_0:mcs_12, .direction = "up") %>%
    # Keep first row per individual
    slice(1L) %>%
    # Remove grouping structure
    ungroup() %>%
    # Create indicator for censoring
    mutate(# 6 months
           cens_6 = if_else(is.na(pcs_6), 1, 0),
           # 12 months
           cens_12 = if_else(is.na(pcs_12), 1, 0),
           # Create extra variables for PCS and MCS containing a 0 in-case of censoring
           across(c(pcs_6, pcs_12, mcs_6, mcs_12), ~ if_else(is.na(.x), 0, .x), .names = "{.col}_t1"))

# Save final data
save(dat_imputed, file = paste0(path, "dat_imputed.Rdata"))
