#-----------------------------------------------------------------------------#
# Predicting HRQoL using Tobit regression                                     #
# Roemer J. Janse                                                             #
# Code for cohort derivation                                                  #
# Creation date: 2025-02-11 || Last update:  2025-03-21                       #
#-----------------------------------------------------------------------------#

# 0. Set-up ----
# Load packages
pacman::p_load("conflicted",     # Solve package conflicts
               "dplyr",          # Data wrangling
               "magrittr",       # Efficient pipelines
               "tidyr",          # Pivoting data
               "stringr",        # Character strings
               "here",           # Relative directories
               "rio",            # Load data
               "summarytools",   # Data summaries
               "mice",           # Impute data
               "miceadds"        # Adds 2l.pmm for longitudinal imputation
)

# Solve conflicts
conflicts_prefer(dplyr::filter)    # Between dplyr & stats

# Source relevant functions
source(here("funs/prd_to_pkd.R"))

# Set working directory
path <- "C:/users/rjjan/downloads/"

# Import data
dat_domestico <- import(paste0(path, "predictie data OCT2024.Rdata"),
                        trust = TRUE) %>%
    # Remove one wrongly included prevalent dialysis patient
    filter(studynr != 1335) %>%
    # Set all columns to lowercase
    set_colnames(tolower(colnames(.)))

# Import PKD and information on previous transplantation for individuals without RENINE linkage
dat_pkd <- import(paste0(path, "pkd.xlsx"),
                  trust = TRUE) %>%
    # Rename columns to DOMESTICO format
    rename(studynr = 1, pkd = 2, prev_ntx = 4, prev_dial = 5) %>%
    # Reformat prev_dial and prev_ntx to logical indicators
    mutate(# If there is a numeric value, a date was given for transplantation
           prev_ntx = case_when(str_detect(prev_ntx, "rans") ~ 0,
                                str_detect(prev_ntx, "\\d") ~ 1),
           # If there is a numeric value, a date was given for transplantation
           prev_dial = case_when(str_detect(prev_dial, "rans|dial") ~ 0,
                                 str_detect(prev_dial, "\\d") ~ 1),
           # Dialysis is only collected as prior to transplantation, so prev_dial == 1 & prev_ntx == 0 are wrong artefacts
           prev_dial = if_else(prev_ntx == 0 & prev_dial == 1, 0, prev_dial)) %>%
    # Drop primary diagnosis column
    select(-`Primaire diagnose`) %>%
    # Change some values for PKD
    mutate(pkd = case_match(pkd,
                            "Atheroembolic renal disease - no histology" ~ "Hypertension / Renal vascular disease",
                            "Chronic hypertensive nephropathy" ~ "Hypertension / Renal vascular disease",
                            "chronische nierinsufficientie stadium 3" ~ NA,
                            "Diabetic nephropathy in type II diabetes" ~ "Diabetes Mellitus",
                            "familiar/hereditary nephropathies" ~ "Familial / hereditary nephropathies",
                            "Geen deelname studie" ~ NA,
                            "IgA nefropathie" ~ "Glomerular disease",
                            "IgA nephropathy" ~ "Glomerular disease",
                            "Miscellaneous" ~ "Miscellaneous renal disorders",
                            "Parenchymeuze nierziekte" ~ "Glomerular disease",     # Could also be tubular disease
                            "syndroom van Goodpasture" ~ "Glomerular disease",
                            "Unknown" ~ NA,
                            .default = pkd))

# Remove attributes
dat_domestico[] <- lapply(dat_domestico, \(x){attributes(x) <- NULL; x})

# 1. Clean up data ----
# Add missing values for prev_ntx and prev_dial due to missing RENINE linkage and create PKD categories
dat_cohort <- dat_domestico %>%
    # Update variables
    mutate(# Categorise primary kidney disease
           pkd = prd_to_pkd(prd)) %>%
    # Join PKD data to replace missings
    left_join(dat_pkd, "studynr", suffix = c("", ".added")) %>%
    # For PKD, prev_dial, and prev_ntx, set missings to value from the added columns
    mutate(across(c(pkd, prev_dial, prev_ntx), ~ coalesce(.x, get(paste0(cur_column(), ".added"))))) %>%
    # Arrange per individual
    arrange(studynr) %>%
    # Group per individual
    group_by(studynr) %>%
    # Fill downwards all values that are only collected at baseline
    fill(prev_dial:stop_red, pkd, .direction = "down") %>%
    # Remove grouping structure again
    ungroup() %>%
    # Set categorical variables to factors and reduce levels for logicals
    mutate(# Change acute_pt from logical 1/2 to 0/1
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
           # We incorporate values of the DSI into DSI burden
           across(all_of(paste0("dsi_last_", 1:30)), \(x) x = if_else(get(str_replace(cur_column(), "_last", "")) == 0, 0, x)))
    
# Data selection 
dat_cohort %<>%
    # Variable selection
    select(studynr, mtpnt, mtpnt_dt, pkd, prev_dial:stop_red, weight, modality, resid_diur,
           bpsyst, bpdiast, med_epo, med_iron, hb:woonsituatie, sf12_1:dsi_last_30) %>%
    # Start therap is inferior to therapie for baseline modality
    select(-start_therap, -all_of(paste0("dsi_", 1:30))) %>%
    # Rename variables for clarity
    rename(ihd = ihd_predial,       # Ischemic heart disease
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

# Set the cluster variable for longitudinal imputation
mat_prd[, "studynr"] <- -2

# Define variables that do not have to be imputed
vec_nimp <- c(# Auxiliary variables
              "prev_dial", "prev_ntx", "acute_pt", "malign", "malign_nometa", 
              "malign_meta", "lymphoma", "leukemia", "hmp", "dm_type", "scvd",
              "parathyr", "dementia", "pulm", "ulcer", "liver_mild", "liver_modsev",
              "aids", "psych", "resid_diur", "bpsyst", "bpdiast", "med_epo",
              "med_iron", "na", "chol",
              # Dates
              "mtpnt_dt", "dial_dt", "stop_dt",
              # Meta information
              "studynr", "mtpnt", "stop_red")

# Adjust predictor matrix accordingly
mat_prd[vec_nimp, ] <- 0

# Define variables that have to be imputed
vec_imp <- colnames(mat_prd)[!(colnames(mat_prd) %in% vec_nimp)]

# Get methods vector
vec_mtd <- mice(dat_cohort, maxit = 0)[["method"]]

# Change variables to be imputed to longitudinal imputation
vec_mtd <- if_else(names(vec_mtd) %in% vec_imp, "2l.pmm", vec_mtd)

# Reset names of methods vector
names(vec_mtd) <- colnames(mat_prd)

# Start imputation
lst_imp <- mice(dat_cohort,
                m = 10,
                maxit = 50,
                method = vec_mtd,
                predictorMatrix = mat_prd)

# Save object
save(lst_imp, file = paste0(path, "imputation_object.Rdata"))



# PKD
pkd = factor(pkd, levels = c("Diabetes Mellitus",
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
modality = factor(therapie, labels = c("HD", "PD")),
# Smoking
smoking = factor(smoking, labels = c("Never", "Current", "Quit")),
# Reason for leaving study
stop_red = factor(stop_red, labels = c("KTx", "Restored kidney function", "Stopping dialysis", "Death", "Withdraw informed consent", "Emigration")),
# Civil status
burgstaat = factor(burgstaat, labels = c("Single", "Married/cohabitating", "Divorced", "Widowed")),
# Living status
woonsituatie = factor(woonsituatie, labels = c("Alone", "With partner", "With parents", "With family", "Caring home")),
# SF-12 questions
across(sf12_1:sf12_12, ~ factor(.x)),

    
    # Change all dates back to date format
    dat_domestico %<>% mutate(across(contains("_dt"), \(x) x = as.Date(x, origin = "1970-01-01")))
    