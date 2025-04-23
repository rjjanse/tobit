# Function to fully develop and validate a Tobit type 2 model for cross-validation
t2_full_cv <- function(.data, outcome, s_formula_rhs, o_formula_rhs, fold_val, plot_s_title){
    # Store data temporarily to have it function inside the ipcw function
    dat_tmp <- .data
    
    # Get development data
    dat_dev <- filter(dat_tmp, fold != fold_val)
    
    # Get validation data
    dat_val <- filter(dat_tmp, fold == fold_val)
    
    # Develop model
    fit_tmp <- t2_dev(dat_dev, outcome, selection_form_rhs, outcome_form_rhs)
    
    # Determine which censoring indicator is needed
    if(str_detect(outcome, "6")) cens_outcome <- "cens_6" else cens_outcome <- "cens_12"
    
    # Get predictions and make data frame
    dat_prd_tmp <- t2_prd(dat_val, outcome, cens_outcome, fit_tmp) %>%
        # Bind columns
        bind_cols() %>%
        # Add identifier, weights and observed value
        mutate(# Identifier from original data
            studynr = filter(dat_val, .imp == 1)[["studynr"]],
            # Weights according to IPCW
            w = 1 / pr,
            # True observed outcome (pooled over all imputations)
            true_y = filter(dat_val, .imp == 1)[[paste0(outcome, "_pooled")]])
    
    # Calculate pseudo R-squared
    r2 <- dat_prd_tmp %$% pseudo_r2(lps_o, fit_tmp[["sigma_o"]])
    
    # Get model calibration
    cal <- t2_cal(dat_prd_tmp, "placeholder", plot_s_title)
    
    # Extract metrics from outcome model calibration
    citl_o <- cal[["citl_o"]]; cslope_o <- cal[["cslope_o"]]
    
    # Extract metrics from selection model calibration
    citl_s <- cal[["citl_s"]]; cslope_s <- cal[["cslope_s"]]; c <- cal[["c"]]
    
    # List with results
    res <- list(prds = dat_prd_tmp,
                r2 = r2,
                citl_o = citl_o,
                cslope_o = cslope_o,
                citl_s = citl_s,
                cslope_s = cslope_s,
                c = c)
    
    # Return results
    return(res)
}
