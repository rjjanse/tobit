# Function to fully develop and validate a Tobit type 1 model for cross-validation
t1_full_cv <- function(.data, outcome, formula_rhs, fold_val){
    # Store data temporarily to have it function inside the ipcw function
    dat_tmp <- .data
    
    # Get development data
    dat_dev <- filter(dat_tmp, fold != fold_val)
    
    # Get validation data
    dat_val <- filter(dat_tmp, fold == fold_val)
    
    # Develop model
    fit_tmp <- t1_dev(dat_dev, outcome, form_rhs)
    
    # Determine which censoring indicator is needed
    if(str_detect(outcome, "6")) cens_outcome <- "cens_6" else cens_outcome <- "cens_12"
    
    # Get predictions and make data frame
    dat_prd_tmp <- t1_prd(dat_val, outcome, fit_tmp) %>%
        # Bind columns
        bind_cols() %>%
        # Add identifier, weights and observed value
        mutate(# Identifier from original data
               studynr = filter(dat_val, .imp == 1)[["studynr"]],
               # Weights according to IPCW
               w = ipcw(dat_val, cens_outcome),
               # True observed outcome (pooled over all imputations)
               true_y = filter(dat_val, .imp == 1)[[paste0(str_replace(outcome, "_t\\d", ""), "_pooled")]])
    
    # Calculate pseudo R-squared
    r2 <- dat_prd_tmp %$% pseudo_r2(lps, fit_tmp[["scale"]])
    
    # Get model calibration
    cal <- t1_cal(dat_prd_tmp, "placeholder")
    
    # Extract metrics from model calibration
    citl <- cal[["citl"]]; cslope <- cal[["cslope"]]
    
    # List with results
    res <- list(prds = dat_prd_tmp,
                r2 = r2,
                citl = as.numeric(citl),
                cslope = as.numeric(cslope))
    
    # Return results
    return(res)
}
    