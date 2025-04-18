# Function to cross-validate a Tobit type 2 model
t2_cv <- function(.data, outcome, plot_label, s_formula_rhs, o_formula_rhs, folds = 5){
    # Get unique studynrs
    ids <- unique(.data[["studynr"]])
    
    # Add in data frame and add fold label
    dat_folds <- tibble(studynr = ids,
                        fold = cut(1:length(ids),
                                   breaks = folds,
                                   labels = FALSE)) 
    
    # Add fold indicator to imputed data
    dat_tmp <- left_join(.data, dat_folds, "studynr")
    
    # Cross-validate with each fold
    lst_cv <- map(1:folds, \(x) t1_full_cv(dat_tmp, outcome, formula_rhs, x))
    
    # Get r2 from cross validation
    r2 <- mean(map_vec(1:folds, \(x) lst_cv[[x]][["r2"]]))
    
    # Get CITL from cross validation
    citl <- mean(map_vec(1:folds, \(x) lst_cv[[x]][["citl"]]))
    
    # Get calibration slope from cross validation
    cslope <- mean(map_vec(1:folds, \(x) lst_cv[[x]][["cslope"]]))
    
    # Pool data for calibration plot
    dat_cal <- bind_rows(map(1:folds, \(x) lst_cv[[x]][["prds"]]))
    
    # Create calibration plot
    p <- t1_cal(dat_cal, plot_label)[["plot"]]
    
    # Create list of results
    res <- list(plot = p,
                r2 = r2,
                citl = citl,
                cslope = cslope)
    
    # Return list
    return(res)
}
