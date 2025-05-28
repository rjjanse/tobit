# Function to cross-validate a Tobit type 2 model
t2_cv <- function(.data, outcome, plot_label, s_formula_rhs, o_formula_rhs, folds = 5, inset_coords = c(0.025, 0.6, 0.4, 0.975), plot_s_title = NULL){
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
    lst_cv <- map(1:folds, \(x) t2_full_cv(dat_tmp, outcome, s_formula_rhs, o_formula_rhs, x, inset_coords, plot_s_title))
    
    # Get r2 from cross validation
    r2 <- mean(map_vec(1:folds, \(x) lst_cv[[x]][["r2"]]))
    
    # Get outcome CITL from cross validation
    citl_o <- mean(map_vec(1:folds, \(x) lst_cv[[x]][["citl_o"]]))
    
    # Get outcome calibration slope from cross validation
    cslope_o <- mean(map_vec(1:folds, \(x) lst_cv[[x]][["cslope_o"]]))
    
    # Get selection CITL from cross validation
    citl_s <- mean(map_vec(1:folds, \(x) lst_cv[[x]][["citl_s"]]))
    
    # Get selection calibration slope from cross validation
    cslope_s <- mean(map_vec(1:folds, \(x) lst_cv[[x]][["cslope_s"]]))

    # Get selection C-statistic from cross validation
    c <- mean(map_vec(1:folds, \(x) lst_cv[[x]][["c"]]))
    
    # Pool data for calibration plot
    dat_cal <- bind_rows(map(1:folds, \(x) lst_cv[[x]][["prds"]]))
    
    # Create calibration plot for outcome
    p_o <- t2_cal(dat_cal, plot_label)[["plot_o"]]
    
    # Create calibation plot for selection
    p_s <- t2_cal(dat_cal, "probability", inset_coords = inset_coords, plot_s_title = plot_s_title)[["plot_s"]]
    
    # Create list of results
    res <- list(plot_o = p_o,
                plot_s = p_s,
                r2 = r2,
                citl_o = citl_o,
                cslope_o = cslope_o,
                citl_s = citl_s,
                cslope_s = cslope_s,
                c = c,
                dat = dat_cal)
    
    # Return list
    return(res)
}
