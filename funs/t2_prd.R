# Function to pool predictions and linear predictors calculated with Tobit type 2
t2_prd <- function(.data, outcome, cens_outcome, coef, intercept = TRUE){
    # Get number of imputations and remove 0 if present
    imps <- setdiff(unique(.data[[".imp"]]), 0)
    
    # For each imputation, get predictions
    lst_prd <- map(imps, \(x) t2_predict(filter(.data, .imp == x), outcome, cens_outcome, coef, intercept))
    
    # Pool outcome predictions
    y <- bind_cols(map(imps, \(x) lst_prd[[x]][["y"]])) %>%
        # Get mean per row
        rowMeans(.)
    
    # Pool selection predictions
    pr <- bind_cols(map(imps, \(x) lst_prd[[x]][["pr"]])) %>%
        # Get mean per row
        rowMeans(.)
    
    # Pool LPs for outcome model
    lps_o <- bind_cols(map(imps, \(x) lst_prd[[x]][["lps_o"]])) %>%
        # Get mean per row
        rowMeans(.)
    
    # Pool LPs for selection model
    lps_s <- bind_cols(map(imps, \(x) lst_prd[[x]][["lps_s"]])) %>%
        # Get mean per row
        rowMeans(.)
    
    # Create list to return results
    res <- list(y = y,
                pr = pr,
                lps_s = lps_s,
                lps_o = lps_o)
    
    # Return pooled predictions and LPs
    return(res)
}
