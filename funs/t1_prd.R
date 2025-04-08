# Function to pool predictions and linear predictors calculated with Tobit type 1
t1_prd <- function(.data, outcome, coef, intercept = TRUE){
    # Get number of imputations and remove 0 if present
    imps <- setdiff(unique(.data[[".imp"]]), 0)
     
    # For each imputation, get predictions
    lst_prd <- map(imps, \(x) t1_predict(filter(.data, .imp == x), outcome, coef, intercept))
    
    # Pool predictions
    prd <- bind_cols(map(imps, \(x) lst_prd[[x]][["y"]])) %>%
        # Get mean per row
        rowMeans(.)
    
    # Pool LPs
    lps <- bind_cols(map(imps, \(x) lst_prd[[x]][["lps"]])) %>%
        # Get mean per row
        rowMeans(.)
    
    # Return pooled predictions and LPs
    return(list(y = prd, lps = lps))
}
    