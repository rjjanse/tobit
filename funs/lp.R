# Function to calculate linear predictor
lp <- function(.data, outcome, coef, intercept){
      # Put data into temporary data set
    dat_tmp <- .data
    
    # Give outcome generic name in fitting data
    dat_tmp[["outcome"]] <- dat_tmp[[outcome]] 
    
    # Get relevant columns from data
    dat_tmp <- select(dat_tmp, all_of(c("outcome", coef[["predictors"]])))
    
    # Create model matrix
    mat_mod <- model.matrix(as.formula(paste0("Surv(outcome, outcome > 0) ~ ", paste0(coef[["predictors"]], collapse = "+"))), dat_tmp)
    
    # Get linear predictor per individual
    lps <- mat_mod %*% as.matrix(coef[["coef"]]["value"])
    
    # Change LPs to vector
    lps <- as.vector(lps)
    
    # Return LPs
    return(lps)
}
    