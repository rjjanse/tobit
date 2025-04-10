# Function to calculate linear predictor for Tobit type 2
lp_t2 <- function(.data, outcome, coef, intercept, model){
    # Put data into temporary data set
    dat_tmp <- .data
    
    # Give outcome generic name in fitting data
    dat_tmp[["outcome"]] <- dat_tmp[[outcome]] 
    
    # Get relevant information from coef object for selection model
    if(model == "selection"){
        # Get predictors
        vec_prd <- coef[["predictors_s"]]
        
        # Get formula righthand side
        formula_rhs <- coef[["formula_rhs_s"]]
        
        # Get coefficients
        dat_coef <- coef[["coef_s"]]
    }
    
    # Get relevant information from coef object for outcome model
    if(model == "outcome"){
        # Get predictors
        vec_prd <- coef[["predictors_o"]]
        
        # Get formula righthand side
        formula_rhs <- coef[["formula_rhs_o"]]
        
        # Get coefficients
        dat_coef <- coef[["coef_o"]]
    }
    
    # Get relevant columns from data 
    dat_tmp <- select(dat_tmp, all_of(c("outcome", vec_prd))) %>%
        # Set missing data in outcome to 0 to prevent exclusion
        mutate(outcome = if_else(is.na(outcome), 0, outcome))
    
    # Create model matrix (the outcome does not influence the model matrix so we can ignore the fact that outcome should be outcome == 0 for the selection model
    mat_mod <- model.matrix(as.formula(paste0("outcome ~ ", formula_rhs)), dat_tmp)
    
    # Get linear predictor per individual
    lps <- mat_mod %*% as.matrix(dat_coef["value"])
    
    # Change LPs to vector
    lps <- as.vector(lps)
    
    # Return LPs
    return(lps)
}
