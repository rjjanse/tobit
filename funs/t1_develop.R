# Function to develop a Tobit type 1 model on imputed data
t1_develop <- function(.data, outcome, formula_rhs){
    # Put data into temporary data set
    dat_tmp <- .data
    
    # Give outcome generic name in fitting data
    dat_tmp[["outcome"]] <- dat_tmp[[outcome]] 
    
    # Add right hand side of formula to full formula
    form <- paste0("Surv(outcome, outcome > 0, type = 'right') ~ ", formula_rhs)
    
    # Fit model
    fit <- survreg(as.formula(form),
                   data = dat_tmp, 
                   dist = "gaussian")
    
    # Get coefficients
    dat_coef <- as_tibble(cbind(names(coef(fit)), coef(fit)),
                          .name_repair = "minimal")
    
    # Get scale
    scale <- fit[["scale"]]
    
    # Get predictors
    predictors <- str_extract_all(formula_rhs, "(?<=ns\\(|\\+\\s(?!ns\\())\\w+")[[1]]
    
    # Add results together
    res <- list(coef = dat_coef,
                scale = scale,
                predictors = predictors)
    
    # Return results
    return(res)
}
        
    
    
    
    
    
    
    
    
    
    
    
   