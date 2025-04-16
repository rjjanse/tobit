# Function to develop a Tobit type 2 model on imputed data
t2_develop <- function(.data, outcome, s_formula_rhs, o_formula_rhs){
    # Put data into temporary data set
    dat_tmp <- .data
    
    # Give outcome generic name in fitting data
    dat_tmp[["outcome"]] <- dat_tmp[[outcome]] 
    
    # Determine which censoring indicator is needed
    if(str_detect(outcome, "6")) dat_tmp[["cens"]] <- dat_tmp[["cens_6"]] else dat_tmp[["cens"]] <- dat_tmp[["cens_12"]]
    
    # Add right hand side of selection formula to full formula
    s_formula <- paste0("cens == 0 ~ ", s_formula_rhs)
    
    # Add right hand side of outcome formula to full formula
    o_formula <- paste0("outcome ~ ", o_formula_rhs)
    
    # Fit model
    fit <- selection(selection = as.formula(s_formula), 
                     outcome = as.formula(o_formula),
                     data = dat_tmp,
                     type = 2,
                     method = "ml")
    
    # Get coefficients for selection model
    dat_coef_s <- as_tibble(cbind(names(coef(fit)), coef(fit)),
                            .name_repair = "minimal") %>%
        # Extract rows for selection models, based on occurrence of second (Intercept) term
        extract(1:(which(names(coef(fit)) %in% "(Intercept)")[[2]] - 1), ) %>%
        # Set column names
        set_colnames(c("var", "coef"))
   
    
    # Get coefficients for outcome model
    dat_coef_o <- as_tibble(cbind(names(coef(fit)), coef(fit)),
                            .name_repair = "minimal") %>%
        # Extract rows for selection models, based on occurrence of second (Intercept) term
        extract(which(names(coef(fit)) %in% "(Intercept)")[[2]]:length(coef(fit)), ) %>%
        # Set column names
        set_colnames(c("var", "coef")) %>%
        # Remove sigma and rho
        filter(!(var %in% c("sigma", "rho")))

    # Get selection model predictors
    s_predictors <- str_extract_all(s_formula_rhs, "(?<=ns\\(|\\+\\s(?!ns\\())\\w+")[[1]]
    
    # Get outcome model predictors
    o_predictors <- str_extract_all(o_formula_rhs, "(?<=ns\\(|\\+\\s(?!ns\\())\\w+")[[1]] 
    
    ### Get covariance between error terms (required to calculate expected y)
    ## Get all required values
    # Standard error of error terms of outcome model
    sigma_o <- fit[["twoStep"]][["sigma"]]

    # Mean inverse Mills ratio which contains rho and standard error of the error terms of selection model
    lambda <- mean(fit[["twoStep"]][["invMillsRatio"]])
    
    # Calculate covariance between error terms
    cov_e <- lambda * sigma_o
    
    # Add results together
    res <- list(coef_s = dat_coef_s,
                coef_o = dat_coef_o,
                predictors_s = s_predictors,
                predictors_o = o_predictors,
                cov_e = cov_e,
                sigma_o = sigma_o)
    
    # Return results
    return(res)
}












