# Function to develop a Tobit type 1 model on imputed data
t1_develop <- function(.data, outcome){
    # Put data into temporary data set
    dat_tmp <- .data
    
    # Give outcome generic name in fitting data
    dat_tmp[["outcome"]] <- dat_tmp[[outcome]] 
    
    # Fit model
    fit <- survreg(Surv(outcome, outcome > 0, type = "left") ~ age + pcs_0 + mcs_0 + bmi + ihd + hb + sc + sb + pth + partner + pkd + sex + smoking + modality,
                   data = dat_tmp, dist = "gaussian")
    
    # Get coefficients
    dat_coef <- as_tibble(cbind(names(coef(fit)), coef(fit)),
                          .name_repair = "minimal")
    
    # Get scale
    scale <- fit[["scale"]]
    
    # Get predictors
    predictors <- str_split_1(as.character(fit[["call"]][["formula"]])[[3]], "\\s\\+\\s")
    
    # Add results together
    res <- list(coef = dat_coef,
                scale = scale,
                predictors = predictors)
    
    # Return results
    return(res)
}
        
    
    
    
    
    
    
    
    
    
    
    
   