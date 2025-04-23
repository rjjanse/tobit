# Function to calculate final predictions of Tobit type 2
t2_predict <- function(.data, outcome, cens_outcome, coef, intercept){
    # Calculate linear predictors for selection model
    lps_s <- lp_t2(.data, cens_outcome, coef, intercept, model = "selection")
    
    # Calculate linear predictors for outcome model
    lps_o <- lp_t2(.data, outcome, coef, intercept, model = "outcome")
    
    # Get covariance of errors
    cov_e <- coef[["cov_e"]]
    
    # We calculate the expected Y (not the unobserved Y* which equals the LP) using the covariance between errors parameter and 
    # linear predictors of the outcome and selection model
    y <- lps_o + cov_e * dnorm(-lps_s) / (1 - pnorm(-lps_s))
    
    # Calculate probability of participation (i.e. cens == 0)
    pr <- pnorm(lps_s)
    
    # Store results in list
    res <- list(lps_s = lps_s,
                lps_o = lps_o,
                pr = pr,
                y = y)
    
    # Return expected Y and linear predictors
    return(res)
}
