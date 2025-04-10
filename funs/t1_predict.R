# Function to calculate final predictions of Tobit type 1
t1_predict <- function(.data, outcome, coef, intercept){
    # Calculate linear predictors
    lps <- lp_t1(.data, outcome, coef, intercept)
    
    # Get scale
    scale <- coef[["scale"]]
    
    # We calculate the expected Y (not the unobserved Y* which equals the LP) using the scale parameter and linear predictor
    y <- (1 - pnorm(-lps / scale)) * lps + scale * dnorm(-lps / scale)
    
    # Return expected Y and linear predictors
    return(list(lps = lps, y = y))
}
    