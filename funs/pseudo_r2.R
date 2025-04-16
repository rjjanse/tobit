# Function to calculate pseudo-R2 according to Laitila (1993)
pseudo_r2 <- function(lps, sigma, digits = 3){
    # Calculate numerator
    num <- sum((lps - mean(lps)) ^ 2)
    
    # Calculate denominator
    denom <- sum((lps - mean(lps)) ^ 2) + length(lps) * sigma
    
    # Calcualte pseudo R-squared
    r2 <- round(num / denom, digits = digits)
    
    # Return value
    return(r2)
}
    