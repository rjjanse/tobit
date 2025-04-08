# Function to calculate pseudo-R2 according to Laitila (1993)
pseudo_r2 <- function(lps, y){
    # Calculate numerator
    num <- sum((lps - mean(lps)) ^ 2)
    
    # Calculate denominator
    denom <- sum((lps - mean(lps)) ^ 2) + length(lps) * var(y)
    
    # Calcualte pseudo R-squared
    r2 <- num / denom
    
    # Return value
    return(r2)
}
    