# Function to pool models developed with Tobit type 1
t1_dev <- function(.data, outcome){
    # Get number of imputations and remove 0 if present
    imps <- setdiff(unique(.data[[".imp"]]), 0)
    
    # For each imputation, get model
    lst_fit <- map(imps, \(x) t1_develop(filter(.data, .imp == x), outcome))
    
    # Get mean coefficient and intercept from all models
    dat_coef <- bind_cols(map(imps, \(x) lst_fit[[x]][["coef"]][[2]])) %>%
        # Set all values to numeric
        mutate(across(everything(), as.numeric)) %>%
        # Get mean per row
        rowMeans(.) %>%
        # Change back to data frame
        as_tibble() %>%
        # Append variables
        mutate(var = lst_fit[[1]][["coef"]][[1]])
    
    # Get mean scale
    scale <- mean(map_vec(imps, \(x) lst_fit[[x]][["scale"]]))
    
    # Get predictors
    predictors <- lst_fit[[1]][["predictors"]]
    
    # Put results into list
    res <- list(coef = dat_coef, 
                scale = scale,
                predictors = predictors)
    
    # Return results
    return(res)
}
    