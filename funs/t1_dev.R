# Function to pool models developed with Tobit type 1
t1_dev <- function(.data, outcome, formula_rhs){
    # Get number of imputations and remove 0 if present
    imps <- setdiff(unique(.data[[".imp"]]), 0)
    
    # For each imputation, get model
    lst_fit <- map(imps, \(x) t1_develop(filter(.data, .imp == x), outcome, formula_rhs))
    
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
    
    ## Get spline information
    # Create vector of spline variables
    vec_spl <- str_extract_all(formula_rhs, "(?<=ns\\()\\w+")[[1]]
    
    # Create vector of separate arguments
    vec_knt <- str_split_1(formula_rhs, "\\s?\\+\\s?")
    
    # Create vector of knots
    vec_knt <- vec_knt[str_detect(vec_knt, "ns\\(")] %>%
        # Keep only knots argument
        str_extract_all("knots = .+(?=\\))")
 
    # Get subset of data
    dat_tmp <- filter(.data, .imp == 1)
    
    # Get boundary knots for each spline
    lst_bks <- map(vec_spl, \(x) attr(ns(dat_tmp[[x]]), "Boundary.knots"))
    
    # Recreate each spline argument with boundary knots
    lst_spl <- map(1:length(vec_spl), \(x){
        # Get variable
        var <- vec_spl[[x]]
        
        # Get knots
        knts <- vec_knt[[x]]
        
        # Get boundary knots
        bks <- paste0(lst_bks[[x]], collapse = ",")
        
        # Add together
        term <- paste0("ns(", var, ", ", knts, ", Boundary.knots = c(", bks, "))")
        
        # Return term
        return(term)
    })
    
    # Create temporary vector containing predictors with updated terms
    vec_prd_tmp <- predictors
    
    # Update vector with spline terms
    for(i in 1:length(lst_spl)) vec_prd_tmp <- str_replace_all(vec_prd_tmp, vec_spl[[i]], lst_spl[[i]])
    
    # Recreate formula_rhs from vec_prd_tmp with updated spline terms
    formula_rhs2 <- paste0(vec_prd_tmp, collapse = " + ")
    
    # Put results into list
    res <- list(coef = dat_coef, 
                scale = scale,
                predictors = predictors,
                formula_rhs = formula_rhs2)
    
    # Return results
    return(res)
}
    