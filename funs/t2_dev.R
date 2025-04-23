# Function to pool models developed with Tobit type 2
t2_dev <- function(.data, outcome, s_formula_rhs, o_formula_rhs){
    # Get number of imputations and remove 0 if present
    imps <- setdiff(unique(.data[[".imp"]]), 0)
    
    # For each imputation, get model
    lst_fit <- map(imps, \(x) t2_develop(filter(.data, .imp == x), outcome, s_formula_rhs, o_formula_rhs))
    
    # Get mean coefficient and intercept from all selection models
    dat_coef_s <- bind_cols(map(imps, \(x) lst_fit[[x]][["coef_s"]][[2]])) %>%
        # Set all values to numeric
        mutate(across(everything(), as.numeric)) %>%
        # Get mean per row
        rowMeans(.) %>%
        # Change back to data frame
        as_tibble() %>%
        # Append variables
        mutate(var = lst_fit[[1]][["coef_s"]][[1]])
    
    # Get mean coefficient and intercept from all outcome models
    dat_coef_o <- bind_cols(map(imps, \(x) lst_fit[[x]][["coef_o"]][[2]])) %>%
        # Set all values to numeric
        mutate(across(everything(), as.numeric)) %>%
        # Get mean per row
        rowMeans(.) %>%
        # Change back to data frame
        as_tibble() %>%
        # Append variables
        mutate(var = lst_fit[[1]][["coef_o"]][[1]])

    # Get predictors for selection model
    predictors_s <- lst_fit[[1]][["predictors_s"]]
    
    # Get predictors for outcome model
    predictors_o <- lst_fit[[1]][["predictors_o"]]
    
    # Get mean covariance of errors
    cov_e <- mean(map_vec(imps, \(x) lst_fit[[x]][["cov_e"]]))
    
    # Get mean sigma for outcome model
    sigma_o <- mean(map_vec(imps, \(x) lst_fit[[x]][["sigma_o"]]))
    
    ## Get spline information for selection model
    # Create vector of spline variables
    vec_spl <- str_extract_all(s_formula_rhs, "(?<=ns\\()\\w+")[[1]]
    
    # Create vector of separate arguments
    vec_knt <- str_split_1(s_formula_rhs, "\\s?\\+\\s?")
    
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
    
    # Create temporary vector containing predictors for selection model with updated terms
    vec_prd_tmp <- predictors_s
    
    # Update vector with spline terms
    for(i in 1:length(lst_spl)) vec_prd_tmp <- str_replace_all(vec_prd_tmp, vec_spl[[i]], lst_spl[[i]])
    
    # Recreate s_formula_rhs from vec_prd_tmp with updated spline terms
    s_formula_rhs2 <- paste0(vec_prd_tmp, collapse = " + ")
    
    ## Get spline information for outcome model
    # Create vector of spline variables
    vec_spl <- str_extract_all(o_formula_rhs, "(?<=ns\\()\\w+")[[1]]
    
    # Create vector of separate arguments
    vec_knt <- str_split_1(o_formula_rhs, "\\s?\\+\\s?")
    
    # Create vector of knots
    vec_knt <- vec_knt[str_detect(vec_knt, "ns\\(")] %>%
        # Keep only knots argument
        str_extract_all("knots = .+(?=\\))")

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
    
    # Create temporary vector containing predictors for selection model with updated terms
    vec_prd_tmp <- predictors_o
    
    # Update vector with spline terms
    for(i in 1:length(lst_spl)) vec_prd_tmp <- str_replace_all(vec_prd_tmp, vec_spl[[i]], lst_spl[[i]])
    
    # Recreate s_formula_rhs from vec_prd_tmp with updated spline terms
    o_formula_rhs2 <- paste0(vec_prd_tmp, collapse = " + ")
    
    # Put results into list
    res <- list(coef_s = dat_coef_s,
                coef_o = dat_coef_o,
                predictors_s = predictors_s,
                predictors_o = predictors_o,
                formula_rhs_s = s_formula_rhs2,
                formula_rhs_o = o_formula_rhs2,
                cov_e = cov_e,
                sigma_o = sigma_o)
    
    # Return results
    return(res)
}
