# Function for mean IPC weight per individual
ipcw <- function(.data, censor){
    # Put data into temporary data set
    dat_tmp <- .data
    
    # Give outcome generic name in fitting data
    dat_tmp[["censoring"]] <- dat_tmp[[censor]] 
    
    # Get number of imputations and remove 0 if present
    imps <- setdiff(unique(dat_tmp[[".imp"]]), 0)
    
    # For each imputation, get censoring PS
    lst_ipcw <- map(imps, \(x){
        # Subset data
        dat_tmp2 <- filter(dat_tmp, .imp == x)
        
        # Fit censoring model
        fit_ipcw <- glm(censoring == 0 ~ age + sex + hb + bpsyst + bmi + ihd + malign + modality + dm + pkd + pcs_0 + mcs_0 + sc + sb,
                        data = dat_tmp2)
        
        # Get censoring PS
        cps <- predict(fit_ipcw, type = "response")
        
        # Calculate weight
        cw <- if_else(dat_tmp2[["cens_6"]] == 0, 1 / cps, 1 / (1 - cps))
        
        # Return weights
        return(cw)
    })
    
    # Pool weights
    cw <- bind_cols(map(imps, \(x) lst_ipcw[[x]])) %>%
        # Get mean per row
        rowMeans(.)
    
    # Return pooled weights
    return(cw)
}
    