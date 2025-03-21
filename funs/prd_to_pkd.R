# Function to change PRD codes to new ERA classification
prd_to_pkd <- function(prd){
    # Switch values
    pkd <- case_match(prd, 
                      c(10:17, 19, 73, 74, 76, 78, 84:87) ~ "Glomerular disease",
                      c(20:25, 29:34, 39, 49, 60, 61, 63, 91:94) ~ "Tubulointerstitial disease",
                      80:81 ~ "Diabetes Mellitus",
                      c(70:72, 79) ~ "Hypertension / Renal vascular disease",
                      c(82, 83, 88, 89) ~ "Other systemic diseases affecting the kidney",
                      c(41:43, 50:54, 59) ~ "Familial / hereditary nephropathies",
                      c(0, 40, 75, 90, 95, 96, 99) ~ "Miscellaneous renal disorders",
                      .default = NA)
    
    # Return PKD
    return(pkd)
}
                                  
    
    