# Function to calculate SF12 summary scores
sf12_v1 <- function(q1,
                    q2a,
                    q2b,
                    q3a,
                    q3b,
                    q4a,
                    q4b,
                    q5,
                    q6a,
                    q6b,
                    q6c,
                    q7,
                    # Component to return
                    return = c("pcs", "mcs")
){
    ## Recode items to have higher scores reflect more positive concepts
    # First question (general health)
    q1r <- 6 - q1
    
    # Eigth question (pain interference)
    q5r <- 6 - q5
    
    # Ninth question (calm & peaceful)
    q6ar <- 7 - q6a
    
    # Tenth question (energy)
    q6br <- 7 - q6b

    # Aggregate physical score
    pcs <- 
        # General health (GH)
        case_match(q1r,
                   1 ~ -8.37399,
                   2 ~ -5.56461,
                   3 ~ -3.02396,
                   4 ~ -1.31872,
                   .default = 0) +
        # Physical functioning (PF)
        case_match(q2a,
                   1 ~ -7.23216,
                   2 ~ -3.45555,
                   .default = 0) +
        case_match(q2b,
                   1 ~ -6.24397,
                   2 ~ -2.73557,
                   .default = 0) +
        # Role physical (RP)
        if_else(q3a == 1, -4.61617, 0) +
        if_else(q3b == 1, -5.51747, 0) +
        # Role emotional (RE)
        if_else(q4a == 1, 3.04365, 0) +
        if_else(q4b == 1, 2.32091, 0) +
        # Bodily pain (BP)
        case_match(q5r,
                   1 ~ -11.25544,
                   2 ~ -8.38063,
                   3 ~ -6.50522,
                   4 ~ -3.80130,
                   .default = 0) +
        # Mental health (MH)
        case_match(q6ar,
                   1 ~ 3.46638,
                   2 ~ 2.90426,
                   3 ~ 2.37241,
                   4 ~ 1.36689,
                   5 ~ 0.66514,
                   .default = 0) +
        case_match(q6c,
                   1 ~ 4.61446, 
                   2 ~ 3.41593, 
                   3 ~ 2.34247, 
                   4 ~ 1.28044, 
                   5 ~ 0.41188,
                   .default = 0) +
        # Vitality (VT)
        case_match(q6br,
                   1 ~ -2.44706,  
                   2 ~ -2.02168,  
                   3 ~ -1.6185, 
                   4 ~ -1.14387,  
                   5 ~ -0.42251,  
                   .default = 0) +
        # Social functioning (SF)
        case_match(q7,
                   1 ~ -0.33682, 
                   2 ~ -0.94342, 
                   3 ~ -0.18043, 
                   4 ~ 0.11038,
                   .default = 0)
    
    # Aggregate mental score
    mcs <- 
        # General health (GH)
        case_match(q1r,
                   1 ~ -1.71175,
                   2 ~ -0.16891,
                   3 ~ 0.03482,
                   4 ~ -0.06064,
                   .default = 0) +
        # Physical functioning (PF)
        case_match(q2a,
                   1 ~ 3.93115,
                   2 ~ 1.8684,
                   .default = 0) +
        case_match(q2b,
                   1 ~ 2.68282,
                   2 ~ 1.43103,
                   .default = 0) +
        # Role physical (RP)
        if_else(q3a == 1, 1.4406, 0) +
        if_else(q3b == 1, 1.66968, 0) +
        # Role emotional (RE)
        if_else(q4a == 1, -6.82672, 0) +
        if_else(q4b == 1, -5.69921, 0) +
        # Bodily pain (BP)
        case_match(q5r,
                   1 ~ 1.48619,
                   2 ~ 1.76691,
                   3 ~ 1.49384,
                   4 ~ 0.90384,
                   .default = 0) +
        # Mental health (MH)
        case_match(q6ar,
                   1 ~ -10.19085,
                   2 ~ -7.92717,
                   3 ~ -6.31121,
                   4 ~ -4.09842,
                   5 ~ -1.94949,
                   .default = 0) +
        case_match(q6c,
                   1 ~ -16.15395, 
                   2 ~ -10.77911, 
                   3 ~ -8.09914, 
                   4 ~ -4.59055, 
                   5 ~ -1.95934,
                   .default = 0) +
        # Vitality (VT)
        case_match(q6br,
                   1 ~ -6.02409,  
                   2 ~ -4.88962,  
                   3 ~ -3.29805, 
                   4 ~ -1.65178,  
                   5 ~ -0.92057,  
                   .default = 0) +
        # Social functioning (SF)
        case_match(q7,
                   1 ~ -6.29724, 
                   2 ~ -8.26066, 
                   3 ~ -5.63286, 
                   4 ~ -3.13896,
                   .default = 0)
    
    # Norm-based standardisation to US population
    pcs <- pcs + 56.57706; mcs <- mcs + 60.75781
    
    ## Return requested component
    # Both
    if(length(return) == 2) return(list(pcs = pcs, mcs = mcs))
    
    # PCS
    else if(return == "pcs") return(pcs)
    
    # MCS
    else if(return == "mcs") return(mcs)
}
