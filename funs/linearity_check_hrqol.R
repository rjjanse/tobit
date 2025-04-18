# Function to check for linearity in regards to HRQoL
linearity_check_hrqol <- function(outcome){
    # Subset data to temporary data with 1 imputation
    dat_tmp <- filter(dat_imputed, .imp == 1)

    # Set outcome to generic name
    dat_tmp[["outcome"]] <- dat_tmp[[outcome]]
    
    # Set label for y axis
    ylabel <- case_match(outcome,
                         "pcs_6_t1" ~ "PCS at 6 months",
                         "mcs_6_t1" ~ "MCS at 6 months",
                         "pcs_12_t1" ~ "PCS at 12 months",
                         "mcs_12_t1" ~ "MCS at 12 months")
    
    # Create vector of continuous variables
    vec_cont <- c("age", "bmi", "pth", "hb", "pcs_0", "mcs_0", "bmi", "sc", "sb")
    
    # Check linearity for each continuous variable
    dat_lin <- bind_rows(map(vec_cont, \(x){
        # Create formula
        form <- paste0("Surv(outcome, outcome > 0, type = 'left') ~ ns(", x, ", df = 4)")
        
        # Fit model
        fit <- survreg(as.formula(form),
                       data = dat_tmp,
                       dist = "gaussian")
        
        # Create vector of possible values for x
        vec_x <- seq(min(dat_tmp[[x]]), max(dat_tmp[[x]]), length.out = 1e4)
        
        # Create dataframe from vector with named x
        dat_prd <- as_tibble(vec_x) %>%
            # Set column names to x
            set_colnames(x)
        
        # Predict values
        dat_prd[["y"]] <- predict(fit, 
                                  newdata = dat_prd, 
                                  type = "response")
        
        # Get standard error
        dat_prd[["std.err"]] <- predict(fit, 
                                        newdata = dat_prd, 
                                        type = "response",
                                        se.fit = TRUE)[["se.fit"]]
        
        # Rename columns and add indicator
        dat_prd %<>% 
            # Add indicator
            mutate(var = x) %>%
            # Change column name of named x to x
            set_colnames(c("x", "y", "std.err", "var")) %>%
            # Calculate lower and upper limits
            mutate(# Lower limit
                   ll = y - 1.96 * std.err,
                   # Upper limit
                   ul = y + 1.96 * std.err)
        
        # Return data
        return(dat_prd)
    })) %>%
        # Change names of variables
        mutate(var = case_match(var, 
                                "age" ~ "Age (years)",
                                "bmi"~ "BMI (kg/m2)",
                                "mcs_0" ~ "Baseline MCS",
                                "pcs_0" ~ "Baseline PCS",
                                "pth" ~ "PTH",
                                "hb" ~ "Haemoglobin",
                                "sb" ~ "Symptom burden",
                                "sc" ~ "Symptom count"))
    
    # Plot linearity
    p <- ggplot(dat_lin, aes(x = x, y = y, ymin = ll, ymax = ul)) +
        # Geometries
        geom_line() +
        geom_ribbon(alpha = 0.3) +
        # Scalings
        scale_y_continuous(name = ylabel,
                           breaks = seq(0, 100, 20)) +
        # Transformations
        facet_wrap(vars(var),
                   scales = "free") +
        coord_cartesian(ylim = c(0, 100)) +
        # Aesthetics
        theme_bw() +
        theme(axis.title.x = element_blank(),
              panel.grid = element_blank(),
              strip.background = element_blank(),
              strip.text = element_text(face = "bold",
                                        hjust = 0.5))
    
    # Return plot
    return(p)
}
    
