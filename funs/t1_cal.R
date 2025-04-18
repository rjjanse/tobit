# Function to get calibration metrics of Tobit type 1
t1_cal <- function(.data, outcome, colour = "#785EF0"){
    # Subset data to observed individuals
    dat_obs <- filter(.data, true_y != 0)
    
    # Draw calibration plot
    p <- ggplot(dat_obs, aes(x = y, y = true_y)) +
        # Geometries
        geom_abline(colour = "black",
                    alpha = 0.3) +
        geom_point(alpha = 0.3) +
        geom_smooth(mapping = aes(weight = w),
                    method = "loess",
                    formula = "y ~ x",
                    fill = colour,
                    colour = colour) +
        # Scaling
        scale_x_continuous(name = paste0("Predicted ", outcome),
                           limits = c(0, 100),
                           breaks = seq(0, 100, 20)) +
        scale_y_continuous(name = paste0("Observed ", outcome),
                           limits = c(0, 100),
                           breaks = seq(0, 100, 20)) +
        # Aesthetics
        theme_calplot()
    
    # Calculate weighted mean observed outcome
    obs <- weighted.mean(dat_obs[["true_y"]], dat_obs[["w"]])
    
    # Calculate weighted mean predicted outcome
    prd <- weighted.mean(dat_obs[["y"]], dat_obs[["w"]])
    
    # Calculate O-E
    citl <- round(obs - prd, 1)

    # Fit model for  calibration slope and derive slope
    cslope <- survreg(Surv(true_y, true_y > 0, type = "right") ~ lps,
                      data = .data, 
                      dist = "gaussian")[["coefficients"]][["lps"]]
    
    # Round calibration slope
    cslope <- round(cslope, 3)
    
    # Create output list
    output <- list(plot = p,
                   citl = citl,
                   cslope = cslope)
    
    # Return output
    return(output)
}
    
    