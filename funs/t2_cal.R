# Function to get calibration metrics of Tobit type 2
t2_cal <- function(.data, outcome){
    # Subset data to observed individuals
    dat_obs <- filter(.data, !is.na(true_y))
    
    # Create data with selection indicator
    dat_full <- mutate(.data, selected = if_else(is.na(true_y), 0, 1))
    
    # Draw calibration plot for selection
    p_s <- ggplot(dat_full, aes(x = pr, y = selected)) +
        # Geometries
        geom_abline(colour = "black",
                    alpha = 0.3) +
        geom_point(alpha = 0.3) +
        geom_smooth(mapping = aes(weight = w),
                    method = "loess",
                    formula = "y ~ x",
                    fill = "#785EF0",
                    colour = "#785EF0") +
        # Transformations
        coord_cartesian(xlim = c(0, 1),
                        ylim = c(0, 1)) +
        # Scaling
        scale_x_continuous(name = paste0("Predicted ", outcome),
                           breaks = seq(0, 1, 0.2)) +
        scale_y_continuous(name = paste0("Observed ", outcome),
                           breaks = seq(0, 1, 0.2)) +
        # Aesthetics
        theme_bw()
    
    # Draw calibration plot for outcome
    p_o <- ggplot(dat_obs, aes(x = y, y = true_y)) +
        # Geometries
        geom_abline(colour = "black",
                    alpha = 0.3) +
        geom_point(alpha = 0.3) +
        geom_smooth(mapping = aes(weight = w),
                    method = "loess",
                    formula = "y ~ x",
                    fill = "#785EF0",
                    colour = "#785EF0") +
        # Scaling
        scale_x_continuous(name = paste0("Predicted ", outcome),
                           limits = c(0, 100),
                           breaks = seq(0, 100, 20)) +
        scale_y_continuous(name = paste0("Observed ", outcome),
                           limits = c(0, 100),
                           breaks = seq(0, 100, 20)) +
        # Aesthetics
        theme_bw()
    
    # Calculate mean observed selection outcome
    obs_s <- mean(!is.na(.data[["true_y"]]))
    
    # Calculate mean predicted selection outcome
    prd_s <- mean(.data[["pr"]])
    
    # Calculate O-E for selection
    citl_s <- format(round(obs_s - prd_s, 3), nsmall = 3)
    
    # Calculate weighted mean observed outcome
    obs_o <- weighted.mean(dat_obs[["true_y"]], dat_obs[["w"]])
    
    # Calculate weighted mean predicted outcome
    prd_o <- weighted.mean(dat_obs[["y"]], dat_obs[["w"]])
    
    # Calculate O-E for outcome
    citl_o <- format(round(obs_o - prd_o, 1), nsmall = 1)
    
    # Fit model for calibration slopes
    fit_cslope <- selection(selection = selected ~ lps_s, 
                            outcome = true_y ~ lps_o,
                            data = dat_full,
                            type = 2,
                            method = "ml")
    
    # Get calibration slope for selection model
    cslope_s <- format(round(coef(fit_cslope)[["lps_s"]], 3), nsmall = 3)
    
    # Get calibration slope for outcome model
    cslope_o <- format(round(coef(fit_cslope)[["lps_o"]], 3), nsmall = 3)
    
    # Calculate discrimination for selection model
    c <- format(round(dat_full %$% Cstat(pr, selected), 2), nsmall = 2)
    
    # Create output list
    output <- list(plot_s = p_s,
                   plot_o = p_o,
                   citl_s = citl_s,
                   citl_o = citl_o,
                   cslope_s = cslope_s,
                   cslope_o = cslope_o,
                   c = c)
    
    # Return output
    return(output)
}

