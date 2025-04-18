# Function to get calibration metrics of Tobit type 2
t2_cal <- function(.data, outcome, colour = "#785EF0"){
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
                    fill = colour,
                    colour = colour) +
        # Transformations
        coord_cartesian(xlim = c(0, 1),
                        ylim = c(0, 1)) +
        # Scaling
        scale_x_continuous(name = "Predicted probability",
                           breaks = seq(0, 1, 0.2)) +
        scale_y_continuous(name = "Observed probability",
                           breaks = seq(0, 1, 0.2)) +
        # Aesthetics
        theme_calplot()
    
    # Get 1st and 3rd quantiles
    q1 <- quantile(dat_full[["pr"]], probs = 0.025); q3 <- quantile(dat_full[["pr"]], probs = 0.975)
    
    # Draw zoomed calibration plot of interquartile range of predictions for selection model
    p_s_zoom <- ggplot(dat_full, aes(x = pr, y = selected)) +
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
                           breaks = seq(0, 1, 0.04)) +
        scale_y_continuous(name = paste0("Observed ", outcome),
                           breaks = seq(0, 1, 0.04)) +
        # Transformations
        coord_cartesian(xlim = c(q1, q3),
                        ylim = c(q1, q3)) +
        # Labels
        ggtitle("Close-up") +
        # Aesthetics
        theme_calplot() +
        theme(axis.title = element_blank(),
              plot.title = element_text(face = "bold",
                                        hjust = 0.5,
                                        size = 12))
    
    # Add zoomed plot to full plot
    p_s <- p_s + inset_element(p_s_zoom, 
                               0.025, 0.6, 0.4, 0.975)
                               
    
    # Draw calibration plot for outcome
    p_o <- ggplot(dat_obs, aes(x = y, y = true_y)) +
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

    # Calculate mean observed selection outcome
    obs_s <- mean(!is.na(.data[["true_y"]]))
    
    # Calculate mean predicted selection outcome
    prd_s <- mean(.data[["pr"]])
    
    # Calculate O-E for selection
    citl_s <- round(obs_s - prd_s, 3)
    
    # Calculate weighted mean observed outcome
    obs_o <- weighted.mean(dat_obs[["true_y"]], dat_obs[["w"]])
    
    # Calculate weighted mean predicted outcome
    prd_o <- weighted.mean(dat_obs[["y"]], dat_obs[["w"]])
    
    # Calculate O-E for outcome
    citl_o <- round(obs_o - prd_o, 1)
    
    # Fit model for calibration slopes
    fit_cslope <- selection(selection = selected ~ lps_s, 
                            outcome = true_y ~ lps_o,
                            data = dat_full,
                            type = 2,
                            method = "ml")
    
    # Get calibration slope for selection model
    cslope_s <- round(coef(fit_cslope)[["lps_s"]], 3)
    
    # Get calibration slope for outcome model
    cslope_o <- round(coef(fit_cslope)[["lps_o"]], 3)
    
    # Calculate discrimination for selection model
    c <- round(dat_full %$% Cstat(pr, selected), 2)
    
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

