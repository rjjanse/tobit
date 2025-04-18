# Function for theming calibration plots
theme_calplot <- function(){
    # Set theme elements
    theme_bw() +
    theme(plot.background = element_rect(colour = "white",
                                         fill = "white"),
          panel.grid = element_blank())
}
