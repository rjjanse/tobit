# Function for theming calibration plots
theme_calplot <- function(){
    # Set theme elements
    theme_bw() +
    theme(plot.background = element_rect(colour = "white",
                                         fill = "white"),
          panel.grid = element_blank(),
          plot.title = element_text(face = "bold",
                                    size = 12,
                                    hjust = 0.5))
}

