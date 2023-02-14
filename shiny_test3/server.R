#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
<<<<<<< HEAD
library(tidyverse)
=======
>>>>>>> 80baebe59cea15123c59bcd8a2524855a61cf385

# Define server logic required to draw a histogram
function(input, output, session) {

    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
<<<<<<< HEAD
        x    <- WIR_clean[, 2]
=======
        x    <- faithful[, 2]
>>>>>>> 80baebe59cea15123c59bcd8a2524855a61cf385
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')

    })

}
