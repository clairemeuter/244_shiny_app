#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(theme="ocean.css",
                navbarPage("Black Bear Aware", #navbarPage allows us to create our tabs
                           tabPanel("Thing 1", #this is how we add tabs.
                                    sidebarLayout(
                                      sidebarPanel("WIDGETS",
                                                   checkboxGroupInput(
                                                     inputId = "pick_species",
                                                     label = "Choose species:",
                                                     choices = unique(starwars$species) #because we've typed unique here, we don't need to list out the species. WE can do the same thing for types of conflict
                                                   )
                                      ), #End sidebarPanel widgets
                                      mainPanel("OUTPUT!",
                                                plotOutput("sw_plot")
                                      )
                                    ) #end sidebar (tab1) layout
                           ), #end tabpanel thing 1
                           tabPanel("Thing 2"),
                           tabPanel("Thing 3")

                ) # end navbarPAge
) #end ui

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application
shinyApp(ui = ui, server = server)
