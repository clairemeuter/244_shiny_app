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

# Define UI for application that displays humna-black bear conflict
ui <- fluidPage(theme="ocean.css",
                navbarPage("Black Bear Aware", #navbarPage allows us to create our tabs
                           tabPanel("Landing Page", #this is how we add tabs.
                                    sidebarLayout(
                                      sidebarPanel("WIDGETS",
                                                   checkboxGroupInput(
                                                     inputId = "pick_category",
                                                     label = "Pick conflict category:",
                                                     choices = unique(WIR_clean$confirmed_category) #because we've typed unique here, we don't need to list out the category of conflict
                                                   )
                                      ), #End sidebarPanel widgets
                                      mainPanel("OUTPUT!",
                                                plotOutput("bear_plot")
                                      )
                                    ) #end sidebar (tab1) layout
                           ), #end tabpanel thing 1
                           tabPanel("Conflict Exploration"),
                           tabPanel("Mapping Conflict"),
                           tabPanel("Mapping Projected Conflict")

                ) # end navbarPAge
) #end ui





# Define server logic required to draw a histogram
server <- function(input, output) {

  bear_reactive <- reactive({
    x<- WIR_clean %>%
      filter(confirmed_category %in% input$pick_species) #What does this do? When it sees the input ID "pick_species" it will filter the species in the starwars dataframe by the selected species in the widget
    return(x)
  }) #End bear_reactive


  output$bear_plot <- renderPlot(
    ggplot(data = bear_reactive(), aes(x=confirmed_category)) +
      geom_bar()
  ) #end output$bear_plot


}


# Run the application

shinyApp(ui = ui, server = server)
