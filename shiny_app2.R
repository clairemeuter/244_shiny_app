library(shiny)
library(tidyverse)

ui <- fluidPage(
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

server <- function(input, output){
  sw_reactive <- reactive({
    x<- starwars %>%
      filter(species %in% input$pick_species) #What does this do? When it sees the input ID "pick_species" it will filter the species in the starwars dataframe by the selected species in the widget
    return(x)
  }) #End sw_reactive


  output$sw_plot <- renderPlot(
    ggplot(data = sw_reactive(),aes(x=mass, y = height)) +
      geom_point(aes(color=species))
  ) #end output$sw_plot

}

shinyApp(ui=ui, server = server)
