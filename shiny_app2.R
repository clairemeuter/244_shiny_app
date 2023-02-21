library(shiny)
library(tidyverse)
library(lubridate)

ui <- fluidPage(theme="ocean.css",
  navbarPage("Black Bear Aware", #navbarPage allows us to create our tabs
             tabPanel("Landing Page", #this is how we add tabs.
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

             tabPanel("Conflict Exploration"),
             tabPanel("Mapping Conflict",
                      sidebarPanel("Conflict occurances",
                                   selectInput("selectcounty", label = "Select County",
                                               choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),
                                               selected = 1),
                                   dateInput("year", label = "Date input", format = "YYYY", startview = "year"),

                                  selectInput("select_conflict", label = "Type of Conflict",
                                               choices = list("A", "B", "C"),
                                               selected = "A")


                      ), # end sidebar panel
                      mainPanel("Output map",
                                plotOutput("conflict_map")
                      )

                      ), # end  mappiing conflict tab panel

             tabPanel("Mapping Projections")


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

  county_reactive <- reactive({
    x<- data %>%
      filter(county %in% input$selectcounty)
    return(x)
  }) #End sw_reactive

  # select county box
  output$conflict_map <- renderPlot(
    ggplot(data = county_reactive(), aes(lat, long)) +
      geom_point(aes(color = conflict_type))
           ) #end output mapping conflict map
}

shinyApp(ui=ui, server = server)
