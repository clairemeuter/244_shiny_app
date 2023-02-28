library(shiny)
library(tidyverse)
library(lubridate)

bear_data <- read_csv("data/WIR_clean.csv")

ui <- fluidPage(theme="ocean.css",
                navbarPage("Black Bear Aware", #navbarPage allows us to create our tabs
                           tabPanel("Landing Page", #this is how we add tabs.
                                    sidebarLayout(
                                      sidebarPanel("WIDGETS",
                                                   checkboxGroupInput(
                                                     inputId = "pick_species",
                                                     label = "Choose species:",
                                                     choices = unique(bear_data$confirmed_category) #because we've typed unique here, we don't need to list out the species. WE can do the same thing for types of conflict
                                                   )
                                      ), #End sidebarPanel widgets
                                      mainPanel(plotOutput("sw_plot")
                                      )
                                    ) #end sidebar (tab1) layout
                           ), #end tabpanel thing 1

                           tabPanel("Conflict Exploration", #this is how we add tabs.
                                    sidebarLayout(
                                      sidebarPanel("WIDGETS",
                                                   checkboxGroupInput(
                                                     inputId = "pick_category",
                                                     label = "Choose conflict type:",
                                                     choices = unique(bear_data$confirmed_category) #because we've typed unique here, we don't need to list out the species. WE can do the same thing for types of conflict
                                                   )
                                      ), #End sidebarPanel widgets
                                      mainPanel(plotOutput("conflict_plot")
                                      )
                                    ) #end sidebar (tab1) layout
                           ),
                           tabPanel("Mapping Conflict",
                                    sidebarPanel("Conflict occurances",
                                                 selectInput("selectcounty", label = "Select County",
                                                             choices = unique(bear_data$county_name)
                                                 ), # end select input
                                                 dateInput("year", label = "Date input", format = "YYYY", startview = "year"),
                                                 selectInput("select_conflict", label = "Type of Conflict",
                                                             choices = unique(bear_data$confirmed_category))

                                    ), # end sidebar panel
                                    mainPanel("Output map",
                                              plotOutput("conflict_map")
                                    ) # end main panel

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

  # conflict ggplot
  conflict_reactive <- reactive({
    x<- bear_data %>%
      filter(confirmed_category %in% input$pick_category)
    return(x)
  }) #End conflict_reactive

  # select county box
  output$conflict_plot <- renderPlot(
    ggplot(data = conflict_reactive(), aes(x=confirmed_category)) +
      geom_bar()
  ) #end output plotting conflict map



  county_reactive <- reactive({
    x<- bear_data %>%
      filter(county_name %in% input$selectcounty)
    return(x)
  }) #End sw_reactive

  # select county box
  output$conflict_map <- renderPlot(
    ggplot(data = county_reactive(), aes(lat, long)) +
      geom_point(aes(color = confirmed_category))
  ) #end output mapping conflict map
}

shinyApp(ui=ui, server = server)

