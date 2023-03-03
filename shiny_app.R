library(shiny)
library(tidyverse)
library(sf)
library(tmap)
library(here)
library(lubridate)

bear_data <- read_sf("data/WIR_clean.csv") %>%
  mutate(date = lubridate::mdy_hm(incident_date),
         year = year(date))


ca_counties_shp <- read_sf(here("data/CA_Counties/CA_counties_TIGER2016.shp")) %>%
  janitor::clean_names() %>%
  select(name)

#st_crs(ca_counties_shp) 3857

ui <- fluidPage(theme="ocean.css",
                navbarPage("Black Bear Aware", #navbarPage allows us to create our tabs
                           tabPanel("Landing Page", p("This project, in coorporation with California Department of Fish and Wildlife, explores human-black bear conflict across California. By analyzing spatial data on suitable bear habitat, human settlement locations, drought and fire extent and severity, and human-wildlife incident reports, we will develop a predictive model to assist wildlife managers in anticipating future conflict.")), #end tabpanel thing 1

                           tabPanel("Conflict Exploration", #this is how we add tabs.
                                    sidebarLayout(
                                      sidebarPanel("WIDGETS",
                                                   checkboxGroupInput(
                                                     inputId = "pick_category",
                                                     label = "Choose conflict type:",
                                                     choices = unique(bear_data$confirmed_category), #because we've typed unique here, we don't need to list out the species. WE can do the same thing for types of conflict
                                                     selected = c("Sighting", "Depredation")
                                                   )
                                      ), #End sidebarPanel widgets
                                      mainPanel(plotOutput("conflict_plot")
                                      ) # endconflict exploration mainPanel
                                    ) #end sidebar (tab1) layout
                           ),
                           tabPanel("Mapping Conflict",
                                    sidebarPanel("Conflict occurances",
                                                 selectInput("selectcounty", label = "Select County",
                                                             choices = unique(bear_data$county_name)
                                                 ), # end select input
                                                 selectInput("select_year", label = "Select Year",
                                                                    choices = unique(bear_data$year)),

                                                 selectInput("select_conflict", label = "Type of Conflict",
                                                             choices = unique(bear_data$confirmed_category)),

                                                 actionButton(inputId = "map_btn", label = "Map")

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

  # conflict selection for exploration
  conflict_reactive <- reactive({
    validate(need(try(length(input$pick_category) > 0),
                  "please make selection")) # error checking
    y<- bear_data %>%
      filter(confirmed_category %in% input$pick_category) %>%
      group_by(year) # group to graph counts over time
    return(y)
  }) #End conflict_reactive


  # output conflict graph
  output$conflict_plot <- renderPlot(
    ggplot(data = bear_data, aes(x = year)) +
      geom_bar(data = conflict_reactive(), aes(fill = confirmed_category))
  ) #end output plotting conflict map



  conflict_map_inputs <- reactive({
    validate(need(try(length(input$select_conflict) > 0),
                  "please make selection")) # error check
    req(input$map_btn) # button has to be pressed to make map
    g <- bear_data %>%
      filter(confirmed_category %in% input$select_conflict) %>%
      filter(year %in% input$select_year) %>%
      filter(county_name %in% input$selectcounty)
    return(g)
  })


output$conflict_map <- renderPlot({
  req(data()) # one progress bar once inputs are complete
  ggplot() +
    geom_sf(data = ca_counties_shp, color = "white") +
    geom_sf(data = conflict_map_inputs(), aes(x = longtitude, y = latitude))
}
)

# progress bar for mapping
data <- eventReactive(input$map_btn, {
  withProgress(message = "Making map", value = 0, {
    for (i in 1:10) {
      incProgress(1 / 10)
      Sys.sleep(0.5)
    }
    #runif(1)
  })
})


#  county_reactive <- reactive({
 #   county<- bear_data %>%
#      filter(county_name %in% input$selectcounty)
#    return(county)
 # }) #End sw_reactive


}

shinyApp(ui=ui, server = server)

