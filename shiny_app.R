library(shiny)
library(tidyverse)
library(sf)
library(tmap)
library(here)
library(leaflet)
library(lubridate)


#bear_data_csv
bear_data_csv <- read_csv("data/WIR_clean.csv") %>%
  mutate(date = lubridate::mdy_hm(incident_date),
         year = lubridate::year(date))

#bear_data wrangling
bear_data_sf <- bear_data_csv %>%
  drop_na(latitude, longitude) %>%
  select(c(-species, -behavior_observed, -incident_status)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 3310)

#bear spatial data
bear_conflict_sf <- read_sf(
  here("data","conflict_buffered_refined","conflict_buffered_refined.shp"))

ca_counties_shp <- read_sf(here("data/CA_Counties/CA_counties_TIGER2016.shp")) %>%
  janitor::clean_names() %>%
  select(name)

#st_crs(ca_counties_shp) 3857

ui <- fluidPage(theme="ocean.css",
                navbarPage("Black Bear Aware", #navbarPage allows us to create our tabs
                           tabPanel("About",
                                    sidebarLayout(
                                      sidebarPanel(h4("About the app developers:"),
                                                   p("Grace Bianchi is the coolest."),
                                                   p("Claire Meuter is a second-year MESM student specializing in Conservation Planning.
                                                     As data manager on the Black Bear Aware team, Claire is excited to combine her group project results with Shiny App creation"),
                                                   p("Katheryn Moya"),
                                                   br(), " ",
                                                   h4("About the Data"),
                                                   br(),"",
                                                   p("The data for this project is provided by the ")),
                                      mainPanel(h4(p("Purpose of the App")),
                                                p("This project, in coorporation with California Department of Fish and Wildlife,
                                                  explores human-black bear conflict across California. By analyzing spatial data on
                                                  suitable bear habitat, human settlement locations, drought and fire extent and severity, and human-wildlife incident reports, we will develop a predictive model to assist wildlife managers in anticipating future conflict."),
                                                p("Information about CDFW"),
                                                p("Info about data"),
                                                br(),
                                                p("For more information on this project, see the",  a("UCSB's Bren School Master's Directory", href = 'https://bren.ucsb.edu/projects/black-bear-aware-predicting-human-black-bear-conflict-likelihood-changing-climate'), "."),
                                                br(),

                                      )
                                    )), #end tabpanel thing 1

                           tabPanel("Conflict Exploration", #this is how we add tabs.
                                    sidebarLayout(
                                      sidebarPanel("",
                                                   checkboxGroupInput(
                                                     inputId = "pick_category",
                                                     label = "Choose conflict type:",
                                                     choices = unique(bear_data_csv$confirmed_category), #because we've typed unique here, we don't need to list out the species. WE can do the same thing for types of conflict
                                                     selected = c("Sighting", "Depredation")
                                                   )
                                      ), #End sidebarPanel widgets
                                      mainPanel(h4(p("Wildlife conflict observations by conflict type over time in California")),
                                                p("Information about each conflict type"),
                                                p("Maybe some info about the total number of observations? like n="),
                                        plotOutput("conflict_plot")
                                      ) # endconflict exploration mainPanel
                                    ) #end sidebar (tab1) layout
                           ),
                           tabPanel("Mapping Conflict",
                                    sidebarPanel("Conflict occurances",
                                                 selectInput("select_county", label = "Select County",
                                                             choices = unique(bear_data_sf$county_name)
                                                 ), # end select input

                                                 selectInput("select_year", label = "Select Year",
                                                             choices = unique(bear_data_sf$year)
                                                 ),

                                                 actionButton(inputId = "map_btn", label = "Generate Map")

                                    ), # end sidebar panel
                                    mainPanel("Output map",
                                              tmapOutput("conflict_map")
                                    ) # end main panel

                           ), # end  mappiing conflict tab panel

                           tabPanel("Mapping Projections")


                ) # end navbarPAge
) #end ui

server <- function(input, output){

  # conflict selection for exploration
  conflict_reactive <- reactive({
    validate(need(try(length(input$pick_category) > 0),
                  "please make selection")) # error checking

    y<- bear_data_csv %>%
      filter(confirmed_category %in% input$pick_category) %>%
      group_by(year)  # group to graph counts over time

    return(y)
  }) #End conflict_reactive


  # output conflict graph
  output$conflict_plot <- renderPlot(
    ggplot(data = bear_data_csv, aes(x = year)) +
    geom_bar(data = conflict_reactive(), aes(fill = confirmed_category)) +
    scale_fill_manual(breaks = c("Depredation", "General Nuisance", "Potential Human Conflict", "Sighting"),
                        values = c("pink", "peru", "dodgerblue", "darkolivegreen")) +
      labs(y = "Number of observations", title = "Wildlife Conflict Type by Year")
  ) #end output plotting conflict map



  #conflict_map_inputs <- reactive({
   # validate(need(try(length(input$select_conflict) > 0),
                 # "please make selection")) # error check
    #req(input$map_btn) # button has to be pressed to make map

 #   county <- ca_counties_shp %>%
 #     filter(name %in% input$select_county) %>%
 #     st_set_crs(3310)



#    g <- bear_conflict_sf %>%
 #     filter(county %in% input$select_county) %>%
 #     filter(year %in% input$select_year) %>%
 #     filter(!is.na(geometry)) %>%
  #    st_as_sf() %>%
  #    st_set_crs(3310)

  #  return(g)

 # })
#county map reactive
  county <- reactive({
    ca_counties_shp %>%
      dplyr::filter(name %in% input$select_county)
  })
  #bear points reactive
  g <- reactive({
    bear_conflict_sf %>%
      dplyr::filter(county %in% input$select_county) %>%
      dplyr::filter(year %in% input$select_year) %>%
      dplyr::filter(!is.na(geometry))
  }) #end g reactive

dataTmap <- reactive({
  sf:st_as_sf(
    data.frame(
    type = g()$type,
    county = g()$count,
    year = g()$year,
    geometry = g()$geometry), wkt = "geometry"

  )
})

  output$conflict_map <- renderTmap({
    #tm_shape(county) +
      #tm_polygons(alpha = 0) +
      tm_shape(dataTmap) +
      tm_dots()+
      tmap_mode("view")
}) #end conflict map output}


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

