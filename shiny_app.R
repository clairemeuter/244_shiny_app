library(shiny)
library(tidyverse)
library(sf)
library(tmap)
library(here)
library(lubridate)
library(tsibble)


#bear_data_csv
bear_data_csv <- read_csv("data/WIR_clean.csv") %>%
  mutate(date = lubridate::mdy_hm(incident_date),
         year = year(date),
         month = month(date))



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
                                      sidebarPanel(h4("About the project:"),
                                                   p("Grace Bianchi is the coolest."),
                                                   p("Claire Meuter"),
                                                   p("Katheryn Moya"),
                                                   br(), " ",
                                                   h4("About the Data"),
                                                   br(),"",
                                                   p("The data")),
                                      mainPanel(h4(p("About the App")),
                                                p("This project, in coorporation with California Department of Fish and Wildlife, explores human-black bear conflict across California. By analyzing spatial data on suitable bear habitat, human settlement locations, drought and fire extent and severity, and human-wildlife incident reports, we will develop a predictive model to assist wildlife managers in anticipating future conflict."),
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
                                                   radioButtons(
                                                     inputId = "select_date",
                                                     label = "Choices",
                                                     choices = c("Yearly"="year", "Monthly"="month"),
                                                     selected = "year"
                                                   ),
                                                   checkboxGroupInput(
                                                     inputId = "pick_category",
                                                     label = "Choose conflict type:",
                                                     choices = unique(bear_data_csv$confirmed_category), #because we've typed unique here, we don't need to list out the species. WE can do the same thing for types of conflict
                                                     selected = c("Depredation", "General Nuisance", "Potential Human Conflict", "Sighting")
                                                   )
                                      ), #End sidebarPanel widgets
                                      mainPanel(h4(p("Wildlife conflict observations in California")),
                                                p("The user has the opportunity to display conflict data at an annual or monthly scale to visualize the frequency of the various types of conflicts over the years and throughout the seasons."),
                                                p("Maybe some info about the total number of observations? like n="),
                                        plotOutput("conflict_plot"),
                                        p("Figure 1. Wildlife Conflict observations since 2016 collected by the California's Department of Fish & Wildlife (n=4665)")
                                      ) # endconflict exploration mainPanel
                                    ) #end sidebar (tab1) layout
                           ),
                           tabPanel("Mapping Conflict",
                                    sidebarPanel("Conflict occurances",
                                                 selectInput("selectcounty", label = "Select County",
                                                             choices = unique(bear_data_sf$county_name)
                                                 ), # end select input
                                                 selectInput("select_year", label = "Select Year",
                                                                    choices = unique(bear_data_sf$year)),

                                                 selectInput("select_conflict", label = "Type of Conflict",
                                                             choices = unique(bear_data_sf$confirmed_category)),

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
    y<- bear_data_csv %>%
      filter(confirmed_category %in% input$pick_category)
    return(y)
  }) #End conflict_reactive

  # output conflict graph
  output$conflict_plot <- renderPlot({
    # Use input$select_date() to determine whether to group data by year or month
    if (input$select_date == "year") {
      x_var <- "year"
      # axis labels by year
      x_scale <- scale_x_continuous(breaks = seq(min(bear_data_csv$year), max(bear_data_csv$year), by = 1))
      x_limits <- coord_cartesian(xlim = c(min(bear_data_csv$year), max(bear_data_csv$year)))
    } else {
      x_var <- "month"
      x_scale <- scale_x_continuous( breaks = seq(min(bear_data_csv$month), max(bear_data_csv$month), by=1), labels = month.abb)
      x_limits <- coord_cartesian(xlim = c(min(bear_data_csv$month), max(bear_data_csv$month)))

    }

    ggplot(data = conflict_reactive()) +
      geom_bar(aes(fill = confirmed_category, x = !!sym(x_var))) + #convert the x_var string into a variable name for aes
      x_scale +
      scale_fill_manual(breaks = c("Depredation", "General Nuisance", "Potential Human Conflict", "Sighting"),
                        values = c("pink", "peru", "dodgerblue", "darkolivegreen")) +
      labs(y = "Number of observations",
           fill = "Conflict Type", x = "") +
     # x_limits +
      coord_cartesian(ylim = c(0, NA)) +
      theme_bw() +
      theme(axis.text.x = element_text(size = 16),
            axis.text.y = element_text(size = 18),
            axis.title.y = element_text(size=18),
            legend.title = element_text(size = 18),
            legend.text = element_text(size = 16),
            plot.background = element_rect(fill = "#BFD5E3")) # change plot background color to page color

  })



  conflict_map_inputs <- reactive({
    validate(need(try(length(input$select_conflict) > 0),
                  "please make selection")) # error check
    #req(input$map_btn) # button has to be pressed to make map
    g <- bear_conflict_sf %>%
      filter(confirmed_category %in% input$select_conflict) %>%
      filter(year %in% input$select_year) %>%
      filter(county_name %in% input$selectcounty)
    return(g)
  })


  output$conflict_map <- renderTmap({
  tm_shape(bear_conflict_sf) +
      tm_dots() +
    tmap_mode(mode = "view")
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

