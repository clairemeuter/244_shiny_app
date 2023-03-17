library(shiny)
library(tidyverse)
library(sf)
library(tmap)
library(here)
library(leaflet)
library(lubridate)
library(terra) # add terra
library(shinyWidgets) # for material switch
library(tsibble)
library(DT)




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

tmapIcon <- tmap_icons(here("data","black_bear2.png"))

# read in data & change label - Katheryn

#st_crs(ca_counties_shp) 3857

# present predicted conflict raster data

model_conflict_raster <- rast(here("data","mod3clim_map_squared.tif")) # CRS ID = EPSG 6414

model_conflict_CRS <- st_crs(model_conflict_raster)

CA_county_6414 <- st_transform(ca_counties_shp, 6414)

bear_6414 <- st_crs(model_conflict_CRS)

bear_data_6414_sf <- bear_data_sf %>%
  st_as_sf(crs = bear_6414)

bear_conflict_6414_sf <- read_sf(here("data","conflict_buffered_refined","conflict_buffered_refined.shp")) #set CRS for bear conflict to be the same as the raster

predicted_conflict <- tm_shape(model_conflict_raster) +
  tm_raster(style= "order", palette = "viridis") + # order =
  tmap_mode(mode = "view") +
  tm_layout(legend.outside = TRUE) +
  tm_layout(title = "Modeled Present Probability of Human-Black Bear Conflict in California",
            title.size = 1.5, title.position = c("right", "top")) +
  tm_minimap()

# 2030 predicted conflict raster data

# read in raster data
model_2030_conflict_raster <- rast(here("data","projected_mod3_clim_map_squared.tif"))

# check coordinate reference system

model_2030_conflict_CRS <- st_crs(model_2030_conflict_raster)

model_2030_conflict_CRS # ID = EPSG 6414


## start shiny

ui <- fluidPage(theme="ocean.css",
                navbarPage("Black Bear Aware", #navbarPage allows us to create our tabs
                           tabPanel("About",
                                    sidebarLayout(
                                      sidebarPanel(h4("About the app developers:"),
                                                   tags$style("#project-grid {display: grid;
                      grid-template-columns: 100px 1fr;grid-gap: 10px;}"),
                      h2('Project team'),
                      div(id = "project-grid",
                          div(img(src='claire.jpg', style = 'border-radius: 50%', width = '100px')),
                          div(h3('Claire Meuter'),
                          h4('Claire Meuter is a 2nd year MESM student specializing in Conservation Planning. She is data manager for her masters group project, Black Bear Aware, which studies human-black bear conflict in California. She is excited to combine her research results with the Shiny app interface! ')), #end Claire bio
                          div(img(src='grace.jpeg', style = 'border-radius: 50%', width = '100px')),
                          div(h3('Grace Bianchi'),
                              h4('Grace Bianchi is a 2nd year MESM student specializing in Energy & Climate and Pollution, Prevention, & Remediation.
                                 Her master’s group project focused on creating model to identify the best regions in the United States for rooftop PV on
                                 apartment buildings based on investment favorability.')), #end grace bio
                          ### katheryn
                          div(img(src='Katheryn.jpeg', style = 'border-radius: 50%', width = '100px')),
                          div(h3('Katheryn Moya'),
                              h4('Katheryn Moya is a 2nd year MESM student specializing in Conservation Planning.
                                 Her master’s group project is focused on projecting the impacts of resource extraction on wildlife habitat
                                 in the Greater Chilkat Watershed in Southeastern Alaska.'))), #end katheryn bio


                                                   h2("About the Data"),
                                                   br(),"",

                                                   h4("The data for this project is provided by the ")),
                                      mainPanel(h1("Purpose of the App"),
                                                h5("This app is intended to allow users to explore human-black bear conflict across California.
                                                   Human-black bear conflict can be defined as any interaction between humans and black bears
                                                   that is perceived as a negative interaction by either party. Typically, human-black bear conflict looks like bears utilizing human resources for food,
                                                   often livestock and trash."),

                                                div(img(src = "dumpster_bear.webp", width = '500px')),
                                                div(),
                                                h5("Information about CDFW"),
                                                h5("Info about data"),

                                                br(),
                                                h5("For more information on this project, see the",  a("UCSB's Bren School Master's Directory", href = 'https://bren.ucsb.edu/projects/black-bear-aware-predicting-human-black-bear-conflict-likelihood-changing-climate'), "."),
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
                                                   ),
                                                   h3("Types of Conflict:"),
                                                   h5("An important note"),
                                      ), #End sidebarPanel widgets
                                      mainPanel(h1("Human-black bear conflict observations in California"),
                                                h5("The data displayed below shows reports of human-black bear conflict across California
                                                   as recorded by the California department of Fish and Wildlife (CDFW) from 2016 to March of 2020. Data is recorded by CDFW's Wildlife Incident Reporting System (WIR)."),
                                                h5("- Conflict reports can be displayed by yearly or monthly counts. Select montly to observe seasonal behaviors of bears."),
                                                h5("-Select different conflict types to vizualize the frequency of types of conflict reported."),
                                                h5("Maybe some info about the total number of observations? like n="),
                                        plotOutput("conflict_plot"),
                                        p("Figure 1. Wildlife Conflict observations since 2016 collected by the California's Department of Fish & Wildlife (n=4665)")
                                      ) # endconflict exploration mainPanel
                                    ) #end sidebar (tab1) layout
                           ),
                           tabPanel("Mapping Conflict",

                                    sidebarPanel("Conflict occurrences",
                                                 selectInput(inputId = "select_county", label = "Select County",
                                                             choices = unique(bear_data_sf$county_name),
                                                             selected = "Santa Barbara"
                                                 ), # end select input for select_county

                                                 selectInput("select_year", label = "Select Year",
                                                             choices = unique(bear_data_sf$year),
                                                             selected = 2020
                                                 ), #end select input for year

                                              #   actionButton(inputId = "map_btn", label = "Generate Map")

                                    ), # end sidebar panel
                                    mainPanel(h1("Recorded Conflict Map"),
                                              tmapOutput("tmapMap"),
                                              br(),
                                              h1("table of counts"),
                                              DTOutput("reactive_df"),


                                    ) # end main panel

                           ), # end  mappiing conflict tab panel

                           tabPanel("Modeling Present Conflict",
                                    sidebarPanel("",
                                                 materialSwitch(
                                                   inputId = "overlap_switch1",
                                                   label = "Overlap with WIR Observed Conflict Occurrences:",
                                                   value = FALSE,
                                                   status = "warning"
                                                 )
                                              # end switch button
                                             ), # end sidebar panel
                                    mainPanel("",
                                              tmapOutput("raster_conflict_map")
                                            )), # end main panel

                           tabPanel("Projecting Conflict in 2030",
                                    sidebarPanel("",
                                                 materialSwitch(
                                                   inputId = "overlap_switch2",
                                                   label = "Overlap with WIR Observed Conflict Occurrences:",
                                                   value = FALSE,
                                                   status = "warning"
                                                 )
                                                 # end switch button
                                    ), # end sidebar panel
                                    mainPanel("",
                                              tmapOutput("raster_2030_conflict_map")
                                    ) # end main panel
                ))) # end navbarPAge

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


### reactive conflict map
  data_conflict <- reactive({
    bear_conflict_sf %>%
      filter(county %in% input$select_county) %>%
      filter(year %in% input$select_year)
  })

  ## reactive datatable
  contents <- reactive({
    df <- bear_data_csv %>%
      filter(county_name %in% input$select_county) %>%
      filter(year %in% input$select_year) %>%
      select(-geometry) %>%
      group_by(type) %>%
      tally()
  })

  #reactive conflict points tmap

  dataTmap <- reactive({
    sf::st_as_sf(data.frame(
      type = data_conflict()$type,
      year = data_conflict()$year,
      county = data_conflict()$county,
      geometry = data_conflict()$geometry

    ))
  })




  #county map reactive
  county_map <- reactive({
    ca_counties_shp %>%
      dplyr::filter(name %in% input$select_county)
  })


#reactive map output
  output$tmapMap <- renderTmap({
    tm_shape(county_map()) +
      tm_polygons(alpha=0, border.col = "black", colorNA = NULL) +
    tm_shape(dataTmap()) +
      tm_symbols(shape = tmapIcon, border.lwd = 1, size = 0.5, border.alpha = 1, border.col = "white") +
      tmap_mode("view")  +
      tmap_options(basemaps = "OpenStreetMap")
  })
 #reactive data table
output$reactive_df <- renderDT(contents())


# progress bar for mapping
# data <- eventReactive(input$map_btn, {
#   withProgress(message = "Making map", value = 0, {
#     for (i in 1:10) {
#       incProgress(1 / 10)
#       Sys.sleep(0.5)
#     }
#     #runif(1)
#   })
# })

# output present conflict probability raster map - Katheryn

  test1 <- reactive({
    input$overlap_switch1
  })

  output$raster_conflict_map <- renderTmap({

   over1 <- tm_shape(model_conflict_raster) +
      tm_raster(style= "order", palette = "viridis") + # order =
      tmap_mode(mode = "view") +
      tm_layout(legend.outside = TRUE) +
      tm_layout(title = "Modeled Present Probability of Human-Black Bear Conflict in California",
                title.size = 1.5, title.position = c("right", "top")) +
      tm_minimap()

     if(test1() == TRUE){
       over1 <- over1 +
         tm_shape(bear_conflict_6414_sf) +
         tm_dots(col = "type",
                 palette = c("darkorange","violetred1","firebrick","darkorchid1"))
    }
    over1
  })

# output 2030 conflict probability raster map - Katheryn

  test2 <- reactive({
    input$overlap_switch2
  })

  output$raster_2030_conflict_map <- renderTmap({

    over2 <- tm_shape(model_2030_conflict_raster) +
      tm_raster(style= "order", palette = "viridis") + # order =
      tmap_mode(mode = "view") +
      tm_layout(legend.outside = TRUE) +
      tm_layout(title = "Modeled Present Probability of Human-Black Bear Conflict in California",
                title.size = 1.5, title.position = c("right", "top")) +
      tm_minimap()

    if(test2() == TRUE){
      over2 <- over2 +
        tm_shape(bear_conflict_6414_sf) +
        tm_dots(col = "type",
                palette = c("darkorange","violetred1","firebrick","darkorchid1"))
    }
    over2
  })


 # end conflict probability raster output




#  county_reactive <- reactive({
 #   county<- bear_data %>%
#      filter(county_name %in% input$selectcounty)
#    return(county)
 # }) #End sw_reactive


}

shinyApp(ui=ui, server = server)

