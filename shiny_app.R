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

tmapIcons <- tmap_icons(c(here("data","dep_bear.png"),
                         here("data","gen_bear.png"),
                         here("data","po_bear.png"),
                         here("data","sight_bear.png")))

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
                           navbarMenu("About", icon = icon("info-circle"),
                                      tabPanel("The Project",
                                               sidebarLayout(
                                                 sidebarPanel(h2("The Black Bear Aware Project"),
                                                              h4("Black Bear Aware is a master’s group thesis project from the Bren School
                                                                 of Environmental Science and Management at the University of California,
                                                                 Santa Barbara. This project is in collaboration with the California Department
                                                                 of Fish and Wildlife (CDFW). The Black Bear Aware project aims to model likely
                                                                 regions of human-black bear conflict and inform agencies like CDFW on where to
                                                                 focus prevention, education, and mediation efforts. Learn more about the Black Bear
                                                                 Aware project ",a("here",
                                                                                   href = 'https://bren.ucsb.edu/projects/black-bear-aware-predicting-human-black-bear-conflict-likelihood-changing-climate')," and in the About the Data tab."),
                                                   h2("Reporting and Preventing Human-Wildlife Conflict:"),

                                                              h4("Curious what the Wildlife Incident Reporting (WIR) form looks like?
                                                                 Have a wildlife encounter you want to share with CDFW?
                                                                 Check out the WIR system ",a("here.",href = 'https://apps.wildlife.ca.gov/wir')," "),
                                                              h4("Explore CDFW's informational human-wildlife conflict page ",a("here",href = 'https://wildlife.ca.gov/Conservation/Laboratories/Wildlife-Health/HWC-Program#551962502-black-bear'),"."),
                                                              ), #end sidebar layout
                                               mainPanel(h1("Purpose of the Black Bear Aware Shiny App"),
                                                         h5("This app allows users to explore human-black bear conflict data across California.
                                                            Residents of California can explore what conflict reports are like in their
                                                            county and see how likely conflict is in their region. Human-black bear conflict
                                                            can be defined as any interaction between humans and black bears that is perceived
                                                            as a negative interaction by either party. Typically, human-black bear conflict
                                                            looks like bears utilizing human resources for food, often livestock and trash."),
                                                   h6("Navigating the App"),
                                                   h5("Click on the exploration tab to see the types of human-black bear
                                                      conflict recorded by the California Department of Fish and Wildlife
                                                      from 2016 to early 2022. In the Mapping Conflict tab, explore recorded
                                                      human-black bear conflict across California by county and year.
                                                      Finally, see the results of modeling for human-black bear conflict
                                                      across California in the projections tab. Select “Modeling Present
                                                      Conflict” to see the % likelihood of human-black bear conflict across
                                                      California under present-day environmental conditions. Select
                                                      “Conflict in 2030” to see the % likelihood of human-black bear
                                                      conflict across California in the year 2030, with environmental
                                                      conditions based on climate projections. Visit About <- The Data to learn more
                                                      about modeling liklihood of human-black bear conflict in California."),

                                                   div(img(src = "dumpster_bear.webp", width = '500px')),


                                               ), #end mainPanel

                                               )), # end About the project tab pan
                                      tabPanel("The Team",
                    #  h2('Project team'),
                      tags$style("#project-grid {display: grid;grid-template-columns: 100px 1fr;grid-gap: 10px;}"),
                      div(id = "project-grid"),
                          fluidRow(column(4,
                                         div(img(src='claire.jpg', style = 'border-radius: 50%', width = '100px'), style="text-align: center;"),
                                          h6('Claire Meuter'),
                                          h5('Claire Meuter is a 2nd year MESM student specializing in Conservation Planning.
                                             She is data manager for her masters group project, Black Bear Aware, which studies human-black bear conflict in California.
                                             She is excited to combine her research results with the Shiny app interface! ')), #end Claire bio

                                   column(4,
                                          div(img(src='grace.jpeg', style = 'border-radius: 50%', width = '100px'), style="text-align: center;"), #end claire bio
                                          h6('Grace Bianchi'),
                                          h5('Grace Bianchi is a 2nd year MESM student specializing in Energy & Climate and Pollution, Prevention, & Remediation.
                                 Her master’s group project focused on creating model to identify the best regions in the United States for rooftop PV on
                                 apartment buildings based on investment favorability.')), # end grace bio

                                 column(4,
                                        div(img(src='Katheryn.jpeg', style = 'border-radius: 50%', width = '110px', height = "145px"), style="text-align: center;"),
                                         h6('Katheryn Moya'),
                                          h5('Katheryn Moya is a 2nd year MESM student specializing in Conservation Planning.
                                 Her master’s group project is focused on projecting the impacts of resource extraction on wildlife habitat
                                 in the Greater Chilkat Watershed in Southeastern Alaska.')),
                                 )),

                                      tabPanel("The Data",
                                              h1("About the Data"),
                                              h6("Data provided by the California Department of Fish and Wildlife (CDFW):"),
                                              h5("Conflict data came from the California Department of Fish and Wildlife’s (CDFW)
                                                 online wildlife incident reporting system (WIR). The general public can use
                                                 this online database to report black bear interactions that fall into
                                                 4 categories: depredation, general nuisance, potential human conflict,
                                                 or sightings. Reports from the WIR system include the latitude and longitude
                                                 of the points and the date of the interaction, along with record numbers and
                                                 the details of each event. The online WIR system has been active since 2016, and
                                                 CDFW provided WIR data from 2016 to February 2022."),
                                              h6("Types of conflict:"),
                                              h5("Depredation- Indicates that Black Bear has damaged human property/livestock,
                                                 and has reported damages to CDFW biologist. Individuals reporting depredation
                                                 by a black bear can apply for a take permit to kill the bear."),
                                              h5("General nuisance- Bear behavior that is generally inconvenient or harmful
                                                 to human property/livestock."),
                                              h5("Potential human conflict- Recorded bear behavior that indicates bears
                                                 might be habituated to human presence and more likely to utilize human spaces."),
                                              h5("Sighting- Reports of a bear sighting. While a sighting may not indicate a direct
                                                 conflict, reported sightings provide information about bears that are utilizing
                                                 human spaces and are of sufficient concern to be reported to the state management agency."),

                                              h6("Conflict Likelihood Maps produced by Black Bear Aware Team:"),
                                              h5("In partnership with the California Department of Fish and Wildlife, the Black Bear Aware
                                                 team created a model to predict the likelihood of human-black bear conflict in California.
                                                 The Black Bear Aware predictive model replicates and builds upon methodology found in Hagani
                                                 et al., (2021), “Mapping and modeling human-black bear interactions in the Catskills region of
                                                 New York”. Hagani et al. used a resource selection probability function (RSPF) to model conflict.
                                                 The RSPF is a common function in spatial ecology, often used to study large carnivore distribution."),
                                              h5("The Black Bear Aware team built a model in RStudio based on the variables provided in the
                                                 Hagani et al. paper and improved the fit of the model by including fire and drought climate
                                                 variables. The most parsimonious model was selected, which used the following variables:
                                                 elevation, land cover, distance to forests, population density, distance to recreational areas,
                                                 distance to streams, terrain ruggedness, distance to urban areas, distance to recent fires, and drought."),
                                              h5("The output of this model is a raster map of California that displays the likelihood of conflict from 0%
                                                 to 100% likely. By integrating climate predictions for 2030, the team also produced a map of the likelihood
                                                 of conflict across California in 2030. These maps can be explored under the “Projections” tab."),
                                              h5("Reference: Hagani, J. S., Kross, S. M., Clark, M., Wynn-Grant, R., & Blair, M. (2021).
                                                 Mapping and modeling human-black bear interactions in the Catskills region of New York
                                                 using resource selection probability functions. PLOS ONE, 16(9), e0257716.")
                                              ) #end data tab bar
                           ), # end navbar menu with more icon

                           tabPanel("Exploration", #this is how we add tabs.
                                    sidebarLayout(
                                      sidebarPanel("",
                                                   h2("Using this tool:"),
                                                   h3("Conflict reports can be displayed by yearly or monthly counts.
                                                      Select montly to observe seasonal behaviors of bears."),
                                                   h3("Select different conflict types to vizualize the
                                                      frequency of types of conflict reported"),
                                                   radioButtons(
                                                     inputId = "select_date",
                                                     label = "Yearly or Monthly",
                                                     choices = c("Yearly"="year", "Monthly"="month"),
                                                     selected = "year"
                                                   ),
                                                   checkboxGroupInput(
                                                     inputId = "pick_category",
                                                     label = "Choose conflict type:",
                                                     choices = unique(bear_data_csv$confirmed_category), #because we've typed unique here, we don't need to list out the species. WE can do the same thing for types of conflict
                                                     selected = c("Depredation", "General Nuisance", "Potential Human Conflict", "Sighting")
                                                   ),

                                      ), #End sidebarPanel widgets
                                      mainPanel(h1("Human-black bear conflict observations in California"),
                                                h6("Types of Conflict"),
                                                h5("Depredation- Indicates that Black Bear has damaged human
                                                   property/livestock, and has reported damages to CDFW biologist.
                                                   Individuals reporting depredation by a black bear can apply
                                                   for a take permit to kill the bear."),
                                                h5("General nuisance- Bear behavior that is generally inconvenient or
                                                   harmful to human property/livestock."),
                                                h5("Potential human conflict- Recorded bear behavior that indicates
                                                   bears might be habituated to human presence and more likely to utilize
                                                   human spaces."),
                                                h5("Sighting- Reports of a bear sighting. While a sighting may not
                                                   indicate a direct conflict, reported sightings provide information
                                                   about bears that are utilizing human spaces and are of sufficient
                                                   concern to be reported to the state management agency."),
                                        plotOutput("conflict_plot"),
                                        h5("Figure 1. The data displayed above shows counts of human-black bear conflict across California
                                                   as recorded by the California department of Fish and Wildlife (CDFW) from 2016 to February
                                           of 2022. Data is recorded by CDFW's Wildlife Incident Reporting System (WIR). Over that time period,
                                           4,663 human-black bear interactions were recorded."),


                                      ) # endconflict exploration mainPanel
                                    ) #end sidebar (tab1) layout
                           ),
                           tabPanel("Mapping Conflict",

                                    sidebarPanel("",
                                                 h2("Using this tool:"),
                                                 h3("Select by county to explore which counties in California are conflict-prone. Select by year to compare changes in conflict on counties over time."),
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

                                              h5("The data below spatially displays reports of human-black bear conflict across California
                                                 as recorded by the California Department of Fish and Wildlife (CDFW) from 2016 to March 2022.
                                                 Data is recorded by CDFW's Wildlife Incident Reporting System (WIR). Each bear icon indicates a reported human-black bear conflict."),
                                              tmapOutput("tmapMap"),
                                              br(),
                                             # h1("table of counts"),
                                              DTOutput("reactive_df"),


                                    ) # end main panel

                           ), # end  mappiing conflict tab panel
                      navbarMenu("Projections",
                           tabPanel("Modeling Present Conflict",
                                    sidebarPanel("",
                                                 h2("Using this tool:"),
                                                 h3("Use the interactive map interface to zoom into regions of
                                                    California and explore conflict likelihood."),
                                                 h3("Observation points of human-black bear conflict from 2016
                                                    to 2022 as recorded in the WIR can be overlapped using the
                                                    toggle switch to assess the difference between reported
                                                    (actual) conflict and predicted conflict."),
                                                 materialSwitch(
                                                   inputId = "overlap_switch1",
                                                   label = "Overlap with WIR Recorded Conflict Points:",
                                                   value = FALSE,
                                                   status = "warning"
                                                 )
                                              # end switch button
                                             ), # end sidebar panel
                                    mainPanel(h1("Modeling Present Conflict"),

                                                h5("In partnership with the California Department of Fish and
                                                   Wildlife, the Black Bear Aware team created a model to predict
                                                   the likelihood of human-black bear conflict in California. The
                                                   most parsimonious model was selected, which used the following
                                                   variables: elevation, land cover, distance to forests, population
                                                   density, distance to recreational areas, distance to streams, terrain ruggedness,
                                                   distance to urban areas, distance to recent fires, and drought."),

                                              h5("The following map displays the results of the model,
                                                 with areas in yellow depicting areas with the highest probability
                                                 (75% to 100%) of human-black bear conflict."),

                                             tmapOutput("raster_conflict_map")
                                            )), # end main panel

                           tabPanel("Conflict in 2030",
                                    sidebarPanel("",
                                                 h2("Using this tool:"),
                                                 h3("Use the interactive map interface to zoom into regions of
                                                    California and explore conflict likelihood."),
                                                 h3("Observation points of human-black bear conflict from 2016
                                                    to 2022 as recorded in the WIR can be overlapped using the
                                                    toggle switch to assess the difference between reported
                                                    (actual) conflict and predicted conflictin 2030."),
                                                 materialSwitch(
                                                   inputId = "overlap_switch2",
                                                   label = "Overlap with WIR Observed Conflict Occurrences:",
                                                   value = FALSE,
                                                   status = "warning"
                                                 )
                                                 # end switch button
                                    ), # end sidebar panel
                                    mainPanel("",
                                              h1("Modeling Future Conflict (2030)"),
                                              h5("In partnership with the California Department of Fish and
                                                 Wildlife, the Black Bear Aware team created a model to predict
                                                 the likelihood of human-black bear conflict in California.
                                                 The most parsimonious model was selected, which used the following
                                                 variables: elevation, land cover, distance to forests, population
                                                 density, distance to recreational areas, distance to streams, terrain
                                                 ruggedness, distance to urban areas, distance to recent fires, and
                                                 drought. By integrating climate predictions for 2030, the team produced
                                                 a map of the likelihood of conflict across California in 2030."),
                                              h5("The following map displays the results of the model, with areas in yellow
                                                 depicting areas with the highest probability (75% to 100%) of human-black bear conflict
                                                 in 2030."),

                                              tmapOutput("raster_2030_conflict_map")
                                    )) # end main panel
                      )
                )) # end navbarPAge

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
      tm_symbols(shape = "type", shapes = tmapIcons, border.lwd = 1, size = 0.5, border.alpha = 1, border.col = "white") +
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
      tm_raster(style= "order", palette = "viridis", title = "Conflict Probability") + # order =
      tmap_mode(mode = "view") +
      tm_layout(title = "Present Human-Black Bear Conflict Probability in CA",
                title.size = 1.5,
                title.position = c("right", "top"),
                legend.outside = TRUE) +
      tm_minimap()

     if(test1() == TRUE){
       over1 <- over1 +
         tm_shape(bear_conflict_6414_sf) +
         tm_dots(col = "type",
                 palette = c("#df7d60", "#aca0bd", "#778aab", "#b9cdca"),
                 title = "Types of Conflict")
    }
    over1
  })

# output 2030 conflict probability raster map - Katheryn

  test2 <- reactive({
    input$overlap_switch2
  })

  output$raster_2030_conflict_map <- renderTmap({

    over2 <- tm_shape(model_2030_conflict_raster) +
      tm_raster(style= "order",
                palette = "viridis",
                title = "Conflict Probability") + # order =
      tmap_mode(mode = "view") +
      tm_layout(legend.outside = TRUE) +
      tm_layout(title = "2030 Human-Black Bear Conflict Probability in CA",
                title.size = 1.5, title.position = c("right", "top")) +
      tm_minimap()

    if(test2() == TRUE){
      over2 <- over2 +
        tm_shape(bear_conflict_6414_sf) +
        tm_dots(col = "type",
                palette = c("#df7d60","#aca0bd","#778aab","#b9cdca"))
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

