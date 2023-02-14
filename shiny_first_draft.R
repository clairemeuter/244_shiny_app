library(shiny)
library(tidyverse)
library(sf)
library(here)

# Create my user interface
ui <- fluidPage(
  titlePanel("Black Bear Aware: Human-Black Bear Interaction in California"),
  sidebarLayout(
    sidebarPanel("Put my widgets here!",
                 #radioButtons()
                 ), #end sidebarPanel
    mainPanel("put my map here")
  ) #end main panel
) #end fluidPage

### Create the server function
# read in data (i don't know how to do this yet)
# conflict <- read_sf(here("conflict_buffered_refined.shp"))
server <- function(input, output){
  conflict_select <- reactive({

  })
}

shinyApp(ui=ui,server = server)

