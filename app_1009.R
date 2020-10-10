#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(sf)
#library(geojsonio)
library(RSocrata)
library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(RColorBrewer)
#library(DT)
#library(ggplot2)
library(shinycssloaders)
library(stringr)


# USE .Renviron in this directory or Sys.setenv("APP_TOKEN" = "YOUR SOCRATA TOKEN")
readRenviron(file.path("./", ".Renviron"))

# options(shiny.error = '')
# for debugging
#options(shiny.error = browser)

ui <- navbarPage("Chicago Crash Data",
  # titlePanel("Chicago Crash Data"),
  #tabPanel("Main",
  #     # Main Map
  #     fluidRow(
  #       leafletOutput("map")
  #     )
  #),
  tabPanel("Crashes",
     # Point Map
     fluidRow(
      leafletOutput("map")
     ),
     fluidRow(
       column(3,
          h4("Options")
       ),
       column(3,
          h4("About"),
          p("Created by Michael McCarthy using public data from the City of Chicago"),
          a("View the Code on GitHub",href="https://github.com/mmmccarthy/chivz")
        )
    )
  ),
  tabPanel("About",
    includeHTML("meta.html")
  )
)

server <- function(input, output) {
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(
        lng = -87.5,
        lat = 41.9,
        zoom = 10
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

