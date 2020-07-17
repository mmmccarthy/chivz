#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(geojsonio)
library(RSocrata)
library(leaflet)
library(tidyverse)
library(RColorBrewer)
library(DT)

Sys.setenv("APP_TOKEN" = "YOUR SOCRATA TOKEN")

options(shiny.error = '')
# Define UI for application that draws a histogram
ui <- fluidPage(

   
   # Application title
   titlePanel("Chicago Crash Data"),
   
   # Main Map
   leafletOutput("map"),
   
   fluidRow(
      column(6,
        selectInput(inputId = "crashtype",
                    label = "Choose a crash type:",
                    choices = c("Pedestrian" = "PEDESTRIAN", "Cyclist" = "PEDALCYCLIST")
                    ),
        dateRangeInput("crashdate", "Date range:",
                       start = "2017-01-01")
       # actionButton("pullCrashes", "Get Crashes for Map Extent"),
       
      ),
      column(6,
      tableOutput("crashsummary")
      )
  ),
  fluidRow(
    column(12,
      DT::dataTableOutput("crashes")
      )
  )
)





server <- function(input, output) {
  
  # Get Data
  # Bike Routes from Chicago Data Portal (Updated Feb 2020)
    bikeroutes <- geojsonio::geojson_read("geo/bikeroutes.geojson", what = "sp")
  
  # Map
    pal <- colorFactor(
      palette = c("#7E5109","#7E5109","#85D8FC","#85D8FC","#ABEBC6","#2ECC71","#FCDE66"),
      levels = c("ACCESS PATH","OFF-STREET TRAIL","BIKE LANE","BUFFERED BIKE LANE","NEIGHBORHOOD GREENWAY","PROTECTED BIKE LANE","SHARED-LANE")
    )
    
    pal2 <- colorFactor(
      palette = "YlOrRd",
      levels = c("","NO INDICATION OF INJURY","REPORTED, NOT EVIDENT","NONINCAPACITATING INJURY","INCAPACITATING INJURY","FATAL")
    )
    
    output$map <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(
          lng = -87.5,
          lat = 41.9,
          zoom = 10
        ) %>%
        addPolylines(
          data = bikeroutes,
          group = "Existing Bike Routes",
          color = ~pal(displayrou),
          opacity = 0.5,
          weight = 3,
          popup = ~paste0("<strong>",str_to_title(street),"</strong><br>",str_to_title(displayrou))
        ) %>%
        addLegend(
          position = "topright",
          title = "Existing Bike Routes",
          colors = c("#7E5109","#85D8FC","#ABEBC6","#2ECC71","#FCDE66"),
          labels = c("Off-street Trails","Standard and Buffered Bike Lanes","Neighborhood Greenways","Protected Bike Lanes","Marked Shared Lanes (\"Sharrows\")")
        ) %>%
        addCircleMarkers(
          data = crash_data(),
          lng = ~longitude,
          lat = ~latitude,
          radius = ~ifelse(most_severe_injury == "FATAL", 10, 4),
          stroke = FALSE,
          fillColor = ~pal2(most_severe_injury),
          fillOpacity = 2,
          popup = ~paste0("<strong>",street_no," ",street_direction," ",str_to_title(street_name),"</strong><br>Date: ",format(crash_date, format = "%b %d, %Y"),"<br>Type: ",str_to_title(first_crash_type),"<br>Most severe injury: ",str_to_title(most_severe_injury))
        ) # %>%
        #addLegend(
        #  position = "topright",
        #  title = "Crash Injury Types", pal = pal2, values = ~most_severe_injury
        #)
    })
    
  # Update Crashes based on inputs
    crash_data <- reactive({
      url <-   "https://data.cityofchicago.org/resource/85ca-t3if.json?$"
      start_date <- format(input$crashdate[1])
      end_date <- format(input$crashdate[2])
      date <- paste0("crash_date between ","'",start_date,"'"," and ","'",end_date,"'")
      type <- paste0("first_crash_type == '",input$crashtype,"'")
      query <- paste0(url,"where=",date," AND ",type)
      crashes <- read.socrata(query, app_token = Sys.getenv("APP_TOKEN"))
      
      # Fix Lat/Long for use with Leaflet
      crashes$latitude = as.numeric(crashes$latitude)
      crashes$longitude = as.numeric(crashes$longitude)
      
      # Eliminate non-Chicago or missing lat/long points
      crashes <- crashes %>%
        filter(latitude > 35 & longitude < -85)
      return(crashes)
    })
    
    visible_crashes <- reactive({
      if (!is.null(input$map_bounds)) {
        visible <- crash_data() %>%
          filter(latitude <= input$map_bounds$north & latitude > input$map_bounds$south & longitude < input$map_bounds$east & longitude >= input$map_bounds$west)
      
      return(visible)
      }else{
        return(NULL)
      }
    })
   
   output$crashsummary <- renderTable(
     if (!is.null(visible_crashes())) {
     visible_crashes() %>%
           mutate(year = format(crash_date,"%Y")) %>%
           group_by(most_severe_injury,year) %>%
           summarize(total = n())%>%
           spread(year,total,fill=NA,convert=FALSE)
        }
       )
   output$crashes <- DT::renderDataTable(
     if (!is.null(visible_crashes())) {
     visible_crashes() %>%
       select(crash_date,first_crash_type,most_severe_injury,latitude,longitude)
     }
     )
}

# Run the application 
shinyApp(ui = ui, server = server)

