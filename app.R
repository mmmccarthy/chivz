#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
###############################################
#
# Chicago Vision Zero Bounds selector
# By Michael McCarthy
# Created: 2021-09-04
# Last update: 2022-01-03
#
###############################################

library(shiny)
library(sf)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(RSocrata)
library(plotly)
# library(tidyverse)
# library(stringr)
# library(promises)
# library(future)
# plan(multisession)

options(shiny.error = '')

source("functions.R")

# Load cached crash data (IDOT: 2009-2017, Chicago: 2018-2021)
crashes_cached <- readRDS("Crashes_2009_2021_injury.rds")

new_crashes <- updateCrashes()
crash_data <- reactive({ rbind(crashes_cached, new_crashes) })

# set years
#start_year = min(crash_data()$crash_date$year+1900)
#end_year = max(crash_data()$crash_date$year+1900)

# Crash date range
#oldest_crash = min(as.Date(crash_data()$crash_date))
#most_recent_crash = max(as.Date(crash_data()$crash_date))

# constants for now TODO
start_year = 2009
end_year = 2022

ui <- navbarPage("Chicago Crash Data",
  id = "open_tab",
  tabPanel("Map",
     fluidRow(
       column(10,
        leafletOutput("map"),
        span(downloadLink("dlCSV","Download CSV File")) # ,downloadLink("dlGeo","Download GeoJson File"))
        ),
       column(2,
              h4("Options"),
              wellPanel(
                selectInput(inputId = "crashtype",
                            label = "Crash Type:",
                            choices = c("Pedestrian" = "PEDESTRIAN", "Pedalcyclist" = "PEDALCYCLIST", 
                                        "Rear End" = "REAR END", "Sideswipe Same Direction" = "SIDESWIPE SAME DIRECTION", "Fixed Object" = "FIXED OBJECT", "Other Object" = "OTHER OBJECT", 
                                        "Other Non-Collision" = c("OTHER NON-COLLISION", "OTHER NONCOLLISION"), "Parked Motor Vehicle" = "PARKED MOTOR VEHICLE", "Angle" = "ANGLE", "Turning" = "TURNING", 
                                        "Overturned" = "OVERTURNED", "Head On"  = "HEAD ON", "Sideswipe Opposite Direction" = "SIDESWIPE OPPOSITE DIRECTION",  "Animal" = "ANIMAL", "Train" = "TRAIN", "Rear to Side" = "REAR TO SIDE", 
                                        "Rear to Front" = "REAR TO FRONT", "Rear to Rear" = "REAR TO REAR"),
                            multiple = TRUE,
                            selected = c("PEDESTRIAN","PEDALCYCLIST")
                ),
                selectInput(inputId = "injtype",
                            label = "Most Serious Injury/Fatality:",
                            choices = c("Fatal (K)" = "FATAL","Incapacitating (A)" = "INCAPACITATING INJURY",
                                        "Non-Incapacitating (B)" = "NONINCAPACITATING INJURY", "Injury Reported, Not Evident (C)" = "REPORTED, NOT EVIDENT",
                                        "No Injury (Property Damage Only)"  = "NO INDICATION OF INJURY"),
                            multiple = TRUE,
                            selected = c("FATAL","INCAPACITATING INJURY")
                ),
                sliderInput(inputId = "hours",
                            label = "Hour range:",
                            min = 0,
                            max = 24,
                            step = 1,
                            sep = "",
                            ticks = FALSE,
                            value = c(0,24)
                ),
                sliderInput(inputId = "years",
                            label = "Year range:",
                            min = start_year,
                            max = end_year,
                            step = 1,
                            sep = "",
                            ticks = FALSE,
                            value = c(start_year,end_year)
                ),
                actionButton("update", "Update Map")
              ),
              p("Click Update Map to populate the map with crashes matching your selections.")
            )
       ),
       fluidRow(
         column(6,
                h3("Crashes by Year and Type"),
                h5("Limited to crashes currently displayed in the map"),
                plotlyOutput("crashperyear")
                ),
         column(6,
                h3("Crash Details"),
                h5("Limited to crashes currently displayed in the map"),
                DT::dataTableOutput("crashtable")
         )
                
      ),
      fluidRow(
        column(4,
               h4("About the Data"),
               p(textOutput("date_range")," To reduce storage and memory needs, this map only displays crashes where the most severe injury was not No Indication of Injury. Crashes with any fatality or reported injury, whether evident to first responders or not, are included."),
               #p("To reduce"),
               h4("About this App"),
               p("Created by Michael McCarthy using public data from the City of Chicago and IDOT"),
               p("Built with R, Shiny, and Leaflet"),
               a("View the Code on GitHub",href="https://github.com/mmmccarthy/chivz")
               ),
        column(8,
               h4("Crash Data for 2018 to present"),
               p("Data for 2018-present were obtained from the Chicago Data Portal. This data may include crashes which do not meet IDOT's reporting thresholds and exclude expressway crashes reported by the Illinois State Police."),
               h4("Disclaimer for 2009-2017 Data"),
               p("This tool uses crash report extracts obtained from the Illinois Department of Transportation (IDOT), which requires the following disclaimer to be used:"),
               em("DISCLAIMER: The motor vehicle crash data referenced herein was provided by the Illinois Department of Transportation. Any conclusions drawn from analysis of the aforementioned data are the sole responsibility of the data recipient(s).  Additionally, for coding years 2015 to present, the Bureau of Data Collection uses the exact latitude/longitude supplied by the investigating law enforcement agency to locate crashes. Therefore, location data may vary in previous years since data prior to 2015 was physically located by bureau personnel.")
               )
      )
  )
)

server <- function(input, output) {
  
  crashes_filtered <- eventReactive(input$update ,{
      crash_data() %>%
        filter(first_crash_type %in% input$crashtype,
               most_severe_injury %in% input$injtype,
               crash_hour >= as.numeric(input$hours[1]) & crash_hour <= as.numeric(input$hours[2]),
               year >= as.numeric(input$years[1]) & year <= as.numeric(input$years[2])
              )
    })
  
  crashes_displayed <- eventReactive(input$map_bounds,{ 
    # BUG for map_bounds: currently map needs to be nudged after hitting update button
    # is is possible to react to both input$update and input$map_bounds?
    bounds <- input$map_bounds
    crashes_filtered() %>%
      filter(latitude > bounds$south & latitude < bounds$north & longitude < bounds$east & longitude > bounds$west)
  })
  
  output$crashtable <- DT::renderDataTable({
    crashes_displayed() %>%
      select(crash_date,rd_no,first_crash_type,most_severe_injury,prim_contributory_cause,sec_contributory_cause)
    })
  
  output$crashperyear <- renderPlotly({
    year_summary <- crashes_displayed() %>%
      group_by(year,first_crash_type) %>%
      summarize(total = n())
    
    plot_ly(year_summary, x = ~year, y = ~total,  type="bar", name = ~first_crash_type)
  })
  
#  crashes_filtered <- eventReactive(input$update ,{
#    if(resolved(crash_data)) {
#      result = crash_data %...>%
#        filter(first_crash_type %in% input$crashtype,
#               year >= as.numeric(input$years[1]) & year <= as.numeric(input$years[2])
#               #TODO: also need bounding box for lat/long 
#              )
#    }else{
#      cat("Waiting for more data to load...")
#      result = dget("crash_sample.txt") %>%
#        filter(first_crash_type %in% input$crashtype,
#               year >= as.numeric(input$years[1]) & year <= as.numeric(input$years[2])
#               #TODO: also need bounding box for lat/long 
#        )
#    }
#    
#    return(result)
#  })

  ## Leaflet Outputs
  shapeFormat = drawShapeOptions(stroke = T, color = "#2980B9", weight = 4, dashArray = "3 5", fillColor = "#9AD5F7", fillOpacity = 0.5)
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      # addDrawToolbar(
      #   targetGroup = "draw",
      #   markerOptions = FALSE,
      #   circleMarkerOptions = FALSE,
      #   polygonOptions = drawPolygonOptions(showArea = F, shapeOptions = shapeFormat),
      #   polylineOptions = drawPolylineOptions(allowIntersection = F, showLength = T, metric = F, feet = T, nautic = F, shapeOptions = shapeFormat),
      #   rectangleOptions = drawRectangleOptions(shapeOptions = shapeFormat),
      #   circleOptions = drawCircleOptions(showRadius = T, metric = F, feet = T, nautic = F, shapeOptions = shapeFormat),
      #   editOptions = editToolbarOptions(),
      #   singleFeature = T
      # ) %>%
      setView(
        lng = -87.5,
        lat = 41.9,
        zoom = 11
      )
  })
  
  
  observe({
    leafletProxy("map", data = crashes_filtered()) %>%
      clearMarkers() %>%
      addCircleMarkers(
        #radius = ~rank_to_radius, # returns 1-9, 9 = worst
        color = ~ifelse(injuries_fatal > 0,"#f03b20","#feb24c"),
        stroke = FALSE,
        fillOpacity = ~ifelse(injuries_fatal > 0, 1, 0.6),
        #popup = ~paste0("<strong>",intersection," <span class='text-muted'>#",rank,"</span></strong><br>Crashes: ",crashes,"<br>Tot Injuries: ",injuries_total,"<br>Tot Fatalaties: ",injuries_fatal,"<br>Tot Incapacitating Injuries: ",injuries_incapacitating),
        popup = ~paste0(ifelse(!is.na(street_name),paste0("<strong>",street_no," ",street_direction," ",street_name,"</strong><br>"),""),"<strong>",format(crash_date,"%b %d, %Y %I %p"),"</strong> <br><strong>Crash Type:</strong> ",first_crash_type,"<br><strong>Most Severe Injury:</strong> ",most_severe_injury,"<br>Injuries:</strong> ",injuries_total,"<br>Fatalaties:</strong> ",injuries_fatal,"<br>Incapacitating Injuries:</strong> ",injuries_incapacitating,"<br><strong>Controls:</strong> ",traffic_control_device," ",device_condition,"<br><strong>Police report number:</strong> ",rd_no),
        group = "crashes",
        layerId = ~crash_record_id
      )
  })
  
  output$date_range = renderText({
    crash_dates = crash_data() %>% pull(crash_date)
    paste0("Showing up-to-date crashes with any injury (excludes property damage only) between ",min(as.Date(crash_dates))," and ",max(as.Date(crash_dates)),".")
  })
  
  output$dlCSV <- downloadHandler(
    filename = function() {
       paste('displayed-crashes-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) { # note file is a temp file provided by Shiny 
       write.csv(crashes_displayed(), file)
    }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)

