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
# By Michael McCarthy, 9-4-2021
#
###############################################

library(shiny)
library(sf)
library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(stringr)
library(promises)
library(future)
plan(multisession)

options(shiny.error = '')
#options(shiny.error = recover)

# start loading crash data
#crash_data <- future(readRDS("./crash_summaries/Crashes_2009_present_IDOT_and_Chicago.rds"))
crash_data <- reactive({ readRDS("./crash_summaries/Crashes_2009_present_IDOT_and_Chicago.rds") })


# constants for now TODO
start_year = 2009
end_year = 2021

ui <- navbarPage("Chicago Crash Data",
  id = "open_tab",
  tabPanel("Selection Map",
     fluidRow(
       column(10,
        leafletOutput("map")
        ),
       column(2,
              h4("Options"),
              wellPanel(
                selectInput(inputId = "crashtype",
                            label = "Crash Type:",
                            choices = c("Pedestrian" = "PEDESTRIAN", "Pedalcyclist" = "PEDALCYCLIST", "Rear End" = "REAR END", "Sideswipe Same Direction" = "SIDESWIPE SAME DIRECTION", "Fixed Object" = "FIXED OBJECT", "Other Object" = "OTHER OBJECT", 
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
              )
            )
       ),
       fluidRow(
         column(12,
                DT::dataTableOutput("crashtable")
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
  
  output$crashtable <- DT::renderDataTable({
    bounds <- input$map_bounds
    crashes_filtered() %>% 
      filter(latitude > bounds$south & latitude < bounds$north & longitude < bounds$east & longitude > bounds$west) %>%
      mutate(pretty_date = format(crash_date,"%b %d, %Y")) %>%
      select(pretty_date,rd_no,first_crash_type,most_severe_injury,prim_contributory_cause,sec_contributory_cause)
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
      addDrawToolbar(
        targetGroup = "draw",
        markerOptions = FALSE,
        circleMarkerOptions = FALSE,
        polygonOptions = drawPolygonOptions(showArea = F, shapeOptions = shapeFormat),
        polylineOptions = drawPolylineOptions(allowIntersection = F, showLength = T, metric = F, feet = T, nautic = F, shapeOptions = shapeFormat),
        rectangleOptions = drawRectangleOptions(shapeOptions = shapeFormat),
        circleOptions = drawCircleOptions(showRadius = T, metric = F, feet = T, nautic = F, shapeOptions = shapeFormat),
        editOptions = editToolbarOptions(),
        singleFeature = T
      ) %>%
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
        popup = ~paste0("<strong>",street_no," ",street_direction," ",street_name,"</strong><br>","<strong>",format(crash_date,"%b %d, %Y %I %p"),"</strong> <br><strong>Crash Type:</strong> ",first_crash_type,"<br><strong>Most Severe Injury:</strong> ",most_severe_injury,"<br>Injuries:</strong> ",injuries_total,"<br>Fatalaties:</strong> ",injuries_fatal,"<br>Incapacitating Injuries:</strong> ",injuries_incapacitating,"<br><strong>Controls:</strong> ",traffic_control_device," ",device_condition,"<br><strong>Police report number:</strong> ",rd_no),
        group = "crashes",
        layerId = ~crash_record_id
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

