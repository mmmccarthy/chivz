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
# Last update: 2022-08-03
#
###############################################

library(shiny)
library(sf)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(RSocrata)
library(plotly)
library(tidyr)
library(stringr)
library(jsonlite) # debugging
# library(tidyverse)
# library(promises)
# library(future)
# plan(multisession)

options(shiny.error = '')
# for debug
# options(shiny.fullstacktrace=TRUE)

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
end_year = as.integer(format(Sys.time(), "%Y")) # 2022

ui <- navbarPage("Chicago Crash Data",
  id = "open_tab",
  tabPanel("Map",
     fluidRow(
       column(9,
        tags$style(type = "text/css", "#map {height: 80vh !important;}"),
        leafletOutput("map"),
        # htmlOutput("dataDesc"),
        ),
       column(3,
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
                radioButtons("usegeobounds", "Geographic Filtering",
                             choices = list("Map Extent" = 1, "Drawn Selection Box" = 2), selected = 1),
                helpText("If no selection box is drawn, the second option will show all crashes citywide."),
                
                actionButton("update", "Update Map")
                
              ),
              p("Click Update Map to populate the map with crashes matching your selections."),
              
            )
       ),
       fluidRow(
         column(8,
                h3("Crashes by Year and Type"),
                plotlyOutput("crashperyear")
                ),
         column(4,
                # h3("Crash Statistics"),
                h3(textOutput("crashstatstext")),
                tableOutput("crashstats"),
                h3("Downloads"),
                span(downloadLink("dlCSV","Download CSV File")),
                br(),
                span(downloadLink("dlGeo","Download GeoJSON File"))
         ),
         column(12,
                h3("Crash Details"),
                DT::dataTableOutput("crashtable")
                )
      )
  ),
  tabPanel("About",
      fluidRow(
        column(12,
               h2("About the Data"),
               p("To reduce storage and memory needs, this map only displays crashes where there was some indication of any injury. Crashes with any fatality or reported injury, whether evident to first responders or not, are included."),
               h3("Crash Data for 2018 to present"),
               p("Data for 2018-present were obtained from the Chicago Data Portal. This data may include crashes which do not meet IDOT's reporting thresholds and exclude expressway crashes reported by the Illinois State Police."),
               h4("Additional Data"),
               p("The Chicago Data Portal also has data on the persons and vehicles involved in crashes. Each person or vehicle record is associated with a crash record."),
               h3("Disclaimer for 2009-2017 Data"),
               p("This tool uses crash report extracts obtained from the Illinois Department of Transportation (IDOT), which requires the following disclaimer to be used:"),
               em("DISCLAIMER: The motor vehicle crash data referenced herein was provided by the Illinois Department of Transportation. Any conclusions drawn from analysis of the aforementioned data are the sole responsibility of the data recipient(s).  Additionally, for coding years 2015 to present, the Bureau of Data Collection uses the exact latitude/longitude supplied by the investigating law enforcement agency to locate crashes. Therefore, location data may vary in previous years since data prior to 2015 was physically located by bureau personnel."),
               h2("About this App"),
               p("Created by Michael McCarthy using public data from the City of Chicago and IDOT"),
               p("Built with R, Shiny, and Leaflet"),
               a("View the Code on GitHub",href="https://github.com/mmmccarthy/chivz")
               )
      )
  )
)

server <- function(input, output) {
  
  crashes_filtered <- eventReactive(input$update,{
    if(input$usegeobounds == 1){
      # map extent
      geo_bounds = input$map_bounds
    } else if(input$usegeobounds == 2) {
      # user-drawn bounding box
      geo_bounds = getDrawnRectangle(input$map_draw_new_feature)
    }
    
    getCrashes(crash_data(),
               yearFrom = as.numeric(input$years[1]),
               yearTo = as.numeric(input$years[2]),
               hourFrom = as.numeric(input$hours[1]),
               hourTo = as.numeric(input$hours[2]),
               boundingBox = geo_bounds, 
               crashTypes = input$crashtype,
               injTypes = input$injtype
               
    )
    
  })

  output$crashtable <- DT::renderDataTable({
    crashes_filtered() %>%
      select(crash_date,rd_no,first_crash_type,most_severe_injury,prim_contributory_cause,sec_contributory_cause)
    })
  
  output$crashstatstext <- renderText({
    crash_total <- crashes_filtered() %>% nrow()
    min_date <- crashes_filtered() %>% summarize(min(crash_date)) %>% pull(`min(crash_date)`) %>% format("%B %d, %Y")
    max_date <- crashes_filtered() %>% summarize(max(crash_date)) %>% pull(`max(crash_date)`) %>% format("%B %d, %Y")
    
    paste0("Showing ",crash_total," crashes occurring between ",min_date," and ",max_date)
  })
  
  output$crashstats <- renderTable({
    crashes_filtered() %>%
      mutate(first_crash_type = str_to_title(first_crash_type),
             most_severe_injury = str_to_title(most_severe_injury) ) %>%
      group_by(first_crash_type,most_severe_injury) %>%
      summarize(Count = n()) %>%
      pivot_wider(names_from = most_severe_injury, values_from = Count, values_fill = list(Count = 0)) %>%
      rename(`Crash Type` = first_crash_type)
  })
  
  # crashstats <- list(total = crashes_filtered() %>% nrow(),
  #                           by_year = crashes_filtered() %>% with(table(crash_year)),
  #                           by_type = crashes_filtered() %>% with(table(first_crash_type)),
  #                           by_inj = crashes_filtered() %>% with(table(most_severe_injury))
  #                             )
  
  output$crashperyear <- renderPlotly({
    year_summary <- crashes_filtered() %>%
      group_by(year,first_crash_type) %>%
      summarize(total = n())
    
    plot_ly(year_summary, x = ~year, y = ~total,  type="bar", name = ~first_crash_type) %>% layout(barmode = "stack")
  })
  

  ## Leaflet Outputs
  shapeFormat = drawShapeOptions(stroke = T, color = "#2980B9", weight = 4, dashArray = "3 5", fillColor = "#9AD5F7", fillOpacity = 0.5)
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addDrawToolbar(
        targetGroup = "draw",
        markerOptions = FALSE,
        circleMarkerOptions = FALSE,
        polygonOptions = FALSE, # drawPolygonOptions(showArea = F, shapeOptions = shapeFormat),
        polylineOptions = FALSE,
        rectangleOptions = drawRectangleOptions(shapeOptions = shapeFormat),
        circleOptions = FALSE,
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
        popup = ~paste0(ifelse(!is.na(street_name),paste0("<strong>",street_no," ",street_direction," ",street_name,"</strong><br>"),""),"<strong>",format(crash_date,"%b %d, %Y %I %p"),"</strong> <br><strong>Crash Type:</strong> ",first_crash_type,"<br><strong>Most Severe Injury:</strong> ",most_severe_injury,"<br>Injuries:</strong> ",injuries_total,"<br>Fatalaties:</strong> ",injuries_fatal,"<br>Incapacitating Injuries:</strong> ",injuries_incapacitating,"<br><strong>Controls:</strong> ",traffic_control_device," ",device_condition,"<br><strong>Police report number:</strong> ",rd_no),
        group = "crashes",
        layerId = ~crash_record_id
      )
  })
  
#  output$date_range = renderText({
#    crash_dates = crash_data() %>% pull(crash_date)
#    paste0("Showing up-to-date crashes with any injury (excludes property damage only) between ",min(as.Date(crash_dates))," and ",max(as.Date(crash_dates)),".")
#  })
  
  output$dlCSV <- downloadHandler(
    filename = function() {
       paste('displayed-crashes-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) { # note file is a temp file provided by Shiny 
       write.csv(crashes_filtered(), file)
    }
    )
  
  output$dlGeo <- downloadHandler(
    filename = function() {
      paste('displayed-crashes-', Sys.Date(), '.geojson', sep='')
    },
    content = function(file) { # note file is a temp file provided by Shiny 
      crashes_geo_temp <- st_as_sf(crashes_filtered(), coords = c("longitude","latitude"))
      crashes_geo_temp <- st_set_crs(crashes_geo_temp, 4326)
      st_write(crashes_geo_temp, driver = "GeoJSON", file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

