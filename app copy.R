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
library(geojsonio)
library(RSocrata)
library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(RColorBrewer)
library(DT)
library(ggplot2)
library(shinycssloaders)
library(rmarkdown)

# USE .Renviron in this directory or Sys.setenv("APP_TOKEN" = "YOUR SOCRATA TOKEN")
readRenviron(file.path("./", ".Renviron"))

options(shiny.error = '')

ui <- navbarPage("Chicago Crash Data",
  # titlePanel("Chicago Crash Data"),
  #tabPanel("Main",
  #     # Main Map
  #     fluidRow(
  #       leafletOutput("map")
  #     )
  #),
  tabPanel("Main",
     # Main Map
     fluidRow(
      leafletOutput("map")
     ),
     fluidRow(
       column(3,
              h3("About"),
                a("View the Code on GitHub",href="https://github.com/mmmccarthy/chivz")
       ),
       column(3,
          h4("Options"),
          wellPanel(
            selectInput(inputId = "crashtype",
                        label = "Choose a crash type:",
                        choices = c("Pedestrian" = "PEDESTRIAN", "Cyclist" = "PEDALCYCLIST")
                        ),
            dateRangeInput("crashdate", "Date range:",
                           start = "2017-09-01"),
            
          span(class="text-muted","All police districts since September 2017. Earliest data since 2015.")
          )
        ) #,
        # column(6,
        #   h4("Crashes Summarized by Most Severe Injury"),
        #   h5("Only counts crashes currently displayed on the map"),
        #   downloadButton("downloadVisCrashes", textOutput("downloadLabel",inline=TRUE)),
        #   tableOutput("crashsummary") %>% withSpinner()
        # )
    ) #,
    # fluidRow(
    #   column(6,
    #         h4("Summary by Community Area"),
    #         h5("All crashes in date range and displayed on map"),
    #         tableOutput("commsummary")
    #   ),
    #   column(6,
    #          h4("Summary by Ward"),
    #          h5("All crashes in date range and displayed on map"),
    #          tableOutput("wardsummary")
    #   )
    # )
  ),
  tabPanel("About",
    includeMarkdown("meta.Rmd")
  )
)

server <- function(input, output) {
  
  # Get Data
  # Bike Routes from Chicago Data Portal (Updated Feb 2020)
    bikeroutes <- read_sf("geo/bikeroutes.geojson")
    
    # Add boundary layers
    commareas <- read_sf("geo/commareas.geojson")
    wards <- read_sf("geo/wards2015.geojson")
    
  # Get Data Portal Data for Ped/Cycle crashes since 2017-01-01 (if no cache available)
    update_crashes <- function(){
      statusmsg <- "Updating crashes from data portal"
      cat(statusmsg)
      
      crashes_cached <- paste0("./cache/crashes_",format(Sys.time(), "%Y-%m-%d_%H%M%S"),".rds")
      url <-   "https://data.cityofchicago.org/resource/85ca-t3if.json?$"
      start_date <- '2017-01-01' #format(input$crashdate[1])
      end_date <- '2020-07-17' #format(input$crashdate[2]) # TODO update to current day
      date <- paste0("crash_date between ","'",start_date,"'"," and ","'",end_date,"'")
      type <- paste0("first_crash_type == 'PEDESTRIAN' OR first_crash_type == 'PEDALCYCLIST'") #,input$crashtype,"'")
      query <- paste0(url,"where=",date," AND ",type)
      crashes <- read.socrata(query, app_token = Sys.getenv("APP_TOKEN"))
      
      # Fix Lat/Long for use with Leaflet
      crashes$latitude = as.numeric(crashes$latitude)
      crashes$longitude = as.numeric(crashes$longitude)
      
      # Eliminate non-Chicago or missing lat/long points
      crashes <- crashes %>%
        select(-c(location.coordinates)) %>%
        filter(latitude > 35 & longitude < -85)
      
      # Reorder injury severity levels
      crashes$most_severe_injury <- factor(as.character(crashes$most_severe_injury), 
                                           levels = c("FATAL","INCAPACITATING INJURY","NONINCAPACITATING INJURY","REPORTED, NOT EVIDENT","NO INDICATION OF INJURY")) 
      
      statusmsg <- "Spatial joining crashes"
      cat(statusmsg)
      
      # Make Crashes sf
      crashes <- st_as_sf(crashes, coords = c("longitude","latitude"))
      crashes <- st_set_crs(crashes, 4326)
      
      # Intersect crashes with Community Areas and Wards
      crash_in_ca <- st_join(crashes,commareas, join = st_within)
      crash_in_ward <- st_join(crashes,wards,join = st_within)
      crashes$commarea <- crash_in_ca$community
      crashes$ward <- crash_in_ward$ward
      
      commarea_crashes <- crashes %>%
        as.data.frame %>% 
        select(-c(geometry)) %>% # remove geom (points of crashes)
        count(commarea,first_crash_type,most_severe_injury) %>%
        spread(most_severe_injury,n,fill=0)

      #write.csv(crashes,crashes_cached)
      crashes <- list("points" = crashes, "ca" = commarea_crashes)
      saveRDS(crashes,crashes_cached)
      return(crashes)
    }
    
    # Get cached crashes or update cache if old
    search_string <- paste0("crashes_",format(Sys.time(), "%Y-%m-%d"),"_*")
    files_list <- list.files("cache",search_string) # search for a cache file from today
    
    if (length(files_list) > 0) {
      #crashes <- read.csv(paste0("cache/",max(files_list))) 
      # get latest cached version
      crashes <- readRDS(paste0("cache/",max(files_list)))
    } else {
      # no cached files from today
      # TODO delete old caches
      crashes <- update_crashes()
    }
    
    # Filter Crashes for map
   # crash_data <- reactive({
   #   crashes$points %>%
   #     mutate(crash_date = as.POSIXct(crash_date)) %>%
   #     filter(first_crash_type == input$crashtype, crash_date >= format(input$crashdate[1]) & crash_date <= format(input$crashdate[2]))
   # })

    crash_ca <- reactive({
      crashes$ca %>%
        filter(first_crash_type == input$crashtype) %>%
        merge(commareas, by.x="commarea", by.y="community")
    })
    
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
          ) #%>%
        #  addPolylines(
        #    data = bikeroutes,
        #    group = "Existing Bike Routes",
        #    color = ~pal(displayrou),
        #    opacity = 0.5,
        #    weight = 3,
        #    popup = ~paste0("<strong>",str_to_title(street),"</strong><br>",str_to_title(displayrou))
        #  ) %>%
           # %>%
        #  addDrawToolbar(
        #    targetGroup = "draw",
        #    editOptions = editToolbarOptions(
        #      selectedPathOptions = selectedPathOptions()
        #    )
        #  )  %>%
        #  addLayersControl(
        #    overlayGroups = c("draw"),
        #    options = layersControlOptions(collapsed = FALSE)
        #  ) %>%
        #  addStyleEditor()
    })
    
  # Update Crashes based on inputs

  #  crash_data <- reactive({
  #    url <-   "https://data.cityofchicago.org/resource/85ca-t3if.json?$"
  #    start_date <- format(input$crashdate[1])
  #    end_date <- format(input$crashdate[2])
  #    date <- paste0("crash_date between ","'",start_date,"'"," and ","'",end_date,"'")
  #    type <- paste0("first_crash_type == '",input$crashtype,"'")
  #    query <- paste0(url,"where=",date," AND ",type)
  #    crashes <- read.socrata(query, app_token = Sys.getenv("APP_TOKEN"))
  #    
  #    # Fix Lat/Long for use with Leaflet
  #    crashes$latitude = as.numeric(crashes$latitude)
  #    crashes$longitude = as.numeric(crashes$longitude)
  #    
  #    # Eliminate non-Chicago or missing lat/long points
  #    crashes <- crashes %>%
  #      select(-c(location.coordinates)) %>%
  #      filter(latitude > 35 & longitude < -85)
  #    return(crashes)
  #  })
  
    # observe({
    #   leafletProxy("map", data = crash_data()) %>%
    #     addCircleMarkers(
    #       data = crash_data(),
    #       radius = ~ifelse(most_severe_injury == "FATAL", 10, 4),
    #       stroke = FALSE,
    #       fillColor = ~pal2(most_severe_injury),
    #       fillOpacity = 2,
    #       popup = ~paste0("<strong>",street_no," ",street_direction," ",str_to_title(street_name),"</strong><br>Date: ",format(crash_date, format = "%b %d, %Y"),"<br>Type: ",str_to_title(first_crash_type),"<br>Most severe injury: ",str_to_title(most_severe_injury),"<br>Community: ",str_to_title(commarea),"<br>Ward: ",str_to_title(ward))
    #     ) %>%
    #     addLegend(
    #       position = "topright",
    #       title = "Crash Injury Types",
    #       pal = pal2,
    #       values = ~most_severe_injury,
    #       labFormat = labelFormat(transform = function(x) str_to_title(x))
    #     )
    # })
    
    observe({
      polygons <- crash_ca()
      names(polygons$geometry) <- NULL
      
      pal = ~colorQuantile(palette = "YlOrRd", domain = polygons$FATAL)
      
      leafletProxy("map") %>%
        addPolygons(data = polygons$geometry,
                    fillColor = ~pal(FATAL),
                    opacity = 1,
                    color = "white",
                    fillOpacity = 0.6,
                    popup = ~paste0("Community: ",str_to_title(community))
                )
       # addCircleMarkers(
       #   data = crash_data(),
       #   radius = ~ifelse(most_severe_injury == "FATAL", 10, 4),
       #   stroke = FALSE,
       #   fillColor = ~pal2(most_severe_injury),
       #   fillOpacity = 2,
       #   popup = ~paste0("<strong>",street_no," ",street_direction," ",str_to_title(street_name),"</strong><br>Date: ",format(crash_date, format = "%b %d, %Y"),"<br>Type: ",str_to_title(first_crash_type),"<br>Most severe injury: ",str_to_title(most_severe_injury),"<br>Community: ",str_to_title(commarea),"<br>Ward: ",str_to_title(ward))
       # ) %>%
       # addLegend(
       #   position = "topright",
       #   title = "Crash Injury Types",
       #   pal = pal2,
       #   values = ~most_severe_injury,
       #   labFormat = labelFormat(transform = function(x) str_to_title(x))
       # )
    })
    
    
    # visible_crashes <- reactive({
    #   if (!is.null(input$map_bounds)) {
    #     map_bbox <- st_as_sfc(st_bbox(c(xmin=input$map_bounds$west,xmax=input$map_bounds$east,ymax=input$map_bounds$north,ymin=input$map_bounds$south), crs = st_crs(4326)))
    #     crash_df <- crash_data()
    #     crash_df$visible <- st_within(crash_df, map_bbox) %>% lengths > 0
    #     crash_df <- st_set_geometry(crash_df,NULL) # remove geometry
    #     
    #     visible <- crash_df %>%
    #       filter(visible == TRUE)
    #       #filter(latitude <= input$map_bounds$north & latitude > input$map_bounds$south & longitude < input$map_bounds$east & longitude >= input$map_bounds$west)
    #   return(visible)
    #   }else{
    #     return(NULL)
    #   }
    # })

  # output$crashsummary <- renderTable(
  #   if (!is.null(visible_crashes())) {
  #   visible_crashes() %>%
  #         mutate(year = format(crash_date,"%Y"), most_severe_injury = str_to_title(most_severe_injury)) %>%
  #         group_by(most_severe_injury,year) %>%
  #         summarize(total = n())%>%
  #         spread(year,total,fill=NA,convert=FALSE) %>%
  #         arrange(most_severe_injury) %>%
  #         rename("Most Severe Injury" = most_severe_injury)
  #      }
  #     )
  # 
  # output$commsummary <- renderTable(
  #   if (!is.null(visible_crashes())) {
  #     visible_crashes() %>%
  #       # mutate(year = format(crash_date,"%Y")) %>%
  #       mutate(commarea = str_to_title(commarea), most_severe_injury = str_to_title(most_severe_injury)) %>%
  #       group_by(commarea,most_severe_injury) %>%
  #       summarize(total = n())%>%
  #       spread(most_severe_injury,total,fill=NA,convert=FALSE) %>%
  #       select(commarea,str_to_title(levels(visible_crashes()$most_severe_injury))) %>% # BUG unknown columns error when < n crash types visible on map
  #       rename("Community Area" = commarea)
  #   }
  # )
  # 
  # output$wardsummary <- renderTable(
  #   if (!is.null(visible_crashes())) {
  #     #severity_order <- str_to_title(levels(visible_crashes()$most_severe_injury))
  #     visible_crashes() %>%
  #       # mutate(year = format(crash_date,"%Y")) %>%
  #       mutate(most_severe_injury = str_to_title(most_severe_injury)) %>%
  #       group_by(ward,most_severe_injury) %>%
  #       summarize(total = n())%>%
  #       spread(most_severe_injury,total,fill=NA,convert=FALSE) %>%
  #       arrange(as.integer(ward)) %>%
  #       select("Ward" = ward,str_to_title(levels(visible_crashes()$most_severe_injury))) # BUG unknown columns error when < n crash types visible on map
  #   }
  # )

  # output$downloadLabel <- renderText(paste0("Download ",nrow(visible_crashes())," rows"))
  # 
  # output$downloadVisCrashes <- downloadHandler(
  #   filename = function() {
  #     paste("crash_extract.csv", sep = "") #,format(Sys.time(), "%m-%d-%Y-%h%m%s"), 
  #   },
  #   content = function(file) {
  #     write.csv(visible_crashes(), file, row.names = FALSE)
  #   }
  # )
}

# Run the application 
shinyApp(ui = ui, server = server)

