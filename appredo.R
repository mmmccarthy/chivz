#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(leaflet.extras)


# USE .Renviron in this directory or Sys.setenv("APP_TOKEN" = "YOUR SOCRATA TOKEN")
readRenviron(file.path("./", ".Renviron"))

 options(shiny.error = '')
# for debugging
# options(shiny.error = browser)

ui <- navbarPage("Chicago Crash Data",
  tabPanel("IDOT Historical Data (2009-2017)",
     fluidRow(
      leafletOutput("map")
     ),
     fluidRow(
       column(3,
          h4("Options"),
          wellPanel(
            selectInput(inputId = "polygoncrashtype",
                        label = "Crash Type:",
                        choices = c("Pedestrian/Cyclist","All Crash Types")
            ),
            selectInput(inputId = "polygontype",
                        label = "Summarize to:",
                        choices = c("Community Areas", "Wards")
            ),
            selectInput(inputId = "polygonyear",
                        label = "Year:",
                        choices = c("All","2017","2016","2015","2014","2013","2012","2011","2010","2009")
            )
          ),
          h4("About"),
          p("Created by ",strong("Michael McCarthy")," using public data from the City of Chicago"),
          a("View the Code on GitHub",href="https://github.com/mmmccarthy/chivz"),
          hr(),
          h4("Disclaimer for 2009-2017 Data"),
          p("This tool uses crash report extracts obtained from the Illinois Department of Transportation (IDOT), which requires the following disclaimer to be used:"),
          em("DISCLAIMER: The motor vehicle crash data referenced herein was provided by the Illinois Department of Transportation. Any conclusions drawn from analysis of the aforementioned data are the sole responsibility of the data recipient(s).  Additionally, for coding years 2015 to present, the Bureau of Data Collection uses the exact latitude/longitude supplied by the investigating law enforcement agency to locate crashes. Therefore, location data may vary in previous years since data prior to 2015 was physically located by bureau personnel.")
       ),
       column(9, 
              textOutput("clickdetails_header",h4),
              tableOutput("clickdetails")
              )
    )
  ) #,
  # tabPanel("About",
  #  includeHTML("meta.html") # causes bug!
  # )
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
  
  polygons <- reactive({
    if (input$polygontype == "Community Areas") {
      label_pre = NULL
      geo = read_sf("geo/commareas.geojson")
      hist_crashes = readRDS("idot_crashes/IDOT_2009_2017_Summary_Community_Areas.rds")
      hist_crashes$name = hist_crashes$commarea
      joinflds = c("community" = "name")
    } else if (input$polygontype == "Wards") {
      label_pre = "Ward"
      geo = read_sf("geo/wards2015.geojson")
      hist_crashes = readRDS("idot_crashes/IDOT_2009_2017_Summary_Wards.rds")
      hist_crashes$name = hist_crashes$ward
      joinflds = c("ward" = "name")
    } else {
      statusmsg <- "Wards/Police Districts on hold"
      cat(statusmsg)
    }
    
      if (input$polygonyear != "All") {
        hist_crashes = hist_crashes %>%
          filter(year == input$polygonyear)
      }

      if (input$polygoncrashtype != "All Crash Types") {
        hist_crashes = hist_crashes %>%
          filter(ped_cyc == "yes")
      }
      
      hist_crashes = hist_crashes %>%
          group_by(name) %>%
          summarise(crashes = sum(crashes), injuries_total = sum(injuries_total), injuries_fatal = sum(injuries_fatal), injuries_incapacitating = sum(injuries_incapacitating), injuries_non_incapacitating = sum(injuries_non_incapacitating), injuries_reported_not_evident = sum(injuries_reported_not_evident), injrate = (injuries_incapacitating + injuries_fatal)/population *100000) %>%
          ungroup()
    
    hist_crashes$label = hist_crashes$name #fix name column issue
    
    join = left_join(geo,hist_crashes,joinflds)
    
    if (!is.null(label_pre)) {
      join = join %>%
        mutate(label = paste0(label_pre," ",label))
    }
    
    return (join)
    })
  
  polygon_table <- function(itemid) {
    if (input$polygontype == "Community Areas") {
      label_pre = NULL
      hist_crashes = readRDS("idot_crashes/IDOT_2009_2017_Summary_Community_Areas.rds")
      hist_crashes = hist_crashes %>%
        mutate(name = commarea)
    } else if (input$polygontype == "Wards") {
      label_pre = "Ward"
      hist_crashes = readRDS("idot_crashes/IDOT_2009_2017_Summary_Wards.rds")
      hist_crashes = hist_crashes %>%
        mutate(name = ward)
    } else {
      statusmsg <- "Police Districts on hold"
      cat(statusmsg)
    }
    
    if (input$polygonyear != "All") {
      hist_crashes = hist_crashes %>%
        filter(year == input$polygonyear)
    }
    
    if (input$polygoncrashtype != "All Crash Types") {
      hist_crashes = hist_crashes %>%
        filter(ped_cyc == "yes")
    }
    
    if (!is.null(itemid)) {
      
      
      
      if(!is.null(label_pre)){
        itemid = substr(itemid,nchar(label_pre) + 2,nchar(itemid))
      }
      
      hist_crashes = hist_crashes %>%
        filter(name == itemid) %>%
        ungroup()
      
      if (!is.null(label_pre)) {
        hist_crashes = hist_crashes %>%
          mutate(name = paste0(label_pre," ",str_to_title(name)))
      } else {
        hist_crashes = hist_crashes %>%
          mutate(name = str_to_title(name))
      }
      
      hist_crashes = hist_crashes %>%
        select(Boundary = name, "Pedestrian/Cyclist Crashes" = ped_cyc, "Year" = year, "Tot Fatalities" = injuries_fatal, "Incapacitating Injuries" = injuries_incapacitating, "Tot Injuries" = injuries_total, "Tot Crash Records" = crashes)
    }

    return (hist_crashes)
  }
  
  observe({
    event = input$map_shape_click
    if (!is.null(event)) {
      getDetailTable(event)
    } else {
      label = substr(input$polygontype,1,nchar(input$polygontype)-1)
      output$clickdetails_header = renderText(paste0("Click on a ",label," for more details"))
    }
  })
  
  getDetailTable <- function(event){
    if(!is.null(event)) {
      output$clickdetails = renderTable(polygon_table(event$id))
      output$clickdetails_header = renderText(paste0("Annual crash summary for ",str_to_title(event$id)))
    }
  }
  
  observe({
    leafletProxy("map", data = polygons()) %>%
      clearShapes() %>%
      addPolygons(
        #data = polygons(),
        color = "#444444",
        weight = 1,
        smoothFactor = 0.5,
        opacity = 0.8,
        fillOpacity = 0.5,
        fillColor = ~colorQuantile("YlOrRd",injrate)(injrate),
        highlightOptions = highlightOptions(
          color = "white",
          weight = 2,
          bringToFront = TRUE),
        label = ~str_to_title(label),
        group = input$polygontype,
        layerId = ~label
      ) # %>%
      # addLegend( # bug
      #   position = "topright",
      #   title = "Total Crashes", # "Fatal and Serious Injury Crashes per 100,000 people",
      #   pal = ~colorQuantile("YlOrRd", crashes),
      #   values = ~crashes,
      #   layerId = "crash_legend"
      # ) # %>%
      #addLayersControl(
      #  overlayGroups = c("Crashes",input$polygontype),
      #  options = layersControlOptions(collapsed = FALSE)
      # ) 
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

