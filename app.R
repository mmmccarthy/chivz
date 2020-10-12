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
library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(stringr)


# USE .Renviron in this directory or Sys.setenv("APP_TOKEN" = "YOUR SOCRATA TOKEN")
readRenviron(file.path("./", ".Renviron"))

options(shiny.error = '')

ui <- navbarPage("Chicago Crash Data",
  id = "open_tab",
  tabPanel("Summarized Crashes (2009-2019)",
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
                        choices = c("Community Areas", "Wards", "Police Districts")
            ),
            selectInput(inputId = "polygonyear",
                        label = "Year:",
                        choices = c("All","2019","2018","2017","2016","2015","2014","2013","2012","2011","2010","2009")
            )
          ),
          h4("About"),
          p("Created by ",strong("Michael McCarthy")," using public data from the City of Chicago"),
          a("View the Code on GitHub",href="https://github.com/mmmccarthy/chivz"),
          hr(),
          h4("2018 and 2019 Data"),
          p("Data for 2018-2019 were obtained from the Chicago Data Portal. This data may include crashes which do not meet IDOT's reporting thresholds and exclude expressway crashes reported by the Illinois State Police."),
          h4("Disclaimer for 2009-2017 Data"),
          p("This tool uses crash report extracts obtained from the Illinois Department of Transportation (IDOT), which requires the following disclaimer to be used:"),
          em("DISCLAIMER: The motor vehicle crash data referenced herein was provided by the Illinois Department of Transportation. Any conclusions drawn from analysis of the aforementioned data are the sole responsibility of the data recipient(s).  Additionally, for coding years 2015 to present, the Bureau of Data Collection uses the exact latitude/longitude supplied by the investigating law enforcement agency to locate crashes. Therefore, location data may vary in previous years since data prior to 2015 was physically located by bureau personnel.")
       ),
       column(9, 
              textOutput("clickdetails_header",h4),
              tableOutput("clickdetails"),
              textOutput("clickdetails_footer",p)
              )
    )
  ),
  tabPanel("Crashes by Intersection",
     fluidRow(
       leafletOutput("map_intersections")
     ),
     fluidRow(
       column(3,
              h4("Options"),
              wellPanel(
                selectInput(inputId = "intxcrashtype",
                            label = "Crash Type:",
                            choices = c("Ped & Cyclist","Pedestrian","Cyclist")
                ),
                selectInput(inputId = "intxyear",
                            label = "Year:",
                            choices = c("All","2019","2018","2017","2016","2015","2014","2013","2012","2011","2010","2009")
                )
              )
       )
     )
  )
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
  
  output$map_intersections <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron, options = providerTileOptions(
        updateWhenZooming = FALSE,
        updateWhenIdle = TRUE
      )) %>%
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
      hist_crashes = readRDS("crash_summaries/Summary_2009_2019_Community_Areas.rds")
      hist_crashes$name = hist_crashes$commarea
      joinflds = c("community" = "name")
    } else if (input$polygontype == "Wards") {
      label_pre = "Ward"
      geo = read_sf("geo/wards2015.geojson")
      hist_crashes = readRDS("crash_summaries/Summary_2009_2019_Wards.rds")
      hist_crashes$name = hist_crashes$ward
      joinflds = c("ward" = "name")
    } else if (input$polygontype == "Police Districts") {
      label_pre = "District"
      geo = read_sf("geo/police_districts.geojson")
      hist_crashes = readRDS("crash_summaries/Summary_2009_2019_PoliceDist.rds")
      hist_crashes$name = hist_crashes$police_dist
      joinflds = c("dist_num" = "name")
    }
    # TODO add Census Tracts -> other demographics
    
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
          summarise(crashes = sum(crashes), injuries_total = sum(injuries_total), injuries_fatal = sum(injuries_fatal), 
                    injuries_incapacitating = sum(injuries_incapacitating), injuries_non_incapacitating = sum(injuries_non_incapacitating),
                    injuries_reported_not_evident = sum(injuries_reported_not_evident)) %>%
          ungroup()

    hist_crashes$label = hist_crashes$name #fix name column issue
    
    join = left_join(geo,hist_crashes,joinflds)
    
    if (!is.null(label_pre)) {
      join = join %>%
        mutate(label = paste0(label_pre," ",label))
    }
    
    if(!is.null(join$pop10)) {
      # 2010 Census Population
      join = join %>%
        mutate(injrate = (injuries_incapacitating + injuries_fatal)/pop10*100000)
    } else {
      # Calculate area in square miles
      join_projected = st_transform(join, crs = 3435) # Project into IL State Plane
      join$areaft = st_area(join_projected)
      join$areasqmi = (join$areaft / 5280)

      join = join %>%
        mutate(injrate = (injuries_incapacitating + injuries_fatal)/areasqmi*1000)
    }
    
    return (join)
    })
  
  polygon_table <- function(itemid) {
    if (input$polygontype == "Community Areas") {
      label_pre = NULL
      hist_crashes = readRDS("crash_summaries/Summary_2009_2019_Community_Areas.rds")
      hist_crashes = hist_crashes %>%
        mutate(name = commarea)
    } else if (input$polygontype == "Wards") {
      label_pre = "Ward"
      hist_crashes = readRDS("crash_summaries/Summary_2009_2019_Wards.rds")
      hist_crashes = hist_crashes %>%
        mutate(name = ward)
    } else if (input$polygontype == "Police Districts") {
      label_pre = "District"
      hist_crashes = readRDS("crash_summaries/Summary_2009_2019_PoliceDist.rds")
      hist_crashes = hist_crashes %>%
        mutate(name = police_dist)
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
    polygontype = input$polygontype
    if (!is.null(event) && event$group == polygontype) {
      getDetailTable(event)
    } else {
      # clear details table
      label = substr(polygontype,1,nchar(polygontype)-1) # remove plural form
      output$clickdetails_header = renderText(paste0("Click on a ",label," for more details"))
      output$clickdetails_footer = NULL
      output$clickdetails = NULL
    }
  })
  
  getDetailTable <- function(event){
    if(!is.null(event)) {
      output$clickdetails = renderTable(polygon_table(event$id))
      output$clickdetails_header = renderText(paste0("Annual crash summary for ",str_to_title(event$id)))
      if(event$group == "Wards" & (input$polygonyear == "All" | as.integer(input$polygonyear) < 2015)) {
        output$clickdetails_footer = renderText("Ward boundaries changed in 2015. These boundaries are used even for data from before 2015.")
      } else {
        output$clickdetails_footer = renderText("")
      }
    }
  }
  
  legend_title <- reactive({
    if (!is.null(polygons()$pop10)) {
      "Fatal and Serious Injury Crashes per 100,000 people" 
    } else{
      "Fatal and Serious Injury Crashes per 1,000 sq. miles"
    }
  })
  
  injratepal <- reactive({
    colorQuantile("YlOrRd",polygons()$injrate)
  })
  
  observe({
    pal = injratepal()
    leafletProxy("map", data = polygons()) %>%
      clearShapes() %>%
      addPolygons(
        #data = polygons(),
        color = "#444444",
        weight = 1,
        smoothFactor = 0.5,
        opacity = 0.8,
        fillOpacity = 0.5,
        fillColor = ~pal(injrate),
        highlightOptions = highlightOptions(
          color = "white",
          weight = 2,
          bringToFront = TRUE),
        label = ~str_to_title(label),
        group = input$polygontype,
        layerId = ~label
      )  %>%
       addLegend(
         position = "topright",
         title = legend_title(),
         pal = pal,
         values = ~injrate,
         labFormat = function(type, cuts, p) {
           n = length(cuts)
           p = paste0(round(p * 100), '%')
           cuts = paste0(formatC(cuts[-n]), " - ", formatC(cuts[-1]))
           # mouse over the legend labels to see the percentile ranges
           paste0(
             '<span title="', p[-n], " - ", p[-1], '">', cuts,
             '</span>'
           )
         },
         layerId = "crash_legend"
       ) # %>%
      #addLayersControl(
      #  overlayGroups = c("Crashes",input$polygontype),
      #  options = layersControlOptions(collapsed = FALSE)
      # ) 
  })
  
  
  #######################################
  # Crashes by Intersection
  #######################################
  
  intersections <- reactive({
    geo = read_sf("geo/all_intersections.geojson")
    hist_crashes = readRDS("crash_summaries/Summary_2009_2019_Intersections.rds")
    
    if (input$intxyear != "All") {
      hist_crashes = hist_crashes %>%
        filter(year == input$intxyear)
    }
    
    if (input$intxcrashtype == "Pedestrian") {
      hist_crashes = hist_crashes %>%
        filter(first_crash_type == "PEDESTRIAN")
    } else if (input$intxcrashtype == "Cyclist") {
      hist_crashes = hist_crashes %>%
        filter(first_crash_type == "PEDESTRIAN")
    } 
    
    hist_crashes = hist_crashes %>%
      group_by(intxid, intersection) %>%
      summarise(crashes = sum(crashes), injuries_total = sum(injuries_total), injuries_fatal = sum(injuries_fatal), 
                injuries_incapacitating = sum(injuries_incapacitating), injuries_non_incapacitating = sum(injuries_non_incapacitating),
                injuries_reported_not_evident = sum(injuries_reported_not_evident)) %>%
      ungroup()
    
    hist_crashes$label = hist_crashes$intersection #fix name column issue
    
    join = left_join(geo,hist_crashes,c("intxid"))
    
    join = join %>%
      mutate(serious_inj_fat = (injuries_incapacitating + injuries_fatal)) %>%
      filter(serious_inj_fat >= 2) %>% # 75th percentile
      head(50)

    return (join)

  })
  
 # intxpal <- reactive({
 #   colorQuantile("YlOrRd",intersections()$serious_inj_fat)
 # })
  
  observe({
    req(input$open_tab == "Crashes by Intersection")
  # pal = intxpal()
    leafletProxy("map_intersections", data = intersections()) %>%
      addCircleMarkers()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

