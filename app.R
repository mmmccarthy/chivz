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


# Run once onload
# Check for new crash data

# Update Crash summaries
search_string <- paste0("crashes_",format(Sys.time(), "%Y-%m-%d"),"_*")
files_list <- list.files("cache",search_string) # search for a cache file from today

if (length(files_list) == 0) {
  # Cache file is old
  last_update = "just now"
  cat("Updating crashes from Data Portal... \n")
  source("chicago_crashes/update.R", chdir = TRUE)
  cat("Update complete \n")
} else {
  cache_file_modified = file.mtime(paste0("cache/",files_list[1])) # file modified date/time
  cache_diff_time = difftime(Sys.time(), cache_file_modified)
  if(as.numeric(cache_diff_time, units = "hours") < 1) {
    last_update = "less than an hour ago"
  } else if (round(as.numeric(cache_diff_time, units = "hours"), digits = 0) == 1) {
    last_update = "1 hour ago"
  } else {
    last_update = paste0(round(as.numeric(cache_diff_time, units = "hours"), digits = 0)," hours ago")
  }
    
}

options(shiny.error = '')

ui <- navbarPage("Chicago Crash Data",
  id = "open_tab",
  tabPanel("Summarized Crashes 2009-Present",
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
                        choices = c("All","2020","2019","2018","2017","2016","2015","2014","2013","2012","2011","2010","2009")
                        # TODO get selection choices from summary file
                        # TODO range of years
                        # TODO bar graphs based displayed years
                        
            )
          ),
          h4("About"),
          p("Created by Michael McCarthy using public data from the City of Chicago and IDOT"),
          p("Built with R, Shiny, and Leaflet"),
          a("View the Code on GitHub",href="https://github.com/mmmccarthy/chivz"),
          hr(),
          h4("Crash Data for 2018 to present"),
          p("Data for 2018-present were obtained from the Chicago Data Portal. This data may include crashes which do not meet IDOT's reporting thresholds and exclude expressway crashes reported by the Illinois State Police."),
          em({ paste0("Updated ",last_update) }),
          h4("Disclaimer for 2009-2017 Data"),
          p("This tool uses crash report extracts obtained from the Illinois Department of Transportation (IDOT), which requires the following disclaimer to be used:"),
          em("DISCLAIMER: The motor vehicle crash data referenced herein was provided by the Illinois Department of Transportation. Any conclusions drawn from analysis of the aforementioned data are the sole responsibility of the data recipient(s).  Additionally, for coding years 2015 to present, the Bureau of Data Collection uses the exact latitude/longitude supplied by the investigating law enforcement agency to locate crashes. Therefore, location data may vary in previous years since data prior to 2015 was physically located by bureau personnel.")
       ),
       column(9,
              htmlOutput("clickdetails_header"),
              htmlOutput("clickdetails_footer"),
              tableOutput("clickdetails")
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
       ),
       column(9, 
              textOutput("clickdetails2_header",h4),
              tableOutput("clickdetails2")
       )
     )
  )
  # tabPanel("About",
  #  includeHTML("meta.html") # causes bug!
  # )
)

server <- function(input, output) {
  
## Leaflet Outputs 
  
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

## Data - use reactive to load when needed & cache
  
  # Geographic Data
  commareas_geo     = reactive({read_sf("geo/commareas.geojson")})
  wards_geo         = reactive({read_sf("geo/wards2015.geojson")})
  police_dist_geo   = reactive({read_sf("geo/police_districts.geojson")})
  intersections_geo = reactive({read_sf("geo/all_intersections.geojson")})
  
  # Polygon Crash Summaries
  commareas_crashes   = reactive({readRDS("crash_summaries/Summary_2009_present_Community_Areas.rds")})
  wards_crashes       = reactive({readRDS("crash_summaries/Summary_2009_present_Wards.rds")})
  police_dist_crashes = reactive({readRDS("crash_summaries/Summary_2009_present_PoliceDist.rds")})
  
  # Intersection Crash Data
  intersection_summary = reactive({readRDS("crash_summaries/Summary_2009_2019_Intersections.rds")})
  intersection_crashes = reactive({readRDS("crash_summaries/crashes_to_intersection.rds")})
  

## Polygon Map (first tab)
  
  polygons <- reactive({
    if (input$polygontype == "Community Areas") {
      label_pre = NULL
      geo = commareas_geo()
      hist_crashes = commareas_crashes()
      hist_crashes$name = hist_crashes$commarea
      joinflds = c("community" = "name")
    } else if (input$polygontype == "Wards") {
      label_pre = "Ward"
      geo = wards_geo()
      hist_crashes = wards_crashes()
      hist_crashes$name = hist_crashes$ward
      joinflds = c("ward" = "name")
    } else if (input$polygontype == "Police Districts") {
      label_pre = "District"
      geo = police_dist_geo()
      hist_crashes = police_dist_crashes()
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
      hist_crashes = commareas_crashes()
      hist_crashes = hist_crashes %>%
        mutate(name = commarea)
    } else if (input$polygontype == "Wards") {
      label_pre = "Ward"
      hist_crashes = wards_crashes()
      hist_crashes = hist_crashes %>%
        mutate(name = ward)
    } else if (input$polygontype == "Police Districts") {
      label_pre = "District"
      hist_crashes = police_dist_crashes()
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
      output$clickdetails_header = renderUI({ h4(paste0("Click on a ",label," for more details")) })
      output$clickdetails_footer = NULL
      output$clickdetails = NULL
    }
  })
  
  getDetailTable <- function(event){
    if(!is.null(event)) {
      output$clickdetails = renderTable(polygon_table(event$id))
      output$clickdetails_header = renderUI({ h4(paste0("Annual crash summary for ",str_to_title(event$id))) })
      if(event$group == "Wards" & (input$polygonyear == "All" | as.integer(input$polygonyear) < 2015)) {
        output$clickdetails_footer = renderText("Ward boundaries changed in 2015. These boundaries are used even for data from before 2015.")
      } else if (event$group == "Community Areas") {
        geo = commareas_geo()
        clicked_commarea = geo %>%
          filter(community == event$id)
        if (!is.na(clicked_commarea$vz_area)) {
          output$clickdetails_header= renderUI({ h4(paste0("Annual crash summary for ",str_to_title(event$id)), span(paste0(clicked_commarea$vz_area," High Crash Area"), class = "label label-danger")) })
          
          output$clickdetails_footer = NULL
          # output$clickdetails_footer = renderUI(HTML(paste0(strong(str_to_title(event$id))," is part of the ",strong(clicked_commarea$vz_area)," High Crash Area, as defined in the ",a(href="https://www.chicago.gov/city/en/depts/cdot/supp_info/vision-zero-chicago.html","Vision Zero Chicago")," Action Plan.")))
        } else {
          output$clickdetails_footer = NULL
        }
      } else {
        output$clickdetails_footer = NULL
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
        weight = 1.5,
        smoothFactor = 0.75,
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
    geo = intersections_geo()
    hist_crashes = intersection_summary()
    
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
    
    join = right_join(geo,hist_crashes,c("intxid"))
    
    join = join %>%
      # Serious Injury + Fatality Score = Incapacitating Injuries + 5x Fatalities
      mutate(serious_inj_fat = (injuries_incapacitating + (injuries_fatal * 5))) %>%
      filter(serious_inj_fat > 1)
    
    # Outliers have a score higher than 75th percentile of selection
    quartiles = quantile(join$serious_inj_fat, na.rm = TRUE)
    join = join %>%
      mutate(outlier = ifelse(serious_inj_fat >= quartiles['75%'],"yes","no")) %>%
      filter(outlier == "yes") %>%
      arrange(desc(serious_inj_fat)) %>%
      head(100) # return max 100 intersections
    
    join$rank = 1:100
    join$rank_to_radius = 2 + (111-join$rank) %/% 10
    
    return (join)
  })
  
  intersection_table <- function(itemid) {
    hist_crashes = intersection_crashes()
    
    hist_crashes = hist_crashes %>%
      st_drop_geometry() %>%
      mutate(crash_date = as.POSIXct(crash_date), year = format(crash_date,"%Y")) %>%
      filter((injuries_incapacitating + injuries_fatal) > 0)
      
    
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
    
    crash_list = hist_crashes %>%
      filter(intersection == itemid) %>%
      mutate(first_crash_type = str_to_title(first_crash_type), formatted_date = format(crash_date,"%a %b %d, %Y"), most_severe_injury = str_to_title(most_severe_injury), serious_inj_fat = paste0(injuries_fatal," / ",injuries_incapacitating)) %>%
      select(Intersection = intersection, "Crash Type" = first_crash_type, "Date" = formatted_date, "Most Severe Injury" = most_severe_injury, "# Fatal / Incapacitating Injuries" = serious_inj_fat, "Tot Injuries" = injuries_total, "Hit and Run (Y/N)" = hit_and_run_i, "Primary Contributing Cause" = prim_contributory_cause, "Secondary Contributing Cause" = sec_contributory_cause)
    
    return(crash_list)
  }
  
 # intxpal <- reactive({
 #   colorQuantile("YlOrRd",intersections()$serious_inj_fat)
 # })
  
  observe({
    req(input$open_tab == "Crashes by Intersection") # Check that the map's tab is open
  # pal = intxpal()
    leafletProxy("map_intersections", data = intersections()) %>%
      clearMarkers() %>%
      addCircleMarkers(
        radius = ~rank_to_radius, # returns 1-9, 9 = worst
        color = ~ifelse(injuries_fatal > 0,"#f03b20","#feb24c"),
        stroke = FALSE,
        fillOpacity = ~ifelse(injuries_fatal > 0, 1, 0.6),
        popup = ~paste0("<strong>",intersection," <span class='text-muted'>#",rank,"</span></strong><br>Crashes: ",crashes,"<br>Tot Injuries: ",injuries_total,"<br>Tot Fatalaties: ",injuries_fatal,"<br>Tot Incapacitating Injuries: ",injuries_incapacitating),
        group = "intxcrashes",
        layerId = ~label
      )
  })
  
  observe({
    event = input$map_intersections_marker_click
    if (!is.null(event) && event$group == "intxcrashes") {
      getIntxDetailTable(event)
    } else {
      # clear details table
      output$clickdetails2_header = renderText(paste0("Click on an intersection for more details"))
      #output$clickdetails2_footer = NULL
      output$clickdetails2 = NULL
    }
  })
  
  getIntxDetailTable <- function(event){
    if(!is.null(event)) {
      output$clickdetails2 = renderTable(intersection_table(event$id))
      output$clickdetails2_header = renderText(paste0("Fatal/Incapacitating Injury crash records for ",str_to_title(event$id)))
    }
  }
  
}

# Run the application 
shinyApp(ui = ui, server = server)

