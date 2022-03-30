###########################################################
#                                                         #
# functions.R                                             #
#   Define common functions for Chicago VZ Dashboard      #
#                                                         #
###########################################################

updateCrashes <- function() {
  # Data Portal API Key
  # Use .Renviron in this directory or Sys.setenv("APP_TOKEN" = "YOUR SOCRATA TOKEN")
  readRenviron(".Renviron")
  
  # Download new Chicago Data Portal crashes
  url <-   "https://data.cityofchicago.org/resource/85ca-t3if.json?$"
  start_date <- '2022-01-01T00:00:00'
  end_date <- Sys.Date()
  date_query <- paste0("crash_date between ","'",start_date,"'"," and ","'",end_date,"'")
  injury_query <- paste0("most_severe_injury != 'NO INDICATION OF INJURY'") # removes ~85% of crashes (property damage only)
  query <- paste0(url,"where=",date_query," AND ",injury_query)
  crashes <- read.socrata(query, app_token = Sys.getenv("APP_TOKEN"))
  
  # Expected columns
  expected_cols <- c("crash_record_id", "rd_no", "crash_date", "posted_speed_limit", 
                     "traffic_control_device", "device_condition", "weather_condition", 
                     "lighting_condition", "first_crash_type", "trafficway_type", 
                     "lane_cnt", "alignment", "roadway_surface_cond", "road_defect", 
                     "report_type", "crash_type", "hit_and_run_i", "damage", "date_police_notified", 
                     "prim_contributory_cause", "sec_contributory_cause", "street_no", 
                     "street_direction", "street_name", "beat_of_occurrence", "num_units", 
                     "most_severe_injury", "injuries_total", "injuries_fatal", "injuries_incapacitating", 
                     "injuries_non_incapacitating", "injuries_reported_not_evident", 
                     "injuries_no_indication", "injuries_unknown", "crash_hour", "crash_day_of_week", 
                     "crash_month", "location.type", "crash_date_est_i", "intersection_related_i", 
                     "photos_taken_i", "statements_taken_i", "private_property_i", 
                     "dooring_i", "work_zone_i", "work_zone_type", "workers_present_i", 
                     "latitude", "longitude")
  
  for(col in expected_cols) {
    if(!col %in% names(crashes)){
      crashes[, col] <- NA # set missing columns NA
    }
  }
  
  # (When getting recent crashes from the portal, some of the police report fields will be missing. Annoyingly, the JSON response just omits these fields. added 2022-01-03)
  
  # Fix Lat/Long
  crashes$latitude = as.numeric(crashes$latitude)
  crashes$longitude = as.numeric(crashes$longitude)
  
  # Eliminate non-Chicago or missing lat/long points
  crashes <- crashes %>%
    select(-c(location.coordinates)) %>%
    filter(latitude > 35 & longitude < -85)
  
  # Reorder injury severity levels
  crashes$most_severe_injury <- factor(as.character(crashes$most_severe_injury), 
                                       levels = c("FATAL","INCAPACITATING INJURY","NONINCAPACITATING INJURY","REPORTED, NOT EVIDENT","NO INDICATION OF INJURY")) 
  
  # Convert numbers saved as text to integers
  crashes$injuries_total = as.integer(crashes$injuries_total)
  crashes$injuries_fatal = as.integer(crashes$injuries_fatal)
  crashes$injuries_incapacitating = as.integer(crashes$injuries_incapacitating)
  crashes$injuries_non_incapacitating = as.integer(crashes$injuries_non_incapacitating)
  crashes$injuries_reported_not_evident = as.integer(crashes$injuries_reported_not_evident)
  crashes$year = format(crashes$crash_date,"%Y")
  
  return(crashes)
}

loadCrashData <- function() {
  # NOTE this is a ton of data, load into memory once
  crashes = future(readRDS("./crash_summaries/Crashes_2009_present_IDOT_and_Chicago.rds"))
  return(crashes)
}

getCrashes <- function(crashes, dateFrom = NULL, dateTo = NULL, boundingBox = NULL, crashTypes = NULL) {
  # Argument formats:
  # dateFrom, dateTo = POSIXlt date format
  # boundingBox
  # crashTypes = string or character vector, e.g. c("PEDESTRIAN","PEDALCYCLIST")
  
  # Query the crash database for selected crashes
  filteredCrashes = crashes
  
  if(!is.null(dateFrom)){
    filteredCrashes = filteredCrashes %>%
      filter(crash_date >= dateFrom)
  }
  
  if(!is.null(dateTo)){
    filteredCrashes = filteredCrashes %>%
      filter(crash_date <= dateTo)
  }
  
  if(!is.null(crashTypes)){
    filteredCrashes = filteredCrashes %>%
      filter(first_crash_type %in% crashTypes)
  }
  
  return(filteredCrashes)
}