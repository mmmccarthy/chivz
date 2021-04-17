## Pre-process 2018 and 2019 crashes from Chicago Data Portal
##    in the same format as IDOT crashes have been processed

library(RSocrata)
library(dplyr)
library(sf)

# Data Portal API Key
# USE .Renviron in this directory or Sys.setenv("APP_TOKEN" = "YOUR SOCRATA TOKEN")
readRenviron(file.path("../", ".Renviron"))

# Query Data Portal
url <-   "https://data.cityofchicago.org/resource/85ca-t3if.json?$"
start_date <- '2018-01-01T00:00:00'  # See https://dev.socrata.com/docs/datatypes/floating_timestamp.html for format
end_date <- '2019-12-31T23:59:00'
date <- paste0("crash_date between ","'",start_date,"'"," and ","'",end_date,"'")
# type <- paste0("first_crash_type == 'PEDESTRIAN' OR first_crash_type == 'PEDALCYCLIST'") #,input$crashtype,"'")
query <- paste0(url,"where=",date,"&$order=crash_date ASC")
crashes <- read.socrata(query, app_token = Sys.getenv("APP_TOKEN"))

# Check head(crashes) and tail(crashes) to ensure you have full data

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

# Write crash point data
saveRDS(crashes,"Crashes_2018_2019.rds")

# Prep crash data by wards, community areas
# Add boundary layers
commareas <- read_sf("../geo/commareas.geojson")
tracts <- read_sf("../geo/tracts2010.geojson")
wards <- read_sf("../geo/wards2015.geojson")
police <- read_sf("../geo/police_districts.geojson")

# Add population to Community Areas
commareas_pop = read.csv("../geo/population/commareas_2010.csv")
commareas_pop$num = as.character(commareas_pop$num)
commareas = left_join(commareas,commareas_pop, by=c("area_numbe" = "num"))

# Make Crashes sf
crashes_geo <- st_as_sf(crashes, coords = c("longitude","latitude"))
crashes_geo <- st_set_crs(crashes_geo, 4326)

# Intersect and add boundary IDs to crash records
crash_in_ca <- st_join(crashes_geo,commareas, join = st_within)
crash_in_tract <- st_join(crashes_geo,tracts, join = st_within)
crash_in_ward <- st_join(crashes_geo,wards,join = st_within)
crash_in_police <- st_join(crashes_geo,police,join = st_within)
crashes_geo$commarea <- crash_in_ca$community.x
crashes_geo$tract <- crash_in_tract$GEOID10
crashes_geo$ward <- crash_in_ward$ward
crashes_geo$police_dist <- crash_in_police$dist_num

# Summarize All Crashes and segment Ped/Bike crashes
# Community Areas
ca_summary = crashes_geo %>%
  st_drop_geometry() %>%
  filter(!is.na(most_severe_injury)) %>% # Data portal has some non-injury crashes where all of the summary fields are NA
  mutate(crash_date = as.POSIXct(crash_date), year = format(crash_date,"%Y")) %>%
  group_by(commarea, year, ped_cyc = ifelse(first_crash_type %in% c("PEDESTRIAN", "PEDALCYCLIST"),"yes","no")) %>%
  summarize(crashes = n(), injuries_total = sum(injuries_total), injuries_fatal = sum(injuries_fatal), injuries_incapacitating = sum(injuries_incapacitating), injuries_non_incapacitating = sum(injuries_non_incapacitating), injuries_reported_not_evident = sum(injuries_reported_not_evident))

# Tracts
tract_summary = crashes_geo %>%
  st_drop_geometry() %>%
  filter(!is.na(most_severe_injury)) %>% # Data portal has some non-injury crashes where all of the summary fields are NA
  mutate(crash_date = as.POSIXct(crash_date), year = format(crash_date,"%Y")) %>%
  group_by(tract, year, ped_cyc = ifelse(first_crash_type %in% c("PEDESTRIAN", "PEDALCYCLIST"),"yes","no")) %>%
  summarize(crashes = n(), injuries_total = sum(injuries_total), injuries_fatal = sum(injuries_fatal), injuries_incapacitating = sum(injuries_incapacitating), injuries_non_incapacitating = sum(injuries_non_incapacitating), injuries_reported_not_evident = sum(injuries_reported_not_evident))

# Wards
ward_summary = crashes_geo %>%
  st_drop_geometry() %>%
  filter(!is.na(most_severe_injury)) %>%
  mutate(crash_date = as.POSIXct(crash_date), year = format(crash_date,"%Y")) %>%
  group_by(ward, year, ped_cyc = ifelse(first_crash_type %in% c("PEDESTRIAN", "PEDALCYCLIST"),"yes","no")) %>%
  summarize(crashes = n(), injuries_total = sum(injuries_total), injuries_fatal = sum(injuries_fatal), injuries_incapacitating = sum(injuries_incapacitating), injuries_non_incapacitating = sum(injuries_non_incapacitating), injuries_reported_not_evident = sum(injuries_reported_not_evident))

# Police Districts
police_summary = crashes_geo %>%
  st_drop_geometry() %>%
  filter(!is.na(most_severe_injury)) %>%
  mutate(crash_date = as.POSIXct(crash_date), year = format(crash_date,"%Y")) %>%
  group_by(police_dist, year, ped_cyc = ifelse(first_crash_type %in% c("PEDESTRIAN", "PEDALCYCLIST"),"yes","no")) %>%
  summarize(crashes = n(), injuries_total = sum(injuries_total), injuries_fatal = sum(injuries_fatal), injuries_incapacitating = sum(injuries_incapacitating), injuries_non_incapacitating = sum(injuries_non_incapacitating), injuries_reported_not_evident = sum(injuries_reported_not_evident))

# Save Results

saveRDS(ca_summary, "Chicago_2018_2019_Summary_Community_Areas.rds")
write.csv(ca_summary, "Chicago_2018_2019_Summary_Community_Areas.csv")

saveRDS(tract_summary, "Chicago_2018_2019_Summary_Tracts.rds")
write.csv(tract_summary, "Chicago_2018_2019_Summary_Tracts.csv")

saveRDS(ward_summary, "Chicago_2018_2019_Summary_Wards.rds")
write.csv(ward_summary, "Chicago_2018_2019_Summary_Wards.csv")

saveRDS(police_summary, "Chicago_2018_2019_Summary_PoliceDist.rds")
write.csv(police_summary, "Chicago_2018_2019_Summary_PoliceDist.csv")

