########################################
# Update Chicago and IDOT Crash Summaries

library(RSocrata)
library(dplyr)
library(sf)

# Delete caches
 files_list <- list.files("../cache")
 
 if (length(files_list) > 0) {
   # Cache file exists
   for (i in seq_along(files_list)) {
     # BUG files_list only lists file names (no path info)
     file.remove(paste0("../cache/",files_list[[i]]))
   }
 }

# Download new Chicago Data Portal crashes
crashes_cached <- paste0("../cache/crashes_",format(Sys.time(), "%Y-%m-%d_%H%M%S"),".rds")
url <-   "https://data.cityofchicago.org/resource/85ca-t3if.json?$"
start_date <- '2020-01-01'
end_date <- Sys.Date()
date <- paste0("crash_date between ","'",start_date,"'"," and ","'",end_date,"'")
query <- paste0(url,"where=",date)
crashes <- read.socrata(query, app_token = Sys.getenv("APP_TOKEN"))

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
saveRDS(crashes,crashes_cached)

# Prep crash data by wards, community areas
# Add boundary layers
commareas <- read_sf("../geo/commareas.geojson")
wards <- read_sf("../geo/wards2015.geojson")
police <- read_sf("../geo/police_districts.geojson")
## Hold on intersections for now
# intersections_geo = read_sf("../geo/all_intersections.geojson")


# Add population to Community Areas
commareas_pop = read.csv("../geo/population/commareas_2010.csv")
commareas_pop$num = as.character(commareas_pop$num)
commareas = left_join(commareas,commareas_pop, by=c("area_numbe" = "num"))

# Make Crashes sf
crashes_geo <- st_as_sf(crashes, coords = c("longitude","latitude"))
crashes_geo <- st_set_crs(crashes_geo, 4326)

# Intersect and add boundary IDs to crash records
crash_in_ca <- st_join(crashes_geo,commareas, join = st_within)
crash_in_ward <- st_join(crashes_geo,wards,join = st_within)
crash_in_police <- st_join(crashes_geo,police,join = st_within)
crashes_geo$commarea <- crash_in_ca$community.x
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

saveRDS(ca_summary, "Chicago_2020_Summary_Community_Areas.rds")
#write.csv(ca_summary, "Chicago_2020_Summary_Community_Areas.csv")

saveRDS(ward_summary, "Chicago_2020_Summary_Wards.rds")
#write.csv(ward_summary, "Chicago_2020_Summary_Wards.csv")

saveRDS(police_summary, "Chicago_2020_Summary_PoliceDist.rds")
#write.csv(police_summary, "Chicago_2020_Summary_PoliceDist.csv")

####################
# Merge Summaries

# Load Merged 2009-2019 Crashes
crashes_2019 = readRDS("../crash_summaries/Crashes_2009_2019_IDOT_and_Chicago.rds")
ca_2019      = readRDS("../crash_summaries/Summary_2009_2019_Community_Areas.rds")
wards_2019   = readRDS("../crash_summaries/Summary_2009_2019_Wards.rds")
police_2019  = readRDS("../crash_summaries/Summary_2009_2019_PoliceDist.rds")
  
updated_crashes = rbind(crashes_2019,crashes)
updated_ca = rbind(ca_2019,ca_summary)
updated_wards = rbind(wards_2019,ward_summary)
updated_police = rbind(police_2019,police_summary)

# Save Merged Results
saveRDS(updated_crashes, "../crash_summaries/Crashes_2009_present_IDOT_and_Chicago.rds")
write.csv(updated_crashes, "../crash_summaries/Crashes_2009_present_IDOT_and_Chicago.csv")

saveRDS(updated_ca, "../crash_summaries/Summary_2009_present_Community_Areas.rds")
write.csv(updated_ca, "../crash_summaries/Summary_2009_present_Community_Areas.csv")

saveRDS(updated_wards, "../crash_summaries/Summary_2009_present_Wards.rds")
write.csv(updated_wards, "../crash_summaries/Summary_2009_present_Wards.csv")

saveRDS(updated_police, "../crash_summaries/Summary_2009_present_PoliceDist.rds")
write.csv(updated_police, "../crash_summaries/Summary_2009_present_PoliceDist.csv")

#########################
# Prep crash time-series

# Monthly Deaths, Injuries, and Reported Crashes, by Ped/Cyc and Other crash types
# monthly_pedcyc_crashes = combined_crashes %>%
#   mutate(monyr = format(crash_date,"%Y-%m"), pedcyc = ifelse(first_crash_type == "PEDESTRIAN" | first_crash_type == "PEDALCYCLIST",first_crash_type,"other")) %>%
#   group_by(monyr,pedcyc) %>%
#   summarize(fatal = sum(injuries_fatal, na.rm = TRUE), serious_inj = sum(injuries_incapacitating, na.rm = TRUE), fatal_serious_inj = fatal + serious_inj, any_inj = sum(injuries_total, injuries_fatal, na.rm = TRUE), crashes = n())
# 
# saveRDS(monthly_pedcyc_crashes, "Crashes_2009_2019_monthly_ped_cyc.rds")
