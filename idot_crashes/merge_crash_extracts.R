
library(dplyr)
library(sf)

crash_extracts = list()
crash_extracts$y2009 = "~/apps/mupp/crash_extracts/Chicago2009Crashes.csv"
crash_extracts$y2010 = "~/apps/mupp/crash_extracts/Chicago2010Crashes.csv"
crash_extracts$y2011 = "~/apps/mupp/crash_extracts/Chicago2011Crashes.csv"
crash_extracts$y2012 = "~/apps/mupp/crash_extracts/Chicago2012Crashes.csv"
crash_extracts$y2013 = "~/apps/mupp/crash_extracts/Chicago2013Crashes.csv"
crash_extracts$y2014 = "~/apps/mupp/crash_extracts/Chicago2014Crashes.csv"
crash_extracts$y2015 = "~/apps/mupp/crash_extracts/Chicago2015Crashes.csv"
crash_extracts$y2016 = "~/apps/mupp/crash_extracts/Chicago2016Crashes.csv"
crash_extracts$y2017 = "~/apps/mupp/crash_extracts/Chicago2017Crashes.csv"

crashes = list()
for (i in seq_along(crash_extracts)) {
  crashes[[i]] = read.csv(crash_extracts[[i]], stringsAsFactors = FALSE)
}

# Align IDOT columns to 2009
crashes_colfix = list()
crashes_colfix[[1]] = crashes[[1]] # base columns
crashes_colfix[[2]] = crashes[[2]] %>%
  transmute(ICN = casenumber,
         CrashID = CrashID,
         CountyCode = County.code,
         CrashYr = year,
         CrashMonth = month,
         CrashDay = day,
         NumberOfVehicles = Nbr.of.Vehicles,
         DayOfWeekCode = Day.of.Week.Int,
         CrashHour = Hour,
         CityCode = City.Code,
         CityClassCode = City.Class.code,
         Township = Township,
         CollisionTypeCode = collType,
         TotalFatals = Total.killed,
         TotalInjured = totalInjuries,
         NoInjuries = No.injuries,
         AInjuries = A.injuries,
         BInjuries = B.injuries,
         CInjuries = C.injuries,
         CrashSeverity = Crash.severity,
         AgencyCode = Agency.code,
         RouteNumber = Route.number,
         Milestation = Milestation,
         ClassOfTrafficwayCode = Class.of.trafficway.Int,
         NHS = National.Highway.System,
         TrafficControlDeviceCode = Traffic.control.device.code,
         RoadSurfaceConditionCode = Road.surface.condition.code,
         RoadDefectsCode = Road.defects.code,
         LightConditionCode = Light.condition.code,
         WeatherCode = Weather.code.Int,
         Cause1Code = Cause.1.code,
         Cause2Code = Cause.2.code,
         RailroadCrossingNumber = Railroad.crossing.number,
         TimeOfCrash = Time.of.crash,
         TrafficControlDeviceConditionCode = Traffic.control.condition.code,
         IntersectionRelated = Intersection.related,
         HitAndRun = Hit.and.run,
         CrashDate = Crash.date,
         NumberOfLanes = Number.of.lanes,
         AlignmentCode = Alignment.code,
         TrafficwayDescriptionCode = Trafficway.description.code,
         RoadwayFunctionalClassCode = Roadway.functional.class,
         WorkZoneRelated = Work.Zone.related,
         City_Township_Flag = City_township.flag,
         TSCrashCoordinateX = Crash.coordinate.X,
         TSCrashCoordinateY = Crash.coordinate.Y,
         TSCrashLatitude = Crash.latitude,
         TSCrashLongitude = Crash.longitude,
         TSCrashLongitude2 = longitude2,
         CrashReportCounty = County.name,
         DayOfWeek = Day.of.Week,
         TypeOfFirstCrash = Type.of.crash,
         CityName = City.name,                 
         CityClass = City.class,
         ClassOfTrafficway = Class.of.trafficway,
         Cause1 = Cause1,
         Cause2 = Cause2,
         TrafficControlDevice = Traffic.Device,
         TrafficControlDeviceCond = Device.condition,
         RoadSurfaceCond = Roadway.surface,
         RoadDefects = Road.defects,
         CrashInjurySeverity = Crash.injury.severity,       
         LightingCond = Light.condition,
         WeatherCond = Weather.code,
         RoadAlignment = Alignment,   
         TrafficwayDescrip = Trafficway.description,
         RoadwayFunctionalClass = Roadway.functional.class.description,
         Investigating.Agency.Descrip = Investigating.agency.description,
         CrashSeverityCd = Crash.injury.severity.code,
         DamagedProperty1 = Property.description.1,
         DamagedProperty2 = Property.description.2,
         AgencyReportYear = Agency.year,
         AgencyReportNumber = Agency.Number,
         SFE = NA,
         DidCrashOccurInWorkZone = NA,
         WorkZoneType = NA,
         WereWorkersPresent = NA,
         WorkZone = NA
        )
crashes_colfix[[3]] = crashes[[3]] %>%
  transmute(ICN = casenumber,
         CrashID = CrashID,
         CountyCode = County.code,
         CrashYr = year,
         CrashMonth = month,
         CrashDay = day,
         NumberOfVehicles = Nbr.of.Vehicles,
         DayOfWeekCode = Day.of.Week.Int,
         CrashHour = Hour,
         CityCode = City.Code,
         CityClassCode = City.Class.code,
         Township = Township,
         CollisionTypeCode = collType,
         TotalFatals = totalKilled,
         TotalInjured = totalInjuries,
         NoInjuries = noInjuries,
         AInjuries = injuryA,
         BInjuries = injuryB,
         CInjuries = injuryC,
         CrashSeverity = Crash.severity,
         AgencyCode = Agency.code,
         RouteNumber = Route.number,
         Milestation = Milestation,
         ClassOfTrafficwayCode = Class.of.trafficway.Int,
         NHS = National.Highway.System,
         TrafficControlDeviceCode = Traffic.control.device.code,
         RoadSurfaceConditionCode = Road.surface.condition.code,
         RoadDefectsCode = Road.defects.code,
         LightConditionCode = Light.condition.code,
         WeatherCode = Weather.code.Int,
         Cause1Code = Cause.1.code,
         Cause2Code = Cause.2.code,
         RailroadCrossingNumber = Railroad.crossing.number,
         TimeOfCrash = Time.of.crash,
         TrafficControlDeviceConditionCode = Traffic.control.condition.code,
         IntersectionRelated = Intersection.related,
         HitAndRun = Hit.and.run,
         CrashDate = Crash.date,
         NumberOfLanes = Number.of.lanes,
         AlignmentCode = Alignment.code,
         TrafficwayDescriptionCode = Trafficway.description.code,
         RoadwayFunctionalClassCode = Roadway.functional.class,
         WorkZoneRelated = Work.Zone.related,
         City_Township_Flag = City_township.flag,
         TSCrashCoordinateX = Crash.coordinate.X,
         TSCrashCoordinateY = Crash.coordinate.Y,
         TSCrashLatitude = Crash.latitude,
         TSCrashLongitude = Crash.longitude,
         TSCrashLongitude2 = longitude2,
         CrashReportCounty = County.name,
         DayOfWeek = Day.of.Week,
         TypeOfFirstCrash = Type.of.crash,
         CityName = City.name,                 
         CityClass = City.class,
         ClassOfTrafficway = Class.of.trafficway,
         Cause1 = Cause1,
         Cause2 = Cause2,
         TrafficControlDevice = Traffic.Device,
         TrafficControlDeviceCond = Device.condition,
         RoadSurfaceCond = Roadway.surface,
         RoadDefects = Road.defects,
         CrashInjurySeverity = Crash.injury.severity,       
         LightingCond = Light.condition,
         WeatherCond = Weather.code,
         RoadAlignment = Alignment,   
         TrafficwayDescrip = Trafficway.description,
         RoadwayFunctionalClass = Roadway.functional.class.description,
         Investigating.Agency.Descrip = Investigating.agency.description,
         CrashSeverityCd = Crash.injury.severity.code,
         DamagedProperty1 = Property.description.1,
         DamagedProperty2 = Property.description.2,
         AgencyReportYear = Agency.year,
         AgencyReportNumber = Agency.Number,
         SFE = NA,
         DidCrashOccurInWorkZone = NA,
         WorkZoneType = NA,
         WereWorkersPresent = NA,
         WorkZone = NA
        )
# 4 through 7 ok
crashes_colfix[[4]] = crashes[[4]] %>%
  mutate(TSCrashLongitude2 = longitude2) %>%
  select(-c(longitude2))
crashes_colfix[[5]] = crashes[[5]] %>%
  mutate(TSCrashLongitude2 = longitude2) %>%
  select(-c(longitude2))
crashes_colfix[[6]] = crashes[[6]] %>%
  mutate(TSCrashLongitude2 = longitude2) %>%
  select(-c(longitude2))
crashes_colfix[[7]] = crashes[[7]] %>%
  mutate(TSCrashLongitude2 = longitude2) %>%
  select(-c(longitude2))

# 8 and 9 missing longitude2 => make longitude negative
crashes_colfix[[8]] = crashes[[8]] %>%
  mutate(TSCrashLongitude2 = -(TSCrashLongitude))

crashes_colfix[[9]] = crashes[[9]] %>%
  mutate(TSCrashLongitude2 = -(as.numeric(TSCrashLongitude)))

# Merge IDOT extracts
crashes_prepped = crashes_colfix[[1]]

for (i in 2:9) {
  crashes_prepped = rbind(crashes_prepped,crashes_colfix[[i]])
}

# Check lat long coords
crashes_good_geo = crashes_prepped %>% 
  mutate(TSCrashLatitude = as.numeric(TSCrashLatitude), TSCrashLongitude2 = as.numeric(TSCrashLongitude2)) %>%
  filter(TSCrashLatitude > 35 & TSCrashLongitude2 < -85)
  
# Get records with bad lat/longs
  crashes_bad_geo = anti_join(crashes_prepped, crashes_good_geo, by = c("ICN","CrashID"))
  
  # Get cases where there may be a good X/Y coord i.e. X/Y exist but Lat/Long is missing
  crashes_bad_geo = crashes_bad_geo %>%
    filter(TSCrashCoordinateX != 0, TSCrashCoordinateY != 0)

  # Project IL state Plane coords
  crashes_bad_geo <- st_as_sf(crashes_bad_geo, coords = c("TSCrashCoordinateX","TSCrashCoordinateY"))
  crashes_bad_geo <- st_set_crs(crashes_bad_geo, 3436) # IL State Plane West PCS
  
  crashes_bad_geo <- st_transform(crashes_bad_geo, 4326) # re-project into WGS 84 GCS
  xycoords = as.data.frame(st_coordinates(crashes_bad_geo)) # X and Y cols
  crashes_fixed_geo = st_drop_geometry(crashes_bad_geo)
  crashes_fixed_geo$TSCrashLatitude = xycoords$Y
  crashes_fixed_geo$TSCrashLongitude2 = xycoords$X
  crashes_fixed_geo$TSCrashCoordinateX = NA
  crashes_fixed_geo$TSCrashCoordinateY = NA
  
crashes_prepped = rbind(crashes_good_geo, crashes_fixed_geo)

# Translate IDOT columns (2009 names) to Chicago Data Portal columns
crashes_export = crashes_prepped %>%
  transmute(crash_record_id = paste0("IDOT_",CrashID),
            rd_no = ifelse(Investigating.Agency.Descrip == "City",AgencyReportNumber,NA), 
                  # for crashes investigated by CPD, get RD no
            crash_date = paste(2000 + CrashYr,sprintf("%02d",CrashMonth),sprintf("%02d",CrashDay),sep = "-"),
            crash_date = paste0(crash_date," ",format(strptime(TimeOfCrash, "%I:%M %p"), "%H:%M:%S")),
            posted_speed_limit = NA,
            traffic_control_device = TrafficControlDevice,
            device_condition = TrafficControlDeviceCond,
            weather_condition = WeatherCond,
            lighting_condition = LightingCond,
            first_crash_type = case_when(
              TypeOfFirstCrash == "1-Pedestrian" ~ "Pedestrian",
              TypeOfFirstCrash == "10-Turning" ~ "Turning",
              TypeOfFirstCrash == "11-Rear end" ~ "Rear End",
              TypeOfFirstCrash == "12-Sideswipe same direction" ~ "Sideswipe Same Direction",
              TypeOfFirstCrash == "13-Sideswipe opp. direction" ~ "Sideswipe Opposite Direction",
              TypeOfFirstCrash == "14-Head on" ~ "Head On",
              TypeOfFirstCrash == "15-Angle" ~ "Angle",
              TypeOfFirstCrash == "2-Pedalcyclist" ~ "Pedalcyclist",
              TypeOfFirstCrash == "3-Train" ~ "Train",
              TypeOfFirstCrash == "4-Animal" ~ "Animal",
              TypeOfFirstCrash == "5-Overturned" ~ "Overturned",
              TypeOfFirstCrash == "6-Fixed object" ~ "Fixed Object",
              TypeOfFirstCrash == "7-Other object" ~ "Other Object",
              TypeOfFirstCrash == "8-Other non collision" ~ "Other Non-Collision",
              TypeOfFirstCrash == "9-Parked motor vehicle" ~ "Parked Motor Vehicle",
              TRUE ~ TypeOfFirstCrash
            ),
            trafficway_type = TrafficwayDescrip,   
            lane_cnt = NumberOfLanes,
            alignment = RoadAlignment,
            roadway_surface_cond = RoadSurfaceCond,
            road_defect = RoadDefects,
            report_type = NA,
            crash_type = NA,
            hit_and_run_i = HitAndRun,
            damage = NA, # over 1500 for state reported crashes without injury
            date_police_notified = NA,    
            prim_contributory_cause = Cause1,
            sec_contributory_cause = Cause2,
            street_no = NA,
            street_direction = NA,
            street_name = NA,
            beat_of_occurrence = NA,
            num_units = NA, # need to join
            most_severe_injury = case_when(
              CrashInjurySeverity == "Fatal Crash" ~ "FATAL",
              CrashInjurySeverity == "A Injury Crash" ~ "INCAPACITATING INJURY",
              CrashInjurySeverity == "B Injury Crash" ~ "NONINCAPACITATING INJURY",
              CrashInjurySeverity == "C Injury Crash" ~ "REPORTED, NOT EVIDENT",
              CrashInjurySeverity == "No Injuries" ~ "NO INDICATION OF INJURY"
            ),
            injuries_total = TotalInjured,
            injuries_fatal = TotalFatals,
            injuries_incapacitating = AInjuries,
            injuries_non_incapacitating = BInjuries,
            injuries_reported_not_evident = CInjuries,
            injuries_no_indication = NA, # possibly # of units without Fatal/A/B/C injuries
            injuries_unknown = NA,
            crash_hour = CrashHour,
            crash_day_of_week = DayOfWeekCode,
            crash_month = CrashMonth,
            location.type = "Point",
            crash_date_est_i = NA,
            intersection_related_i = IntersectionRelated,
            photos_taken_i = NA,
            statements_taken_i = NA,
            private_property_i = NA,
            dooring_i = NA,
            work_zone_i = DidCrashOccurInWorkZone,
            work_zone_type = WorkZoneType,
            workers_present_i = WereWorkersPresent,
            latitude = TSCrashLatitude,
            longitude = TSCrashLongitude2 # must be negative
        )
            
 saveRDS(crashes_export,"IDOT_Crashes_Chicago_2009_2017.rds")
 write.csv(crashes_export,"IDOT_Crashes_Chicago_2009_2017.csv")

# Prep crash data by wards, community areas
    # Add boundary layers
    commareas <- read_sf("../geo/commareas.geojson")
    wards <- read_sf("../geo/wards2015.geojson")
    police <- read_sf("../geo/police_districts.geojson")
    
    # Add population to Community Areas
    commareas_pop = read.csv("../geo/population/commareas_2010.csv")
    commareas_pop$num = as.character(commareas_pop$num)
    commareas = left_join(commareas,commareas_pop, by=c("area_numbe" = "num"))
  
  crashes_geo = crashes_export %>%
    filter(latitude > 35 & longitude < -85)
  crashes_geo$latitude = as.numeric(crashes_geo$latitude)
  crashes_geo$longitude = as.numeric(crashes_geo$longitude)
  
  # Make Crashes sf
  crashes_geo <- st_as_sf(crashes_geo, coords = c("longitude","latitude"))
  crashes_geo <- st_set_crs(crashes_geo, 4326)
  
  crash_in_ca <- st_join(crashes_geo,commareas, join = st_within)
  crash_in_ward <- st_join(crashes_geo,wards,join = st_within)
  crash_in_police <- st_join(crashes_geo,police,join = st_within)
  crashes_geo$commarea <- crash_in_ca$community.x
  crashes_geo$ward <- crash_in_ward$ward
  crashes_geo$police_dist <- crash_in_police$dist_num
  

  # Summarize to community areas
  ca_summary_ped_cyc = crashes_geo %>%
    st_drop_geometry() %>%
    mutate(crash_date = as.POSIXct(crash_date), year = format(crash_date,"%Y")) %>%
    group_by(commarea, year, ped_cyc = ifelse(first_crash_type %in% c("Pedestrian", "Pedalcyclist"),"yes","no")) %>%
    summarize(crashes = n(), injuries_total = sum(injuries_total), injuries_fatal = sum(injuries_fatal), injuries_incapacitating = sum(injuries_incapacitating), injuries_non_incapacitating = sum(injuries_non_incapacitating), injuries_reported_not_evident = sum(injuries_reported_not_evident))

  saveRDS(ca_summary_ped_cyc, "IDOT_2009_2017_Summary_Community_Areas.rds")
  write.csv(ca_summary_ped_cyc, "IDOT_2009_2017_Summary_Community_Areas.csv")
  
  # Summarize to wards
  ward_summary_ped_cyc = crashes_geo %>%
    st_drop_geometry() %>%
    mutate(crash_date = as.POSIXct(crash_date), year = format(crash_date,"%Y")) %>%
    group_by(ward, year, ped_cyc = ifelse(first_crash_type %in% c("Pedestrian", "Pedalcyclist"),"yes","no")) %>%
    summarize(crashes = n(), injuries_total = sum(injuries_total), injuries_fatal = sum(injuries_fatal), injuries_incapacitating = sum(injuries_incapacitating), injuries_non_incapacitating = sum(injuries_non_incapacitating), injuries_reported_not_evident = sum(injuries_reported_not_evident))

  saveRDS(ward_summary_ped_cyc, "IDOT_2009_2017_Summary_Wards.rds")
  write.csv(ward_summary_ped_cyc, "IDOT_2009_2017_Summary_Wards.csv")
  
  # Summarize to police districts
  police_summary_ped_cyc = crashes_geo %>%
    st_drop_geometry() %>%
    mutate(crash_date = as.POSIXct(crash_date), year = format(crash_date,"%Y")) %>%
    group_by(police_dist, year, ped_cyc = ifelse(first_crash_type %in% c("Pedestrian", "Pedalcyclist"),"yes","no")) %>%
    summarize(crashes = n(), injuries_total = sum(injuries_total), injuries_fatal = sum(injuries_fatal), injuries_incapacitating = sum(injuries_incapacitating), injuries_non_incapacitating = sum(injuries_non_incapacitating), injuries_reported_not_evident = sum(injuries_reported_not_evident))

  saveRDS(police_summary_ped_cyc, "IDOT_2009_2017_Summary_PoliceDist.rds")
  write.csv(police_summary_ped_cyc, "IDOT_2009_2017_Summary_PoliceDist.csv")


