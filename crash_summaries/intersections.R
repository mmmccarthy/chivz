## Pre-process crashes by intersection 2009-2018


# Merge IDOT and Chicago crash records
idot = readRDS("../idot_crashes/IDOT_Crashes_Chicago_2009_2017.rds")
chicago = readRDS("../chicago_crashes/Crashes_2018_2019.rds")
crashes = rbind(idot,chicago)

# Create SF and Project
crashes = crashes %>%
  filter(latitude > 35 & longitude < -85)
crashes = st_as_sf(crashes, coords = c("longitude","latitude"))
crashes = st_set_crs(crashes, 4326)

# Project intersections into IL State Plane East (NAD 83 - US Feet)
chi_intersections = read_sf("../geo/all_intersections.geojson")
st_crs(chi_intersections) = 4326
chi_intersections = st_transform(chi_intersections, crs = 3435)
chi_intersections_buffer = st_buffer(chi_intersections,dist=100) # 100 foot buffer

# Project crashes
crashes_projected = st_transform(crashes,crs = 3435)

# Intersect crashes with buffer / handle overlapping buffers
crash_by_intersection = st_join(crashes_projected,chi_intersections_buffer, join = st_within, largest = TRUE)
crashes$intersection = ifelse(!is.na(crash_by_intersection$STREET_NAM),paste0(str_to_title(crash_by_intersection$STREET_NAM)," / ",str_to_title(crash_by_intersection$STREET_NAM_2)),NA)

# Summarize crashes by Intersection
intersection_summary = crashes %>%
  st_drop_geometry() %>%
  filter(first_crash_type %in% c("PEDESTRIAN", "PEDALCYCLIST", "Pedestrian", "Pedalcyclist") & !is.na(most_severe_injury)) %>% # Data portal has some non-injury crashes where all of the summary fields are NA
  mutate(crash_date = as.POSIXct(crash_date), year = format(crash_date,"%Y")) %>%
  group_by(intersection, year) %>%
  summarize(crashes = n(), injuries_total = sum(injuries_total), injuries_fatal = sum(injuries_fatal), injuries_incapacitating = sum(injuries_incapacitating), injuries_non_incapacitating = sum(injuries_non_incapacitating), injuries_reported_not_evident = sum(injuries_reported_not_evident))

# Save results
saveRDS(crashes,"crashes_to_intersection.rds")
saveRDS(intersection_summary,"Summary_2009_2019_Intersections.rds")