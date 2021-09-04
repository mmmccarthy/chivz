## Pre-process crashes by bike route 2009-present

# Merged IDOT and Chicago crash records
crashes = readRDS("Crashes_2009_present_IDOT_and_Chicago.rds")

# Create SF and Project
crashes = crashes %>%
  filter(latitude > 35 & longitude < -85) %>%
  filter(first_crash_type %in% c("PEDESTRIAN","PEDALCYCLIST"))
crashes = st_as_sf(crashes, coords = c("longitude","latitude"))
crashes = st_set_crs(crashes, 4326)

# Project bike routes into IL State Plane East (NAD 83 - US Feet) and add 150 ft buffer
# Also buffer intersections
chi_intersections = read_sf("../geo/all_intersections.geojson")
chi_bikeroutes = read_sf("../geo/bikeroutes.geojson") %>%
  filter(!displayrou %in% c("OFF-STREET TRAIL", "ACCESS PATH")) # Remove off-street bikeways

st_crs(chi_intersections) = 4326
chi_intersections = st_transform(chi_intersections, crs = 3435)
chi_intersections_buffer = st_buffer(chi_intersections,dist=150)

st_crs(chi_bikeroutes) = 4326
chi_bikeroutes = st_transform(chi_bikeroutes, crs = 3435)
chi_bikeroutes_buffer = st_buffer(chi_bikeroutes,dist=100, endCapStyle = "FLAT")

# Project crashes
crashes_projected = st_transform(crashes,crs = 3435)

# Crashes by Intersection
crash_by_intersection = st_join(crashes_projected,chi_intersections_buffer, join = st_within, largest = TRUE)
crashes$intersection = ifelse(!is.na(crash_by_intersection$street_nam) & !is.na(crash_by_intersection$street_nam_2),paste0(str_to_title(crash_by_intersection$street_nam)," / ",str_to_title(crash_by_intersection$street_nam_2)),NA)
crashes$intxid = crash_by_intersection$intxid

# Other crashes join to Bike routes
crashes_projected_nonint = crashes_projected %>% anti_join(crashes[!is.na(crashes$intxid),])



# Summarize crashes by Intersection

intersection_summary = crashes %>%
  st_drop_geometry() %>%
  filter(!is.na(intersection)) %>%
  mutate(crash_date = as.POSIXct(crash_date), year = format(crash_date,"%Y")) %>%
  group_by(intxid, intersection, first_crash_type, year) %>%
  summarize(crashes = n(), injuries_total = sum(injuries_total), injuries_fatal = sum(injuries_fatal), injuries_incapacitating = sum(injuries_incapacitating), injuries_non_incapacitating = sum(injuries_non_incapacitating), injuries_reported_not_evident = sum(injuries_reported_not_evident))

# Save results

saveRDS(crashes,"crashes_to_intersection.rds")
saveRDS(intersection_summary,"Summary_2009_present_Intersections.rds")