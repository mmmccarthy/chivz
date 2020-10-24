########################################
# Merge Chicago and IDOT Crash Summaries

# Load IDOT 2009-2017 Summaries

idot_full = readRDS("../idot_crashes/IDOT_Crashes_Chicago_2009_2017.rds")
idot_ca = readRDS("../idot_crashes/IDOT_2009_2017_Summary_Community_Areas.rds")
idot_wards = readRDS("../idot_crashes/IDOT_2009_2017_Summary_Wards.rds")
idot_police = readRDS("../idot_crashes/IDOT_2009_2017_Summary_PoliceDist.rds")

# Load Chicago Data Portal 2018-2019 Summaries

chicago_full = readRDS("../chicago_crashes/Crashes_2018_2019.rds")
chicago_ca = readRDS("../chicago_crashes/Chicago_2018_2019_Summary_Community_Areas.rds")
chicago_wards = readRDS("../chicago_crashes/Chicago_2018_2019_Summary_Wards.rds")
chicago_police = readRDS("../chicago_crashes/Chicago_2018_2019_Summary_PoliceDist.rds")

# Combine

combined_crashes = rbind(idot_full,chicago_full)
combined_ca = rbind(idot_ca,chicago_ca)
combined_wards = rbind(idot_wards,chicago_wards)
combined_police = rbind(idot_police,chicago_police)

# Save Results

saveRDS(combined_crashes, "Crashes_2009_2019_IDOT_and_Chicago.rds")
write.csv(combined_crashes, "Crashes_2009_2019_IDOT_and_Chicago.csv")

saveRDS(combined_ca, "Summary_2009_2019_Community_Areas.rds")
write.csv(combined_ca, "Summary_2009_2019_Community_Areas.csv")

saveRDS(combined_wards, "Summary_2009_2019_Wards.rds")
write.csv(combined_wards, "Summary_2009_2019_Wards.csv")

saveRDS(combined_police, "Summary_2009_2019_PoliceDist.rds")
write.csv(combined_police, "Summary_2009_2019_PoliceDist.csv")

#########################
# Prep crash time-series

# Monthly Deaths, Injuries, and Reported Crashes, by Ped/Cyc and Other crash types
monthly_pedcyc_crashes = combined_crashes %>%
  mutate(monyr = format(crash_date,"%Y-%m"), pedcyc = ifelse(first_crash_type == "PEDESTRIAN" | first_crash_type == "PEDALCYCLIST","yes","no")) %>%
  group_by(monyr,pedcyc) %>%
  summarize(fatal = sum(injuries_fatal), serious_inj = sum(injuries_incapacitating), fatal_serious_inj = fatal + serious_inj, any_inj = sum(injuries_total, injuries_fatal), crashes = n())

saveRDS(monthly_pedcyc_crashes, "Crashes_2009_2019_monthly_ped_cyc.rds")
