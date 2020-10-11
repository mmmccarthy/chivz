# Merge Chicago and IDOT Crash Summaries

# Load IDOT 2009-2017 Summaries

idot_ca = readRDS("../idot_crashes/IDOT_2009_2017_Summary_Community_Areas.rds")
idot_wards = readRDS("../idot_crashes/IDOT_2009_2017_Summary_Wards.rds")
idot_police = readRDS("../idot_crashes/IDOT_2009_2017_Summary_PoliceDist.rds")

# Load Chicago Data Portal 2018-2019 Summaries

chicago_ca = readRDS("../chicago_crashes/Chicago_2018_2019_Summary_Community_Areas.rds")
chicago_wards = readRDS("../chicago_crashes/Chicago_2018_2019_Summary_Wards.rds")
chicago_police = readRDS("../chicago_crashes/Chicago_2018_2019_Summary_PoliceDist.rds")

# Combine

combined_ca = rbind(idot_ca,chicago_ca)
combined_wards = rbind(idot_wards,chicago_wards)
combined_police = rbind(idot_police,chicago_police)

# Save Results

saveRDS(combined_ca, "Summary_2009_2019_Community_Areas.rds")
write.csv(combined_ca, "Summary_2009_2019_Community_Areas.csv")

saveRDS(combined_wards, "Summary_2009_2019_Wards.rds")
write.csv(combined_wards, "Summary_2009_2019_Wards.csv")

saveRDS(combined_police, "Summary_2009_2019_PoliceDist.rds")
write.csv(combined_police, "Summary_2009_2019_PoliceDist.csv")