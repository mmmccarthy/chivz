# preprocess.R
# Run all pre-proccessing steps

source("idot_crashes/merge_crash_extracts.R", chdir = TRUE)
source("chicago_crashes/preprocess_2018_2019.R", chdir = TRUE)
source("crash_summaries/merge.R", chdir = TRUE)