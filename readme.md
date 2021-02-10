Chicago Vision Zero Dashboard
=============================

This tool can be used to summarize multiple years of reported crashes, with a focus on identifying intersections and corridors in need of safety interventions to reach Vision Zero goals.

## Web App
This app can be run online at [mmmccarthy.shinyapps.io/vzcrash/](https://mmmccarthy.shinyapps.io/vzcrash/).

## Run App Locally

### Run the R Shiny app

You'll need to install RStudio if you haven't already. The app.R file contains the whole Shiny app and will read in geographic data from `geo/` and summaries from `crash_summaries/`.

### Data Portal API Key (optional)

If you want to access crash data from Chicago's Data Portal, you may need to register and get an API key from [Socrata](https://opendata.socrata.com/login) to allow for more frequent data queries.

The API key can be added by creating a **.Renviron** file:

	APP_TOKEN = YOUR SOCRATA TOKEN

or by updating the script querying the data portal to comment out the `readRenviron` code and set a system environment variable like:

```R	
# USE .Renviron in this directory or Sys.setenv("APP_TOKEN" = "YOUR SOCRATA TOKEN")
# readRenviron(file.path("./", ".Renviron"))
Sys.setenv("APP_TOKEN" = "YOUR SOCRATA TOKEN")
```
	
Either reading in **.Renviron** or setting an environment variable will make your API Key available in `Sys.getenv("APP_TOKEN")`. For example:

```R
read.socrata(query, app_token = Sys.getenv("APP_TOKEN"))
```


## Data Sources

Crash report extracts for 2009 through 2017 were obtained by request from the Illinois Department of Transportation (IDOT). As of September 2017, crash data can be obtained for all police districts on the city's Data Portal. This tool combines IDOT's crash extracts, which are reported by Chicago Police and Illinois State Police on Interstate highways, plus more recent data from Chicago Police to help visualize and summarize crashes over the past 10+ years in Chicago.


#### Disclaimer for 2009-2017 Data

This tool uses crash report extracts obtained from the Illinois Department of Transportation (IDOT), which requires the following disclaimer to be used:

_DISCLAIMER: The motor vehicle crash data referenced herein was provided by the Illinois Department of Transportation. Any conclusions drawn from analysis of the aforementioned data are the sole responsibility of the data recipient(s).  Additionally, for coding years 2015 to present, the Bureau of Data Collection uses the exact latitude/longitude supplied by the investigating law enforcement agency to locate crashes. Therefore, location data may vary in previous years since data prior to 2015 was physically located by bureau personnel._

## Summarizing Crash Data

This repo contains summarized IDOT and Chicago Data Portal crash data, already grouped by community area, ward, and police district boundaries for each year of data.

To re-process these summaries, follow these steps:

1. Process the IDOT yearly extracts (2009-2017, _not included_) first. Here you could add earlier years or switch to IDOT data for 2018 and later years as it becomes available. Run this from the top level directory of the repo:

	```R
	source("idot_crashes/merge_crash_extracts.R", chdir = TRUE)
	```

2. Download and process data from Chicago's Data Portal. See the note about obtaining a Socrata API Key above. You could update this file to pull more recent data than 2019. You can also run **Update.R** to pull newer data (see below).

	```R
	source("chicago_crashes/preprocess_2018_2019.R", chdir = TRUE)
	```

3. Merge the IDOT and Chicago Data Portal crash data and summaries. If you changed any file names in the previous steps, make sure to update them in this script before running.

	```R
	source("crash_summaries/merge.R", chdir = TRUE)
	```

4. Finally run **Update.R** to pull year-to-date data from the Chicago Data Portal. While the other scripts described here are typically run once to pre-process data, this script is run each time we need to pull an update for current year crash data.

	```R
	source("chicago_crashes/update.R", chdir = TRUE)
	```
This script creates/updates the `crash_summaries/Crashes_2009_present_IDOT_and_Chicago.*` (CSV file too large to include here, RDS is included), `crash_summaries/Summary_2009_present_*`, and `chicago_crashes/Crashes_2009_present_monthly_ped_cyc.rds` files containing crash records and summaries for 2009-2019 and 2020 year-to-date.
      