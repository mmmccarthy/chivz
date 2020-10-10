Chicago Vision Zero Dashboard
=============================

This tool can be used to summarize multiple years of reported crashes, with a focus on identifying intersections and corridors in need of safety interventions to reach Vision Zero goals.

## Web App
This app can be run online at [mmmccarthy.shinyapps.io/vzcrash/](https://mmmccarthy.shinyapps.io/vzcrash/).

## Run App Locally

### Summarize IDOT Crash Extracts

This repo contains summarized IDOT Crash data, already grouped by community area, ward, and police district boundaries for each year of data.

### Run the R Shiny app

You'll need to install RStudio if you haven't already. The app.R file contains the whole Shiny app and will read in geographic data from `geo/` and IDOT summaries from `idot_crashes/`.

### Data Portal API Key (optional)

If you want to access crash data from Chicago's Data Portal, you may need to register and get an API key from [Socrata](https://opendata.socrata.com/login) to allow for more frequent data queries.

The API key can be added by creating a .Renviron file:

	APP_TOKEN = YOUR SOCRATA TOKEN

or by updating **app.R**:
	
	# USE .Renviron in this directory or Sys.setenv("APP_TOKEN" = "YOUR SOCRATA TOKEN")
	# readRenviron(file.path("./", ".Renviron"))
	Sys.setenv("APP_TOKEN" = "YOUR SOCRATA TOKEN")


## Data Sources

Crash report extracts for 2009 to 2017 were obtained by request from the Illinois Department of Transportation (IDOT). As of September 2017, crash data can be obtained for all police districts on the city's Data Portal. This tool combines IDOT's crash extracts, which are reported by Chicago Police and Illinois State Police on Interstate highways, plus more recent data from Chicago Police to help visualize and summarize crashes over the past 10+ years in Chicago.


#### Disclaimer for 2009-2017 Data

This tool uses crash report extracts obtained from the Illinois Department of Transportation (IDOT), which requires the following disclaimer to be used:

_DISCLAIMER: The motor vehicle crash data referenced herein was provided by the Illinois Department of Transportation. Any conclusions drawn from analysis of the aforementioned data are the sole responsibility of the data recipient(s).  Additionally, for coding years 2015 to present, the Bureau of Data Collection uses the exact latitude/longitude supplied by the investigating law enforcement agency to locate crashes. Therefore, location data may vary in previous years since data prior to 2015 was physically located by bureau personnel._
      