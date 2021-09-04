Data File Locations
=============================

The data used in this tool is available for download within this repository.

### Geographic Files

|File  |Desc  |Source/Notes  |
|:------|:------|:--------------|
| geo/ all_intersections.geojson | Point layer with named street intersections for Chicago | I created this using the city's street centerlines file, also available on the Data Portal. Some intersections have multiple points, particularly 6-leg intersections. |
| geo/ bikeroutes.geojson | Line layer denoting sharrows, bike lanes, and trails | Chicago Data Portal |
| geo/ commareas.geojson | Polygon layer of the city's defined Community Areas. These boundaries do not change frequently. | Chicago Data Portal |
| geo/ police_districts.geojson | Polygon layer of the city's police districts | Chicago Data Portal |
| geo/ wards2015.geojson | Polygon layer of the city's legislative districts. These boundaries change every 10 years after the Census. | Chicago Data Portal |
	
### Crash Summaries

Summaries are available for each boundary type in either a CSV file or RDS for simple reading into R. Summaries have been produced from different datasets, the most current being the last one listed here:

- idot_crashes/IDOT\_2009\_2017\_Summary\_{boundary}.{csv | rds}
- crash_summaries/Summary\_2009\_2019\_{boundary}.{csv | rds}
- crash_summaries/Summary\_2009\_present\_{boundary}.{csv | rds}

Each row is a summary for the given geographic boundary and year, with a row each for separating pedestrian/cyclist crashes from other crash types.

For 2009 Community Areas, the records for the Uptown community area appear as:

| _row number_ |commarea|year|ped_cyc|crashes|injuries_total|injuries_fatal|injuries_incapacitating|injuries_non_incapacitating|injuries_reported_not_evident|
|----|------|----|---|---|---|---|---|---|---|
|1122|UPTOWN|2009|no|981|119|2|12|60|47|
|1123|UPTOWN|2009|yes|94|96|1|9|37|50|

The total number of crash records for 2009 is `981 + 94 = 1075`.

### Processed Crash Records

Again, multiple datasets have been merged in different ways.

- idot_crashes/IDOT\_Crashes\_Chicago\_2009\_2017.rds
- crash_summaries/Crashes\_2009\_2019\_IDOT\_and\_Chicago.rds
- crash_summaries/Crashes\_2009\_present\_IDOT\_and\_Chicago.rds

Each row is a record from the `crashes` table of the crash dataset. Each crash record which may involve a single or multiple persons/vehicles. All columns have been aligned/renamed to match the way data is stored in the Chicago Data Portal.
      