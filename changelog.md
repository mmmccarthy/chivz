# Changelog

All notable changes to this project will be documented in this file.

## [1.0] - 2022-03-31

### Added
- Adds bar graph by year and crash type
- CSV downloads

### Updated
- Separate functions into functions.R
- Includes 2021 crashes in cache, including all crash types with any injury (not just pedestrian and cyclist crashes)
- Better instructions and headings

### Removed
- Summaries by polygons (wards, community areas, police districts) are no longer included

## [Untagged] - Mid 2021

### Added

- Download 2020 year-to-date data from the Chicago Data Portal and summarize on launch
- Add crash trends
- Process and display of crashes by intersection

### Updated
- Update Readme
- Fix crashes by intersection year filtering

## [0.6] - 2020-10-11

### Added

- 2018-2019 crash data from the Chicago Data Portal
- Fix issue with IDOT 2015-2017 crash data where the given latitude/longitude was zero or null, but the crash location was given in Illinois State Plane West X/Y coordinates; re-run all affected pre-processed summaries

### Removed
- Remove non-existent 31st police district from city's police districts shapefile. This was the label given to Norridge, Harwood Heights, etc.

## [0.5] - 2020-10-10

### Added

- First tagged release
- Clean, merge, geocode to boundaries, and Summarize IDOT crash data across several years (offline/pre-processed)
- Display IDOT crash data for each type of boundary, for each year or all years, and for all crashes or only Pedestrian/Cyclist crashes using Shiny and Leaflet

### Removed

- Previous versions displayed crash point data from the Chicago Data Portal. This version instead lays a foundation using several years of crash data from IDOT. IDOT's data has been re-formatted to match the data portal's column names and will allow for querying and merging in recent crashes (starting 2018-01-01) from the data portal.

## [0.1] - archive

- Archives a previous approach to consider later


-------

Format from [Keep a Changelog](https://keepachangelog.com/en/1.0.0/)

[unreleased]: https://github.com/mmmccarthy/chivz/compare/v0.6...HEAD
[0.6]: https://github.com/mmmccarthy/chivz/compare/v0.5...v0.6
[0.5]: https://github.com/mmmccarthy/chivz/compare/v0.1...v0.5
[0.1]: https://github.com/mmmccarthy/chivz/releases/tag/v0.1