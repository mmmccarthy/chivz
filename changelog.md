# Changelog

All notable changes to this project will be documented in this file.

## [Unreleased]

## [v0.5] - 2020-10-10

### Added
- First tagged release
- Clean, merge, geocode to boundaries, and Summarize IDOT crash data across several years (offline/pre-processed)
- Display IDOT crash data for each type of boundary, for each year or all years, and for all crashes or only Pedestrian/Cyclist crashes using Shiny and Leaflet

### Removed
- Previous versions displayed crash point data from the Chicago Data Portal. This version instead lays a foundation using several years of crash data from IDOT. IDOT's data has been re-formatted to match the data portal's column names and will allow for querying and merging in recent crashes (starting 2018-01-01) from the data portal.


-------

Format from [Keep a Changelog](https://keepachangelog.com/en/1.0.0/)

[unreleased]: https://github.com/mmmccarthy/chivz/compare/v0.5...HEAD
[1.1.0]: https://github.com/mmmccarthy/chivz/releases/tag/v0.5