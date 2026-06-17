# Changelog

## inbodb 0.0.10

- bugfix: solve encoding problem with database Watina by deprecating
  argument `autoconvert_utf8` in function
  [`connect_inbo_dbase()`](https://inbo.github.io/inbodb/reference/connect_inbo_dbase.md)
  (issue [\#70](https://github.com/inbo/inbodb/issues/70))
- defunct argument `multiple` from
  [`get_inboveg_recording()`](https://inbo.github.io/inbodb/reference/get_inboveg_recording.md)
  that was deprecated in version 0.0.5

## inbodb 0.0.9

- bugfix: error on load

## inbodb 0.0.8

- connect to new database server, and connect to old server if database
  is not yet on new server (issue
  [\#67](https://github.com/inbo/inbodb/issues/67))
- fix issue [\#65](https://github.com/inbo/inbodb/issues/65) in
  [`get_taxonlijsten_items()`](https://inbo.github.io/inbodb/reference/get_taxonlijsten_items.md):
  return all results in default `feature = '%'`, also results without
  features
- add documentation on use of package on home page (README)

## inbodb 0.0.7

- update functions
  [`get_florabank_observations()`](https://inbo.github.io/inbodb/reference/get_florabank_observations.md),\
  [`get_florabank_taxon_ifbl_year()`](https://inbo.github.io/inbodb/reference/get_florabank_taxon_ifbl_year.md)
  and
  [`get_florabank_traits()`](https://inbo.github.io/inbodb/reference/get_florabank_traits.md)
  (database ‘D0021_00_userFlora’ is deprecated and replaced with
  ‘D0152_00_Flora’)

## inbodb 0.0.6

- new functions
  [`get_taxonlijsten_lists()`](https://inbo.github.io/inbodb/reference/get_taxonlijsten_lists.md),
  [`get_taxonlijsten_features()`](https://inbo.github.io/inbodb/reference/get_taxonlijsten_features.md)
  and
  [`get_taxonlijsten_items()`](https://inbo.github.io/inbodb/reference/get_taxonlijsten_items.md)
  to query the ‘D0156_00_Taxonlijsten’ database
- check out
  `vignette("How to retrieve data from the Taxonlijsten database", package = "inbodb")`
  for more information
- new functions
  [`get_meetnetten_schemes()`](https://inbo.github.io/inbodb/reference/get_meetnetten_schemes.md),
  [`get_meetnetten_locations()`](https://inbo.github.io/inbodb/reference/get_meetnetten_locations.md),
  [`get_meetnetten_visits()`](https://inbo.github.io/inbodb/reference/get_meetnetten_visits.md),
  [`get_meetnetten_observations()`](https://inbo.github.io/inbodb/reference/get_meetnetten_observations.md)
  to query the Meetnetten database
- check out
  `vignette("How to retrieve data from the Meetnetten database", package = "inbodb")`
  for more information

## inbodb 0.0.5

- Add new function
  [`get_inboveg_ppa()`](https://inbo.github.io/inbodb/reference/get_inboveg_ppa.md)
  to get point-plant-distance type data (PPA) from INBOVEG
  ([\#50](https://github.com/inbo/inbodb/issues/50))
- Simplified
  [`get_inboveg_recording()`](https://inbo.github.io/inbodb/reference/get_inboveg_recording.md)
  so that a user no longer needs to specify `multiple = TRUE` if
  multiple surveys, user references, or `recording_givid`’s are passed
  to their respective arguments. Argument `multiple` is no longer needed
  and is therefore deprecated. A warning is issued if the argument is
  still being used.

## inbodb 0.0.4

- Publish package inbodb on Zenodo (fix problem).

## inbodb 0.0.3

- Added support for Microsoft ODBC Driver 18 for SQL Server
  ([\#43](https://github.com/inbo/inbodb/issues/43)).

## inbodb 0.0.2

- New functions
  [`get_inboveg_layer_cover()`](https://inbo.github.io/inbodb/reference/get_inboveg_layer_cover.md)
  and
  [`get_inboveg_layer_qualifier()`](https://inbo.github.io/inbodb/reference/get_inboveg_layer_qualifier.md).
- Renamed functions `get_inboveg_qualifiers()` as
  [`get_inboveg_qualifier()`](https://inbo.github.io/inbodb/reference/get_inboveg_qualifier.md),
  `get_inboveg_recordings()` as
  [`get_inboveg_recording()`](https://inbo.github.io/inbodb/reference/get_inboveg_recording.md)
  and `get_inboveg_relation()` as
  [`get_inboveg_relation_recording()`](https://inbo.github.io/inbodb/reference/get_inboveg_relation_recording.md).
- Added a `NEWS.md` file to track changes to the package.
