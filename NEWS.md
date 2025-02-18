# inbodb 0.0.7

* update functions `get_florabank_observations()`,  
  `get_florabank_taxon_ifbl_year()` and
  `get_florabank_traits()` (database 'D0021_00_userFlora' is deprecated and 
  replaced with 'D0152_00_Flora')

# inbodb 0.0.6

* new functions `get_taxonlijsten_lists()`, `get_taxonlijsten_features()` and
  `get_taxonlijsten_items()` to query the 'D0156_00_Taxonlijsten' database
* check out `vignette("How to retrieve data from the Taxonlijsten database",
  package = "inbodb")` for more information
* new functions `get_meetnetten_schemes()`, `get_meetnetten_locations()`,
  `get_meetnetten_visits()`, `get_meetnetten_observations()` to query the
  Meetnetten database
* check out `vignette("How to retrieve data from the Meetnetten database",
  package = "inbodb")` for more information

# inbodb 0.0.5

* Add new function `get_inboveg_ppa()` to get point-plant-distance type data
  (PPA) from INBOVEG (#50)
* Simplified `get_inboveg_recording()` so that a user no longer needs to specify
  `multiple = TRUE` if multiple surveys, user references, or `recording_givid`'s
  are passed to their respective arguments.
  Argument `multiple` is no longer needed and is therefore deprecated.
  A warning is issued if the argument is still being used.

# inbodb 0.0.4

* Publish package inbodb on Zenodo (fix problem).

# inbodb 0.0.3

* Added support for Microsoft ODBC Driver 18 for SQL Server (#43).

# inbodb 0.0.2

* New functions `get_inboveg_layer_cover()` and `get_inboveg_layer_qualifier()`.
* Renamed functions `get_inboveg_qualifiers()` as `get_inboveg_qualifier()`,
  `get_inboveg_recordings()` as `get_inboveg_recording()` and
  `get_inboveg_relation()` as `get_inboveg_relation_recording()`.
* Added a `NEWS.md` file to track changes to the package.
