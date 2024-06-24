# inbodb 0.0.6

* new functions `get_taxonlijsten_lists()`, `get_taxonlijsten_features()` and
  `get_taxonlijsten_items()` to query the 'D0156_00_Taxonlijsten' database


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
