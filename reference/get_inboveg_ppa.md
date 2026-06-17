# Query PPA (point-plant distance) information from INBOVEG

This function queries the INBOVEG database for PPA-type relevé
information (which species were recorded at what distance from a point
location) for one or more surveys, or in combination with the unique ID
(`recordingGIVID`) or user reference Wildcards in `survey_name`,
`user_reference` or `recording_givid` should only be used if a character
string (a length one character vector), otherwise values are assumed to
match exactly.

## Usage

``` r
get_inboveg_ppa(
  connection,
  survey_name = "%",
  user_reference = "%",
  recording_givid = "%",
  collect = FALSE
)
```

## Arguments

- connection:

  `dbconnection` with the database 'Cydonia' on the `inbo-sql07-prd`
  server

- survey_name:

  A character string or a character vector, giving the name or names of
  the survey(s) for which you want to extract relevé information. As
  default (`survey_name` = "%") all surveys are returned.

- user_reference:

  A character string or a character vector giving the name of a
  recording for which you want to extract relevé information. As default
  (`user_reference` = "%") all user-references are returned.

- recording_givid:

  A character string or a character vector giving the unique id of a
  recording for which you want to extract relevé information. As default
  (`recording_givids` = "%") all `recording_givids` are returned.

- collect:

  If FALSE (the default), a remote `tbl` object is returned. This is
  like a reference to the result of the query but the full result of the
  query is not brought into memory. If TRUE the full result of the query
  is collected (fetched) from the database and brought into memory of
  the working environment.

## Value

A remote `tbl` object (collect = FALSE) or a `tibble` dataframe (collect
= TRUE) with variables `SurveyName`, `RecordingGivid`, `UserReference`,
`DateRecording`, `LocationCode`, `CoordinateRefSystem`, `GivenLatitude`,
`GivenLongitude`, `GivenLatitude2`, `GivenLongitude2`,
`MaxSearchEffortUnit`, `MaxSearchEffortLabel`, `Indirect`, `NotSure`,
`LayerCode`, `LayerCover`, `OriginalName`, `ScientificName`,
`TaxonGroupCode`, `PhenologyCode`, `Distance`, `Comment`
`DateIdentification`, `RecordTypeName`

## See also

Other inboveg:
[`get_inboveg_classification()`](https://inbo.github.io/inbodb/reference/get_inboveg_classification.md),
[`get_inboveg_header()`](https://inbo.github.io/inbodb/reference/get_inboveg_header.md),
[`get_inboveg_layer_cover()`](https://inbo.github.io/inbodb/reference/get_inboveg_layer_cover.md),
[`get_inboveg_layer_qualifier()`](https://inbo.github.io/inbodb/reference/get_inboveg_layer_qualifier.md),
[`get_inboveg_qualifier()`](https://inbo.github.io/inbodb/reference/get_inboveg_qualifier.md),
[`get_inboveg_recording()`](https://inbo.github.io/inbodb/reference/get_inboveg_recording.md),
[`get_inboveg_relation_recording()`](https://inbo.github.io/inbodb/reference/get_inboveg_relation_recording.md),
[`get_inboveg_survey()`](https://inbo.github.io/inbodb/reference/get_inboveg_survey.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(inbodb)
con <- connect_inbo_dbase("D0010_00_Cydonia")

# get the recordings from one survey and collect the data
specifieke_survey <- get_inboveg_ppa(con, survey_name =
"LEN_sinusmaaiproject_ppa", collect = TRUE)

# get all recordings from with partial matching, don't collect
partial_match <- get_inboveg_ppa(con, survey_name = "%LEN%",
collect = FALSE)

# get recordings from several specific recordinggivid
recording_severalgivids <- get_inboveg_ppa(con,
recording_givid = c("IV2024040411243457","IV2024040411263782"),
collect = TRUE)

# get all PPA-type recordings of all surveys,  don't collect the data
all_ppa <- get_inboveg_ppa(con)

# Close the connection when done
dbDisconnect(con)
rm(con)
} # }
```
