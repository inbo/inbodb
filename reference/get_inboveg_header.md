# Query header information from INBOVEG

This function queries the INBOVEG database for header information
(metadata for a vegetation-recording or relevé) for one or more surveys
and the recorder type. All records, also with 'work needed' are
selected. See the examples for how to get information for all surveys.

## Usage

``` r
get_inboveg_header(
  connection,
  survey_name,
  rec_type,
  additional_variables = character(0),
  multiple = FALSE,
  collect = FALSE
)
```

## Arguments

- connection:

  `dbconnection` with the database 'Cydonia' on the `inbo-sql07-prd`
  server

- survey_name:

  A character string or a character vector giving the name or names of
  the survey(s) for which you want to extract header information. If
  missing, all surveys are returned.

- rec_type:

  A character vector giving the name of record type for which you want
  to extract header information e.g. `'Classic'`, `'Classic-emmer'`,
  `'Classic-ketting'`, `'BioHab'`, `'ABS'`, `'PPA'`. If missing, all
  recording types are returned.

- additional_variables:

  Default character(0). A character vector with names of additional
  variables to select from `ivRecording` table: `CoordinateRefSystem`,
  `GivenLatitute`, `GivenLongitude`, `GivenLatitude2`,
  `GivenLongitude2`, `Pq`, `Homogenous`

- multiple:

  If TRUE, survey_name can take a character vector with multiple survey
  names that must match exactly. If FALSE (the default), survey_name
  must be a single character string (one survey name) that can include
  wildcards to allow partial matches

- collect:

  If FALSE (the default), a remote `tbl` object is returned. This is
  like a reference to the result of the query but the full result of the
  query is not brought into memory. If TRUE the full result of the query
  is collected (fetched) from the database and brought into memory of
  the working environment.

## Value

A remote `tbl` object (collect = FALSE) or a `tibble` dataframe (collect
= TRUE) with variables `RecordingGivid`, `SurveyName`, `UserReference`,
`Observer`, `LocationCode`, `Latitude`, `Longitude`, `Area` (in m2),
`Length` (in cm), `Width` (in cm), `VagueDateType`, `VagueDateBegin`,
`VagueDateEnd`, `SurveyId`, `RecTypeID`, `RecTypeName`.

## See also

Other inboveg:
[`get_inboveg_classification()`](https://inbo.github.io/inbodb/reference/get_inboveg_classification.md),
[`get_inboveg_layer_cover()`](https://inbo.github.io/inbodb/reference/get_inboveg_layer_cover.md),
[`get_inboveg_layer_qualifier()`](https://inbo.github.io/inbodb/reference/get_inboveg_layer_qualifier.md),
[`get_inboveg_ppa()`](https://inbo.github.io/inbodb/reference/get_inboveg_ppa.md),
[`get_inboveg_qualifier()`](https://inbo.github.io/inbodb/reference/get_inboveg_qualifier.md),
[`get_inboveg_recording()`](https://inbo.github.io/inbodb/reference/get_inboveg_recording.md),
[`get_inboveg_relation_recording()`](https://inbo.github.io/inbodb/reference/get_inboveg_relation_recording.md),
[`get_inboveg_survey()`](https://inbo.github.io/inbodb/reference/get_inboveg_survey.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(inbodb)
con <- connect_inbo_dbase("D0010_00_Cydonia")

# get header information from a specific survey and a specific recording type
# and collect the data
header_info <- get_inboveg_header(con, survey_name = "OudeLanden_1979",
rec_type = "Classic", collect = TRUE)

# with additional variables
header_info <- get_inboveg_header(con, survey_name = "OudeLanden_1979",
rec_type = "Classic", additional_variables = c("Pq", "Homogenous"),
collect = TRUE)
# get header information from several specific surveys
header_severalsurveys <- get_inboveg_header(con, survey_name =
c("MILKLIM_Heischraal2012", "NICHE Vlaanderen"), multiple = TRUE)

# get header information of all surveys,  don't collect the data
all_header_info <- get_inboveg_header(con)

# close the connection when done
dbDisconnect(con)
rm(con)
} # }
```
