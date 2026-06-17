# Query recording (relevé) information from INBOVEG

This function queries the INBOVEG database for relevé information (which
species were recorded in which plots and in which vegetation layers with
which cover) for one or more surveys, or in combination with the unique
ID (`recordingGIVID`) or user reference Wildcards in `survey_name`,
`user_reference` or `recording_givid` should only be used if a character
string (a length one character vector), otherwise values are assumed to
match exactly.

## Usage

``` r
get_inboveg_recording(
  connection,
  survey_name = "%",
  user_reference = "%",
  recording_givid = "%",
  collect = FALSE,
  multiple = deprecated()
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
= TRUE) with variables `RecordingGivid` (unique ID), `User reference`,
`LayerCode`, `CoverCode`, `OriginalName`, `ScientificName`,
`TaxonGroupCode`, `PhenologyCode`, `Comment`, `CoverageCode`, `PctValue`
(percentage coverage), `RecordingScale` (name of the scale of coverage)

## See also

Other inboveg:
[`get_inboveg_classification()`](https://inbo.github.io/inbodb/reference/get_inboveg_classification.md),
[`get_inboveg_header()`](https://inbo.github.io/inbodb/reference/get_inboveg_header.md),
[`get_inboveg_layer_cover()`](https://inbo.github.io/inbodb/reference/get_inboveg_layer_cover.md),
[`get_inboveg_layer_qualifier()`](https://inbo.github.io/inbodb/reference/get_inboveg_layer_qualifier.md),
[`get_inboveg_ppa()`](https://inbo.github.io/inbodb/reference/get_inboveg_ppa.md),
[`get_inboveg_qualifier()`](https://inbo.github.io/inbodb/reference/get_inboveg_qualifier.md),
[`get_inboveg_relation_recording()`](https://inbo.github.io/inbodb/reference/get_inboveg_relation_recording.md),
[`get_inboveg_survey()`](https://inbo.github.io/inbodb/reference/get_inboveg_survey.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(inbodb)
con <- connect_inbo_dbase("D0010_00_Cydonia")

# get the recordings from one survey and collect the data
recording_heischraal2012 <- get_inboveg_recording(con, survey_name =
"MILKLIM_Heischraal2012", collect = TRUE)

# get all recordings from MILKLIM surveys (partial matching), don't collect
recording_milkim <- get_inboveg_recording(con, survey_name = "%MILKLIM%",
collect = FALSE)

# get recordings from several specific surveys
recording_severalsurveys <- get_inboveg_recording(con, survey_name =
c("MILKLIM_Heischraal2012", "NICHE Vlaanderen"),
collect = TRUE)

# get recordings from several specific recordinggivid
recording_severalgivids <- get_inboveg_recording(con,
recording_givid = c("IV2012081609450300","IV2012081610204607"),
collect = TRUE)

# get all recordings of all surveys,  don't collect the data
allrecordings <- get_inboveg_recording(con)

# Close the connection when done
dbDisconnect(con)
rm(con)
} # }
```
