# Query qualifier information of recordings (relevé) from INBOVEG

This function queries the INBOVEG database for qualifier information
(site qualifier or management qualifier)on recordings for one or more
surveys.

## Usage

``` r
get_inboveg_qualifier(
  connection,
  survey_name,
  qualifier_type,
  multiple = FALSE
)
```

## Arguments

- connection:

  `dbconnection` with the database 'Cydonia' on the `inbo-sql07-prd`
  server

- survey_name:

  A character string or a character vector, depending on multiple
  parameter, giving the name or names of the survey(s) for which you
  want to extract recordings information. If missing, all surveys are
  returned.

- qualifier_type:

  A character vector giving the name of qualifier type for which you
  want to extract information e.g. `'SQ'` (site qualifier), `'MQ'`
  (management qualifier). If missing, all qualifier types are returned.

- multiple:

  If TRUE, survey_name can take a character vector with multiple survey
  names that must match exactly. If FALSE (the default), survey_name
  must be a single character string (one survey name) that can include
  wildcards to allow partial matches

## Value

A dataframe with variables `RecordingGivid` (unique Id),
`UserReference`, `Observer`, `QualifierType`, `QualifierCode`,
`Description`, `2nd QualifierCode`, `2nd Description`,
`3rd QualifierCode`, `3rd Description`, `Elucidation`, in case qualifier
is `'NotSure'`, `ParentID`, `QualifierResource`

## See also

Other inboveg:
[`get_inboveg_classification()`](https://inbo.github.io/inbodb/reference/get_inboveg_classification.md),
[`get_inboveg_header()`](https://inbo.github.io/inbodb/reference/get_inboveg_header.md),
[`get_inboveg_layer_cover()`](https://inbo.github.io/inbodb/reference/get_inboveg_layer_cover.md),
[`get_inboveg_layer_qualifier()`](https://inbo.github.io/inbodb/reference/get_inboveg_layer_qualifier.md),
[`get_inboveg_ppa()`](https://inbo.github.io/inbodb/reference/get_inboveg_ppa.md),
[`get_inboveg_recording()`](https://inbo.github.io/inbodb/reference/get_inboveg_recording.md),
[`get_inboveg_relation_recording()`](https://inbo.github.io/inbodb/reference/get_inboveg_relation_recording.md),
[`get_inboveg_survey()`](https://inbo.github.io/inbodb/reference/get_inboveg_survey.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(inbodb)
con <- connect_inbo_dbase("D0010_00_Cydonia")

# get the qualifiers from one survey
qualifiers_heischraal2012 <- get_inboveg_qualifier(con, survey_name =
"MILKLIM_Heischraal2012")

# get all site qualifiers (SQ) from MILKLIM surveys (partial matching)
qualifiers_milkim <- get_inboveg_qualifier(con, survey_name = "%MILKLIM%",
qualifier_type = "SQ")

# get qualifiers from several specific surveys
qualifiers_severalsurveys <- get_inboveg_qualifier(con, survey_name =
c("MILKLIM_Heischraal2012", "NICHE Vlaanderen"), multiple = TRUE)

# get all qualifiers of all surveys
allqualifiers <- get_inboveg_qualifier(con)

# Close the connection when done
dbDisconnect(con)
rm(con)
} # }
```
