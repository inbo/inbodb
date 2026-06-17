# Query layer qualifier information of recordings (relevé) from INBOVEG

This function queries the INBOVEG database for layer qualifier
information on recordings for one or more surveys.

## Usage

``` r
get_inboveg_layer_qualifier(connection, survey_name, multiple = FALSE)
```

## Arguments

- connection:

  `dbconnection` with the database 'Cydonia' on the `inbo-sql07-prd`
  server

- survey_name:

  A character string or a character vector, depending on multiple
  parameter, giving the name or names of the survey(s) for which you
  want to extract information. If missing, all surveys are returned.

- multiple:

  If TRUE, survey_name can take a character vector with multiple survey
  names that must match exactly. If FALSE (the default), survey_name
  must be a single character string (one survey name) that can include
  wildcards to allow partial matches

## Value

A dataframe with variables `Name` (of the survey), `RecordingGivid`
(unique Id), `UserReference`, `LayerCode`, `LayerDescription`,
`QualifierCode`, `Qualifier Description`, `Elucidation` and `NotSure` in
case the qualifier is doubtful, `CoverCode` and `Cover percentage`

## See also

Other inboveg:
[`get_inboveg_classification()`](https://inbo.github.io/inbodb/reference/get_inboveg_classification.md),
[`get_inboveg_header()`](https://inbo.github.io/inbodb/reference/get_inboveg_header.md),
[`get_inboveg_layer_cover()`](https://inbo.github.io/inbodb/reference/get_inboveg_layer_cover.md),
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

# get the layer qualifiers from one survey
layerqualifiers_Gagealutea <-
    get_inboveg_layer_qualifier(con, survey_name = "GageaLutea_1980")

# get all layer qualifiers from MILKLIM surveys (partial matching)
layerqualifiers_milkim <-
  get_inboveg_layer_qualifier(con, survey_name = "%MILKLIM%")

# get layer qualifiers from several specific surveys
layerqualifiers_severalsurveys <- get_inboveg_layer_qualifier(con,
  survey_name = c("MILKLIM_Overstroming", "NICHE Vlaanderen"),
  multiple = TRUE)

# get all layer qualifiers of all surveys
alllayerqualifiers <- get_inboveg_layer_qualifier(con)

# Close the connection when done
dbDisconnect(con)
rm(con)
} # }
```
