# Query classification information from INBOVEG

This function queries the INBOVEG database for information on the field
classification (N2000 or local vegetation type, e.g. BWK-code) of the
relevé (recording) for one or more survey(s) by the name of the survey.
See the examples for how to get information for all surveys.

## Usage

``` r
get_inboveg_classification(
  connection,
  survey_name,
  classif,
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
  the survey(s) for which you want to extract Classification
  information. If missing, all surveys are returned.

- classif:

  A character vector giving the Classification code of the vegetation
  type for which you want to extract information. If missing, all
  classifications are returned.

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
= TRUE) with variables `Id`, `SurveyName`, `Classification-code`,
vegetation type / BWK or N2000-list, `LocalClassification`, Description
of the habitat type, Cover-code, Cover in percentage.

## See also

Other inboveg:
[`get_inboveg_header()`](https://inbo.github.io/inbodb/reference/get_inboveg_header.md),
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

# get a specific classification from a survey and collect the data
classif_info <- get_inboveg_classification(con,
survey_name = "MILKLIM_Heischraal2012", classif = "4010", collect = TRUE)

# get the classification from several specific surveys
classif_info <- get_inboveg_classification(con,
  survey_name = c("MILKLIM_Heischraal2012", "NICHE Vlaanderen" ),
  multiple = TRUE)

# get all surveys, all classifications,  don't collect the data
allecodes <- get_inboveg_classification(con)

# Close the connection when done
dbDisconnect(con)
rm(con)
} # }
```
