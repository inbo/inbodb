# Query survey information from INBOVEG

This function queries the INBOVEG database for survey information
(metadata about surveys) for one or more survey(s) by the name of the
survey. See the examples for how to get information for all surveys.

## Usage

``` r
get_inboveg_survey(connection, survey_name, collect = FALSE)
```

## Arguments

- connection:

  `dbconnection` with the database 'Cydonia' on the `inbo-sql07-prd`
  server

- survey_name:

  A character vector giving the names of the surveys for which you want
  to extract survey information.

- collect:

  If FALSE (the default), a remote `tbl` object is returned. This is
  like a reference to the result of the query but the full result of the
  query is not brought into memory. If TRUE the full result of the query
  is collected (fetched) from the database and brought into memory of
  the working environment.

## Value

A remote `tbl` object (collect = FALSE) or a `tibble` dataframe (collect
= TRUE) with variables Id, Name, Description, Owner and Creator.

## See also

Other inboveg:
[`get_inboveg_classification()`](https://inbo.github.io/inbodb/reference/get_inboveg_classification.md),
[`get_inboveg_header()`](https://inbo.github.io/inbodb/reference/get_inboveg_header.md),
[`get_inboveg_layer_cover()`](https://inbo.github.io/inbodb/reference/get_inboveg_layer_cover.md),
[`get_inboveg_layer_qualifier()`](https://inbo.github.io/inbodb/reference/get_inboveg_layer_qualifier.md),
[`get_inboveg_ppa()`](https://inbo.github.io/inbodb/reference/get_inboveg_ppa.md),
[`get_inboveg_qualifier()`](https://inbo.github.io/inbodb/reference/get_inboveg_qualifier.md),
[`get_inboveg_recording()`](https://inbo.github.io/inbodb/reference/get_inboveg_recording.md),
[`get_inboveg_relation_recording()`](https://inbo.github.io/inbodb/reference/get_inboveg_relation_recording.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(inbodb)
con <- connect_inbo_dbase("D0010_00_Cydonia")

# get information of a specific survey and collect data
survey_info <- get_inboveg_survey(con, survey_name = "OudeLanden_1979",
collect = TRUE)

# get information of all surveys and collect data
allsurveys <- get_inboveg_survey(con)

# Close the connection when done
dbDisconnect(con)
rm(con)
} # }
```
