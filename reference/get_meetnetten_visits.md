# Query visit data from Meetnetten

This function queries the Meetnetten database for visit data (data about
a counting event) for a specified monitoring scheme or for all
monitoring schemes within a specified species group. When no monitoring
scheme or species group is specified, the visits of all monitoring
schemes are returned.

## Usage

``` r
get_meetnetten_visits(
  connection,
  scheme_name = NULL,
  species_group = NULL,
  collect = FALSE
)
```

## Arguments

- connection:

  dbconnection with the database 'S0008_00_Meetnetten' on the
  inbo-sql08-prd.inbo.be server.

- scheme_name:

  the name of the monitoring scheme for which you want to extract visit
  data.

- species_group:

  the name of the species group for which you want to extract visit
  data.

- collect:

  If `FALSE` (the default), a remote tbl object is returned. This is
  like a reference to the result of the query but the full result of the
  query is not brought into memory. If `TRUE` the full result of the
  query is collected (fetched) from the database and brought into memory
  of the working environment.

## Value

A remote `tbl` object (`collect` = `FALSE`) or a `tibble` dataframe
(`collect` = `TRUE`) with following variables:

- `species_group`

- `scheme`: the name of the monitoring scheme

- `protocol`: the protocol used

- `location`: the name of the location

- `visit_id`: unique id for a count event

- `validation_status`: validation status of the visit (visits that are
  validated and not approved are not provided)

  - `10`: visit not validated

  - `100`: visit validated and approved

- `start_date`: the start date of the visit

- `start_time`: the start time of the visit

- `end_date`: the end date of the visit

- `end_time`: the end time of the visit

- `date_created`: the date at which the data was imported in the
  database

- `visit_status`: the status of the visit (determined by the observer)
  using following categories:

  - `conform protocol`: the protocol was applied

  - `weersomstandigheden waren ongunstig`: weather conditions were
    unfavourable

  - `telmethode uit handleiding niet gevolgd`: the protocol was not
    applied

  - `geen veldwerk mogelijk - locatie ontoegankelijk`: counting was not
    possible because the location is inaccessible

  - `geen veldwerk mogelijk - locatie is ongeschikt voor de soort`:
    counting was not possible because the location is not suitable for
    the species

- `for_analysis`: whether the data is suited for analysis (determined by
  the validator)

- `for_targets`: every year targets are set in terms of the number of
  locations that have to be counted per monitoring scheme; when
  `for_targets` = `TRUE` the visit contributes to these targets

- `notes`: notes by the observer

## See also

Other meetnetten:
[`get_meetnetten_locations()`](https://inbo.github.io/inbodb/reference/get_meetnetten_locations.md),
[`get_meetnetten_observations()`](https://inbo.github.io/inbodb/reference/get_meetnetten_observations.md),
[`get_meetnetten_schemes()`](https://inbo.github.io/inbodb/reference/get_meetnetten_schemes.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(inbodb)
con <- connect_inbo_dbase("S0008_00_Meetnetten")

# get visits for a specific monitoring scheme and collect data
get_meetnetten_visits(con, scheme_name = "Boomkikker", collect = TRUE)

# get visits for a specific species_group and collect data
get_meetnetten_visits(con, species_group = "libellen", collect = TRUE)

# get visits for all species and do not collect data
visits_all <- get_meetnetten_visits(con)

# Close the connection when done
dbDisconnect(con)
rm(con)
rm(visits_all)
} # }
```
