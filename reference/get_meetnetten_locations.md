# Query monitoring scheme locations from Meetnetten

This function queries the Meetnetten database for the locations and
sublocations for a specified monitoring scheme or for all monitoring
schemes within a specified species group. When no monitoring scheme or
species group is specified, the observations of all monitoring schemes
are returned.

## Usage

``` r
get_meetnetten_locations(connection, scheme_name = NULL, species_group = NULL)
```

## Arguments

- connection:

  dbconnection with the database 'S0008_00_Meetnetten' on the
  inbo-sql08-prd.inbo.be server

- scheme_name:

  the name of the monitoring scheme for which you want to extract
  location data. Data from multiple schemes can be selected by providing
  a vector with the names of the schemes.

- species_group:

  the name of the species group for which you want to extract location
  data. Data from multiple species groups can be selected by providing a
  vector with the names of the species groups.

## Value

When the `sf` package is installed, a list with two `sf` objects is
returned:

- `main_locations`: the main locations of the selected monitoring
  schemes, with following attribute variables:

  - `species_group`

  - `scheme`: name of the monitoring scheme

  - `location`: name of the location

  - `is_sample`: whether the location belongs to the sample of locations
    for the monitoring scheme (see details)

  - `is_active`: when a location is not suited for counting any more,
    the location becomes inactive (`is_active` = `FALSE`)

- `sublocations`: the sublocations (for example the sections of a
  transect) for each of the selected main locations, with following
  attribute variables:

  - `species_group`

  - `scheme`: name of the monitoring scheme

  - `location`: name of the main location

  - `sublocation`: name of the sublocation

  - `is_active`: whether the sublocation is counted or not

When the `sf` package is not installed, a list with two `tibble` objects
is returned, with the same attribute variables as above and an
additional variable `geom` that contains the geometry information in
`wkt` (well-known text) format.

Not all main locations are subdivided in sublocations. So in some cases
the sublocations object is empty.

## Details

Each monitoring scheme of the species monitoring programme of Flanders
[Meetnetten](https://www.meetnetten.be) consists of a fixed set of
locations. A monitoring scheme for rare species includes all locations
where the species occurs. For more common species a sample of locations
is drawn and the the selected locations are included in the monitoring
scheme. In some cases, the monitoring project in
[Meetnetten](https://www.meetnetten.be) also contains locations that are
not part of the sample. These locations can be counted optionally and
are indicated by (`is_sample` = `FALSE`).

It also occurs that a location becomes inaccessible or that the target
species disappears. Then, a locations can be made inactive (`is_active`
= `FALSE`), which means that no observations can be recorded any more.

## See also

Other meetnetten:
[`get_meetnetten_observations()`](https://inbo.github.io/inbodb/reference/get_meetnetten_observations.md),
[`get_meetnetten_schemes()`](https://inbo.github.io/inbodb/reference/get_meetnetten_schemes.md),
[`get_meetnetten_visits()`](https://inbo.github.io/inbodb/reference/get_meetnetten_visits.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(inbodb)
con <- connect_inbo_dbase("S0008_00_Meetnetten")

# get locations for a specific monitoring scheme
locations_heivlinder <- get_meetnetten_locations(con,
                                                 scheme_name = "Heivlinder")

locations_heivlinder$main_locations
locations_heivlinder$sublocations

# get locations for a specific species_group
locations_dragonflies <- get_meetnetten_locations(con,
                                                  species_group = "libellen")

locations_dragonflies$main_locations
locations_dragonflies$sublocations

# Close the connection when done
dbDisconnect(con)
rm(con)
rm(locations_heivlinder)
rm(locations_dragonflies)
} # }
```
