# Query the florabank to get taxon trait values for (a) taxon trait(s)

This function takes as input (part of) a taxon trait name, queries the
florabank and returns the taxon trait values in a tidy data format

## Usage

``` r
get_florabank_traits(connection, trait_name, collect = FALSE)
```

## Arguments

- connection:

  A connection to the florabank database. See the example section for
  how to connect and disconnect to the database.

- trait_name:

  A (part of) a trait name for which you want to get the associated
  taxon-specific trait values. If this is missing, the function returns
  an error and prints a message showing all possible trait names.

- collect:

  If FALSE (the default), a remote `tbl` object is returned. This is
  like a reference to the result of the query but the full result of the
  query is not brought into memory. If TRUE the full result of the query
  is collected (fetched) from the database and brought into memory of
  the working environment.

## Value

A remote `tbl` object (collect = FALSE) or a `tibble` dataframe (collect
= TRUE) containing the trait values for each species and for all
partially matched traits. The dataframe contains the variables
`TaxonID`, `TaxonCode`, `NaamWetenschappelijk`, `NaamNederlands`,
`Kenmerk`, `KenmerkCode`, `Omschrijving`, `Rekenwaarde`, `Bron` and
`ExtraOmschrijving`. The first four variables identify the taxon, the
latter six variables relate to the taxon traits.

## See also

Other florabank:
[`get_florabank_observations()`](https://inbo.github.io/inbodb/reference/get_florabank_observations.md),
[`get_florabank_taxon_ifbl_year()`](https://inbo.github.io/inbodb/reference/get_florabank_taxon_ifbl_year.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(inbodb)
library(dplyr)
# connect to florabank
db_connectie <- connect_inbo_dbase("D0152_00_Flora")

# get all Ellenberg values via partial matching, return as lazy query
fb_ellenberg <- get_florabank_traits(db_connectie, "llenberg")
# collect the data
fb_ellenberg <- fb_ellenberg %>% collect()
# the same can be done by using the collect parameter
fb_ellenberg <-
  get_florabank_traits(db_connectie, "llenberg", collect = TRUE)

# get all red lists via partial matching
fb_rodelijsten <- get_florabank_traits(db_connectie, "rode")

# get only the red list for vascular plant species
fb_rodelijstvaatplanten <-
  get_florabank_traits(db_connectie, "Rode lijst Vaatplanten")

#if the trait_name argument is missing, a list of possible names is printed
get_florabank_traits(db_connectie)

#disconnect from florabank
dbDisconnect(db_connectie)
} # }
```
