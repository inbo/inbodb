# Query to extract Taxonlist features from `D0156_00_Taxonlijsten`

This function queries `D0156_00_Taxonlijsten` and gives an overview of
all the features associated with a `TaxonlijstVersie` (a red list status
or an annex of the Habitat Directive are examples of a feature). This is
an auxiliary function to check the accepted values (KenmerkwaardeCodes)
of the feature parameter in the core function `get_taxonlijsten_items`

## Usage

``` r
get_taxonlijsten_features(
  connection,
  list = "%",
  version = c("latest", "old", "all"),
  collect = FALSE
)
```

## Arguments

- connection:

  dbconnection with the database `D0156_00_Taxonlijsten` on the
  inbo-sql07-prd server

- list:

  name of the taxonlist that you want to retrieve. Wildcards % are
  allowed. Case insensitive.

- version:

  A choice ('latest', 'old', 'all'). If 'latest' (the default) only the
  most recent version is returned. If 'old' all but the most recent
  version is returned. If 'all' all versions are returned.

- collect:

  If FALSE (the default), a remote tbl object is returned. This is like
  a reference to the result of the query but the full result of the
  query is not brought into memory. If TRUE the full result of the query
  is collected (fetched) from the database and brought into memory of
  the working environment.

## Value

A remote tbl object (collect = FALSE) or a tibble dataframe (collect =
TRUE) with variables Taxonlijst, Publicatiejaar, Version, Kenmerkcode,
KenmerkBeschrijving, KenmerkwaardeCode, KenmerkwaardeBeschrijving

## See also

Other taxonlijsten:
[`get_taxonlijsten_items()`](https://inbo.github.io/inbodb/reference/get_taxonlijsten_items.md),
[`get_taxonlijsten_lists()`](https://inbo.github.io/inbodb/reference/get_taxonlijsten_lists.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(inbodb)
con <- connect_inbo_dbase("D0156_00_Taxonlijsten")

# get features of all versions of the 'Rode lijst van de Dagvlinders'
get_taxonlijsten_features(con, version = 'all', list = '%rode%dagvlinders%'
, collect = TRUE)

# get features of Habitattypical fauna
get_taxonlijsten_features(con, list = '%Habitattyp%fauna%')

# use function with default values (all features of recent versions)
get_taxonlijsten_features(con)

# note that function also returns taxonlists without features
get_taxonlijsten_features(con, list = '%SBP%')

# Close the connection when done
dbDisconnect(con)
rm(con)
} # }
```
