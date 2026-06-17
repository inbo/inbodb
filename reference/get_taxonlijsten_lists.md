# Query to extract Taxonlijsten from `D0156_00_Taxonlijsten`

This function queries `D0156_00_Taxonlijsten` and gives an overview of
all the taxon lists and list versions currently available in the
database. Only the latest version is shown unless specified otherwise

## Usage

``` r
get_taxonlijsten_lists(
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
TRUE) with variables TaxonlijstType, TaxonlijstCode, Taxonlijst,
Publicatiejaar, Version, ReferentieURL, Criteria, Validering,
Vaststelling.

## See also

Other taxonlijsten:
[`get_taxonlijsten_features()`](https://inbo.github.io/inbodb/reference/get_taxonlijsten_features.md),
[`get_taxonlijsten_items()`](https://inbo.github.io/inbodb/reference/get_taxonlijsten_items.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(inbodb)
con <- connect_inbo_dbase("D0156_00_Taxonlijsten")

# get the most recent version of the 'Rode lijst van de Dagvlinders'
get_taxonlijsten_lists(con, version = 'latest', list =
'%rode%dagvlinders%', collect = FALSE)

# get all recent red lists
get_taxonlijsten_lists(con, list = '%rode lijst%')

# get all taxonlist versions in the database
get_taxonlijsten_lists(con, version = 'all', collect = TRUE)

# use function with default values (only most recent versions)
get_taxonlijsten_lists(con)

# status of red lists
rl <- get_taxonlijsten_lists(con, list = '%rode lijst%')
select(rl,"Taxonlijst", "PublicatieJaar", "Criteria", "Validering",
"Vaststelling")

# Close the connection when done
dbDisconnect(con)
rm(con, rl)
} # }
```
