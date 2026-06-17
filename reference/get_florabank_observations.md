# Get all validated observations for one or more taxa from the florabank database

This function takes as input a character vector with one or more names
of species either as scientific names and/or Dutch names. By default
(fixed = FALSE), partial matching will be used (the names are prepended
and appended with %). The function queries the florabank, and returns a
dataframe with observation level information about the matching taxa.

## Usage

``` r
get_florabank_observations(connection, names, fixed = FALSE, collect = FALSE)
```

## Arguments

- connection:

  A connection to the florabank database. See the example section for
  how to connect and disconnect to the database.

- names:

  Default missing. A character vector with scientific names and/or Dutch
  names. If fixed = TRUE, character strings are matched exactly and
  scientific names must include authorship in order to match.

- fixed:

  Logical. If TRUE, names is to be matched as is (no partial matching) .

- collect:

  If FALSE (the default), a remote `tbl` object is returned. This is
  like a reference to the result of the query but the full result of the
  query is not brought into memory. If TRUE the full result of the query
  is collected (fetched) from the database and brought into memory of
  the working environment.

## Value

A dataframe with the following variables: `NaamNederlands`,
`NaamWetenschappelijk`, `AcceptedNaamWetenschappelijk`, `Bron`,
`BeginDatum`, `EindDatum`, `Hok`, `Toponiem`, `CommentaarEvent`,
`CommentaarWaarneming`, `EventID`, `X_event`, `Y_event`, `X_waarneming`,
`Y_waarneming`

## See also

Other florabank:
[`get_florabank_taxon_ifbl_year()`](https://inbo.github.io/inbodb/reference/get_florabank_taxon_ifbl_year.md),
[`get_florabank_traits()`](https://inbo.github.io/inbodb/reference/get_florabank_traits.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# code can only be run if a connection to the database is possible
library(inbodb)
# connect to florabank
db_connectie <- connect_inbo_dbase("D0152_00_Flora")

# query and collect the data using scientific name
succprat1 <-  get_florabank_observations(db_connectie,
names = 'Succisa pratensis Moench', collect = TRUE)

# the same species but using Dutch name
succprat2 <-  get_florabank_observations(db_connectie,
names = 'Blauwe knoop', collect = TRUE)

# providing both a Dutch name and scientific name will not duplicate records
# if they are the same species
succprat3 <- get_florabank_observations(db_connectie,
names = c("Succisa pratensis Moench", "Blauwe knoop"), collect = TRUE)

all.equal(succprat1, succprat2)
all.equal(succprat1, succprat3)

# passing dutch names and scientific names for different species
# is possible (records for each species is returned)
myspecies1 <- get_florabank_observations(db_connectie,
names = c('Succisa pratensis Moench', 'Gevlekte orchis'), collect = TRUE)

# passing multiple dutch names
myspecies2 <- get_florabank_observations(db_connectie,
names = c('Gevlekte orchis', 'Blauwe knoop'),
collect = TRUE)

all.equal(myspecies1, myspecies2)

# using default for collect will return a lazy query
# fixed = TRUE for exact matches only
myspecies3 <-  get_florabank_observations(db_connectie,
names = c('Succisa pratensis Moench', 'Gevlekte orchis'),
fixed = TRUE)

# to collect the data for a lazy query you can also use the collect()
# function:
myspecies3 <- dplyr::collect(myspecies3)

# disconnect from florabank
dbDisconnect(db_connectie)
} # }
```
