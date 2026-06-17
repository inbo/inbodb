# Query observation data from Meetnetten

This function queries the Meetnetten database for observation data
(standardized counts) for a specified monitoring scheme or for all
monitoring schemes within a specified species group. When no monitoring
scheme or species group is specified, the observations of all monitoring
schemes are returned.

## Usage

``` r
get_meetnetten_observations(
  connection,
  scheme_name = NULL,
  species_group = NULL,
  collect = FALSE
)
```

## Arguments

- connection:

  dbconnection with the database 'S0008_00_Meetnetten' on the
  inbo-sql08-prd.inbo.be server

- scheme_name:

  the name of the monitoring scheme for which you want to extract visit
  data. Data from multiple schemes can be selected by providing a vector
  with the names of the schemes.

- species_group:

  the name of the species group for which you want to extract visit
  data. Data from multiple species groups can be selected by providing a
  vector with the names of the species groups.

- collect:

  If `FALSE` (the default), a remote `tbl` object is returned. This is
  like a reference to the result of the query but the full result of the
  query is not brought into memory. If `TRUE` the full result of the
  query is collected (fetched) from the database and brought into memory
  of the working environment.

## Value

A remote `tbl` object (`collect` = `FALSE`) or a `tibble` dataframe
(`collect` = `TRUE`) with following variables:

- `species_group`

- `scheme`

- `protocol`: the protocol used

- `visit_id`: unique id for a count event

- `start_date`:date of the observation

- `location`: the name of the location

- `sublocation`: the name of the sublocation

- `not_counted`: `TRUE` when the sublocation is not counted

- `sample_id`: unique id for a count subevent (see details)

- `target_species`: `TRUE` when the observed species is the target
  species, `FALSE` when the observed species is a secondary species
  (another species than the target species that can be counted with the
  same protocol, see details)

- `checklist_complete`: whether all secondary species, defined in the
  monitoring scheme, are counted

- `name_nl`: Dutch name of the observed species

- `scientific_name`: scientific name of the observed species

- `sex`: M (male), F (female), U (undefined)

- `activity`: activity of the observed species

- `life_stage`: live stage of the observed species

- `count`: number of individuals counted

- `count_type`: most of the time the number of individuals are counted
  (`count_type` = `exact count`), however for some monitoring schemes
  different type of counts are performed. Check the protocol for more
  information when this is the case.

- `notes`: notes of the observed

- `x` and `y`: when the Meetnetten-app is used, GPS coordinates
  (longitude and latitude, `crs` = `WGS84`) of each observation is
  recorded

## Details

The species monitoring programme of Flanders
([Meetnetten](https://www.meetnetten.be)) consists of a series of
monitoring schemes in which one or more target species are counted based
on a specific protocol. Optionally, other species, that can be counted
using the same protocol, can be recorded as well. When
`checklist_complete` = `TRUE`, all secondary species were counted, and
we can assume that the secondary species that were not recorded are
absent.

Depending on the protocol, counting has to be done at the location or
the sublocation level. Sublocations are, for example, different sections
of a transect. For some monitoring schemes, it is necessary to record
several count subevents at the location level. This is, for example, the
case for the crested newt fyke count protocol, where two fykes are used
per location and the counts are recorded per fyke. For every count
subevent a unique `sample_id` is created.

The protocol of a monitoring scheme also defines for which combinations
of sex, life stage, and activity type the counts have to be recorded.
For example, for the crested newt fyke counts the number of female
adults, male adults and juveniles (sex undefined) are counted. Another
example: in the alcon blue monitoring scheme only the number of eggs are
counted.

It is also important to know that counts can be recorded in the
[Meetnetten](https://www.meetnetten.be) website or by using the
Meetnetten app. When using the Meetnetten app, the GPS coordinates of
all observations are recorded and the observations are assigned to a
location or sublocation based on the coordinates. For example, when you
record a butterfly transect count in the website, you will enter the
total number of individuals per species for each section (the
sublocation) of the transect. When you use the app, you can record the
position of every individual separately in the Meetnetten database. So
when you want to know the total number of individuals per section, you
will have to aggregate the data.

To conclude, it is important to understand how the data is organised for
a certain monitoring scheme, before you start analysing the data. For
more details on the monitoring schemes we refer to Maes et al. (2023)

## References

- Maes D, Piesschaert F, Ledegen H, Van De Poel S, Adriaens T, Anselin
  A, Belpaire C, Breine J, Brosens D, Brys R, De Bruyn L, Decleer K, De
  Knijf G, Devos K, Driessens G, Feys S, Gouwy J, Gyselings R, Herremans
  M, Jacobs I, Lewylle I, Leyssen A, Louette G, Onkelinx T, Packet J,
  Provoost S, Quataert P, Ruyts S, Scheppers T, Speybroeck J, Steeman R,
  Stienen E, Thomaes A, Van Den Berge K, Van Keer K, Van Landuyt W, Van
  Thuyne G, Veraghtert W, Verbelen D, Verbeylen G, Vermeersch G, Westra
  T, Pollet M (2023). Monitoring schemes for species of conservation
  concern in Flanders (northern Belgium). An overview of established
  schemes and the design of an additional monitoring scheme. Reports of
  the Research Institute for Nature and Forest (INBO) 2023 (15).
  Research Institute for Nature and Forest (INBO), Brussels.
  [doi:10.21436/inbor.93332112](https://doi.org/10.21436/inbor.93332112)
  .

## See also

Other meetnetten:
[`get_meetnetten_locations()`](https://inbo.github.io/inbodb/reference/get_meetnetten_locations.md),
[`get_meetnetten_schemes()`](https://inbo.github.io/inbodb/reference/get_meetnetten_schemes.md),
[`get_meetnetten_visits()`](https://inbo.github.io/inbodb/reference/get_meetnetten_visits.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(inbodb)
con <- connect_inbo_dbase("S0008_00_Meetnetten")

# get observations for a specific monitoring scheme and collect data
get_meetnetten_observations(con, scheme_name = "Boomkikker", collect = TRUE)

# get observations for a specific species_group and collect data
get_meetnetten_observations(con, species_group = "libellen", collect = TRUE)

# get observations for all species and do not collect data
observations_all <- get_meetnetten_observations(con)

# Close the connection when done
dbDisconnect(con)
rm(con)
rm(observations_all)
} # }
```
