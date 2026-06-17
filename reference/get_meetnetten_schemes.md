# Overview of monitoring schemes in the Meetnetten database

This function queries the Meetnetten database to give an overview of
monitoring schemes that are included.

## Usage

``` r
get_meetnetten_schemes(connection)
```

## Arguments

- connection:

  dbconnection with the database 'S0008_00_Meetnetten' on the
  inbo-sql08-prd.inbo.be server.

## Value

A tibble dataframe with variables species_group, scheme and protocol.

## Details

The species monitoring programme of Flanders
([Meetnetten](https://www.meetnetten.be)) consists of a series of
monitoring schemes. In each monitoring scheme one or more target species
are counted based on a specific protocol. For more details we refer to
Maes et al. (2023)

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
[`get_meetnetten_observations()`](https://inbo.github.io/inbodb/reference/get_meetnetten_observations.md),
[`get_meetnetten_visits()`](https://inbo.github.io/inbodb/reference/get_meetnetten_visits.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(inbodb)
con <- connect_inbo_dbase("S0008_00_Meetnetten")

# get overview of monitoring schemes in meetnetten database
meetnetten_schemes <- get_meetnetten_schemes(con)

# Close the connection when done
dbDisconnect(con)
rm(con)
} # }
```
