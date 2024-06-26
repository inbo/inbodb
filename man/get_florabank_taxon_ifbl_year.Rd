% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_florabank_taxon_ifbl_year.R
\name{get_florabank_taxon_ifbl_year}
\alias{get_florabank_taxon_ifbl_year}
\title{Get unique combinations of taxon, IFBL-square and year.}
\usage{
get_florabank_taxon_ifbl_year(
  connection,
  starting_year = 2010,
  ifbl_resolution = c("1km-by-1km", "4km-by-4km"),
  taxongroup = c("Vaatplanten", "Mossen", "Lichenen (korstmossen)", "Kranswieren"),
  collect = FALSE
)
}
\arguments{
\item{connection}{A connection to the florabank database. See the example
section for how to connect and disconnect to the database.}

\item{starting_year}{Filter for observations that start from this year
onwards.
Default is 2010.}

\item{ifbl_resolution}{The requested spatial resolution can be either
1km-by-1km \code{IFBL} squares or 4km-by-4km. Default is 1km-by-1km.}

\item{taxongroup}{Choose for which taxonomic group you want the unique
combinations. One of \code{"Vaatplanten"} (the default), \code{"Mossen"},
\code{"Korstmossen"}
or \code{"Kranswieren"}.}

\item{collect}{If FALSE (the default), a remote \code{tbl} object is returned.
This
is like a reference to the result of the query but the full result of the
query is not brought into memory. If TRUE the full result of the query is
collected (fetched) from the database and brought into memory of the working
environment.}
}
\value{
A dataframe with one line for each combination of taxon,
\code{IFBL}-square
(either at 1 km x 1 km or 4 km x 4 km resolution) and year. In case the
resolution is 1 km x 1 km, a variable \code{ifbl_4by4} gives the corresponding
\code{ifbl_4by4} identifier within which the \code{ifbl_1by1} square is located.
In case
the resolution is 4 km x 4 km, the variable \code{ifbl_squares} is a concatenation
of all nested squares with observations for the taxon in the corresponding
year. This can be nested 1 x 1 squares as well as the corresponding 4 x 4
square (the latter is the case if the original resolution of the observation
is at 4 x 4 resolution). In addition, the variable \code{ifbl_number_squares}
gives
the number of unique nested squares where the taxon was observed for that
year and 4 x 4 square combination.
}
\description{
This functions queries all validated observations of the florabank database
and returns unique combinations of taxon, \code{IFBL}-square and year.
Either a 1 km
by 1 km or a 4 km x 4 km resolution can be chosen and a begin year can be
set.
Observations of taxa at genus level or higher are excluded. The taxonomic
group can be chosen.
}
\examples{
\dontrun{
library(inbodb)
# connect to florabank
db_connectie <- connect_inbo_dbase("D0021_00_userFlora")

# get records at 1 km x 1 km resolution for vascular plants from 2010
# (default) without collecting all data into memory (default).
fb_kwartier <- get_florabank_taxon_ifbl_year(db_connectie)
# to collect the data in memory set collect to TRUE or do
fb_kwartier <- collect(fb_kwartier)

# get records at 4 km x 4 km resolution starting from 2000
fb_uur <- get_florabank_taxon_ifbl_year(db_connectie, starting_year = 2000,
 ifbl_resolution = "4km-by-4km", taxongroup = "Mossen")

# disconnect from florabank
dbDisconnect(db_connectie)
}
}
\seealso{
Other florabank: 
\code{\link{get_florabank_observations}()},
\code{\link{get_florabank_traits}()}
}
\concept{florabank}
