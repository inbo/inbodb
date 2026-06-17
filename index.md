# inbodb ![A hexagon with the word inbodb](reference/figures/hexsticker.svg)

Connect to and retrieve data from databases on the INBO server

# Installation

To install `inbodb` from the [INBO
universe](https://inbo.r-universe.dev/builds), start a new R session and
run this code (before loading any packages):

``` r

# Enable the INBO universe (not needed for INBO employees, as this is the default setting)
options(
  repos = c(
    inbo = "https://inbo.r-universe.dev", CRAN = "https://cloud.r-project.org"
  )
)
# Install the packages
install.packages("inbodb")
```

To install `inbodb` from GitHub, start a new R session and run this code
(before loading any packages):

``` r

#install.packages("remotes")
remotes::install_github("inbo/inbodb")
```

# Use `inbodb`

The main function,
[`connect_inbo_dbase()`](https://inbo.github.io/inbodb/reference/connect_inbo_dbase.md),
makes a connection to an INBO database by simply providing the
database’s name as an argument (when connected to the INBO network).
After making this connection, a Connections pane in RStudio shows an
overview of the INBO databases, in which database contents can be
explored by clicking on the icons.

The connection allows to download or query data from the database using
functions of packages [DBI](https://dbi.r-dbi.org/) and
[dbplyr](https://dbplyr.tidyverse.org/). Some of the `DBI` functions
have extra functionality in `inbodb`, for instance
[`dbDisconnect()`](https://dbi.r-dbi.org/reference/dbDisconnect.html)
will also close the Connections pane in RStudio in addition to closing
the connection. Other functions may give more informative errors or
(temporarily) fix small technical issues in addition to the DBI
functionality, to ensure smooth access to the INBO databases.

Some code examples:

``` r

# load packages
library(inbodb)
library(DBI)

# open database connection
con <- connect_inbo_dbase("D0152_00_Flora")

# read a whole table
dbReadTable(con, "Bron")

# query a database using a SQL query
dbGetQuery(con, "SELECT ID, Code, Beschrijving FROM Bron")

# compose a query using R-code and dbplyr
library(dplyr)
tbl(con, "Bron") |>
  select("ID", "Code", "Beschrijving") |>
  collect()

# close the connection
dbDisconnect(con)

# convert column names of an imported dataset to snake_case
janitor::clean_names(dataset)
```

As databases can be rather complex, we also wrote manuals and functions
to easily retain data from some databases:

- [`Florabank`](https://www.vlaanderen.be/inbo/datasets/florabank/) (see
  description of [functions under
  ‘Reference’](https://inbo.github.io/inbodb/reference/index.html))
- [`INBOveg`](https://www.vlaanderen.be/inbo/datasets/inboveg/) (see
  [`vignette("get_data_inboveg", package = "inbodb")`](https://inbo.github.io/inbodb/articles/get_data_inboveg.md)
  and [functions](https://inbo.github.io/inbodb/reference/index.html))
- [`meetnetten.be`](https://www.vlaanderen.be/inbo/datasets/meetnettenbe/)
  (see
  [`vignette("get_data_meetnetten", package = "inbodb")`](https://inbo.github.io/inbodb/articles/get_data_meetnetten.md)
  and [functions](https://inbo.github.io/inbodb/reference/index.html))
- `taxonlijsten` (see
  [`vignette("get_data_taxonlijsten", package = "inbodb")`](https://inbo.github.io/inbodb/articles/get_data_taxonlijsten.md)
  and [functions](https://inbo.github.io/inbodb/reference/index.html))

Some other databases have functions in a dedicated R package to retain
or analyse data, e.g.

- [`LSVIHabitatTypes`](https://inbo.github.io/LSVI/)
- [`watina`](https://inbo.github.io/watina/)
