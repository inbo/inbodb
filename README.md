<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Lifecycle:stable](https://img.shields.io/badge/lifecycle-stable-green.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
![GitHub](https://img.shields.io/github/license/inbo/inbodb)
[![Release](https://img.shields.io/github/release/inbo/inbodb.svg)](https://github.com/inbo/inbodb/releases)
[![R build status](https://github.com/inbo/inbodb/actions/workflows/check_on_main.yml/badge.svg)](https://github.com/inbo/inbodb/actions)
![r-universe
name](https://inbo.r-universe.dev/badges/:name?color=c04384)
![r-universe package](https://inbo.r-universe.dev/badges/inbodb)
![GitHub code size in bytes](https://img.shields.io/github/languages/code-size/inbo/inbodb.svg)
![GitHub repo size](https://img.shields.io/github/repo-size/inbo/inbodb.svg)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.6353907.svg)](https://doi.org/10.5281/zenodo.6353907)
<!-- badges: end -->

# inbodb <img src="man/figures/hexsticker.svg" align="right" alt="A hexagon with the word inbodb" width="120" />

Connect to and retrieve data from databases on the INBO server

# Installation
  
To install `inbodb` from the [INBO universe](https://inbo.r-universe.dev/builds),
start a new R session and run this code (before loading any packages):

```r
# Enable the INBO universe (not needed for INBO employees, as this is the default setting)
options(
  repos = c(
    inbo = "https://inbo.r-universe.dev", CRAN = "https://cloud.r-project.org"
  )
)
# Install the packages
install.packages("inbodb")
```

To install `inbodb` from GitHub, start a new R session and run this code (before loading any packages):

```r
#install.packages("remotes")
remotes::install_github("inbo/inbodb")
```

# Use `inbodb`

The main function, `connect_inbo_dbase()`, makes a connection to an INBO database by simply providing the database's name as an argument (when connected to the INBO network).
After making this connection, a Connections pane in RStudio shows an overview of the INBO databases, in which database contents can be explored by clicking on the icons.

The connection allows to download or query data from the database using functions of packages [DBI](https://dbi.r-dbi.org/) and [dbplyr](https://dbplyr.tidyverse.org/).
Some of the `DBI` functions have extra functionality in `inbodb`, for instance `dbDisconnect()` will also close the Connections pane in RStudio in addition to closing the connection.
Other functions may give more informative errors or (temporarily) fix small technical issues in addition to the DBI functionality, to ensure smooth access to the INBO databases.

Some code examples:

```r
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

As databases can be rather complex, we also wrote manuals and functions to easily retain data from some databases:

- [`Florabank`](https://www.vlaanderen.be/inbo/datasets/florabank/) (see description of [functions under 'Reference'](https://inbo.github.io/inbodb/reference/index.html))
- [`INBOveg`](https://www.vlaanderen.be/inbo/datasets/inboveg/) (see `vignette("get_data_inboveg", package = "inbodb")` and [functions](https://inbo.github.io/inbodb/reference/index.html))
- [`meetnetten.be`](https://www.vlaanderen.be/inbo/datasets/meetnettenbe/) (see `vignette("get_data_meetnetten", package = "inbodb")` and [functions](https://inbo.github.io/inbodb/reference/index.html))
- `taxonlijsten` (see `vignette("get_data_taxonlijsten", package = "inbodb")` and [functions](https://inbo.github.io/inbodb/reference/index.html))

Some other databases have functions in a dedicated R package to retain or analyse data, e.g.

- [`LSVIHabitatTypes`](https://inbo.github.io/LSVI/)
- [`watina`](https://inbo.github.io/watina/)
