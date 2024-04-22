<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Lifecycle:stable](https://img.shields.io/badge/lifecycle-stable-green.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
![r-universe
name](https://inbo.r-universe.dev/badges/:name?color=c04384)
![r-universe package](https://inbo.r-universe.dev/badges/inbodb)
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
