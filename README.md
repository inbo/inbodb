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
