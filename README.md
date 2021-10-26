# inbodb

Connect to and retrieve data from databases on the INBO server

# Installation
  
To install `inbodb` from the [INBO universe](https://inbo.r-universe.dev/ui#builds),
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

To install `inbodb` from Github, start a new R session and run this code (before loading any packages):

```r
#install.packages("remotes")
remotes::install_github("inbo/inbodb")
```
