# Connect to an INBO database

Connects to an INBO database by simply providing the database's name as
an argument. The function can only be used from within the INBO network.

## Usage

``` r
connect_inbo_dbase(database_name, autoconvert_utf8 = deprecated())
```

## Arguments

- database_name:

  char Name of the INBO database you want to connect

- autoconvert_utf8:

  this argument is not needed any more thanks to better encoding in R
  for Windows and is therefore deprecated, do not use it any more

## Value

odbc connection

## Details

For more information, refer to [this
tutorial](https://tutorials.inbo.be/tutorials/r_database_access/).

## See also

Other connection:
[`dbDisconnect,OdbcConnection-method`](https://inbo.github.io/inbodb/reference/dbDisconnect-OdbcConnection-method.md),
[`dbFetch,OdbcResult-method`](https://inbo.github.io/inbodb/reference/dbFetch-OdbcResult-method.md)

## Author

Stijn Van Hoey <stijnvanhoey@gmail.com>

Els Lommelen <els.lommelen@inbo.be>

## Examples

``` r
if (FALSE) { # \dontrun{
connection <- connect_inbo_dbase("D0152_00_Flora")
connection <- connect_inbo_dbase("W0003_00_Lims")
} # }
```
