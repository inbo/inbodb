#' Get unique combinations of taxon, IFBL-square and year.
#'
#' This functions queries all validated observations of the florabank database
#' and returns unique combinations of taxon, IFBL-square and year. Either a 1 km
#' by 1 km or a 4 km x 4 km resolution can be chosen and a begin year can be
#' set.
#' Observations of taxa at genus level or higher are excluded. The taxonomic
#' group can be chosen.
#'
#' @param connection A connection to the florabank database. See the example
#' section for how to connect and disconnect to the database.
#'
#' @param starting_year Filter for observations that start from this year
#' onwards.
#' Default is 2010.
#'
#' @param ifbl_resolution The requested spatial resolution can be either
#' 1km-by-1km IFBL squares or 4km-by-4km. Default is 1km-by-1km.
#'
#' @param taxongroup Choose for which taxonomic group you want the unique
#' combinations. One of "Vaatplanten" (the default), "Mossen", "Korstmossen"
#' of "Kranswieren".
#'
#' @param collect If FALSE (the default), a remote tbl object is returned. This
#' is like a reference to the result of the query but the full result of the
#' query is not brought into memory. If TRUE the full result of the query is
#' collected (fetched) from the database and brought into memory of the working
#' environment.
#'
#' @return A dataframe with one line for each combination of taxon, IFBL-square
#' (either at 1 km x 1 km or 4 km x 4 km resolution) and year. In case the
#' resolution is 1 km x 1 km, a variable ifbl_4by4 gives the corresponding
#' ifbl_4by4 identifier within which the ifbl_1by1 square is located. In case
#' the resolution is 4 km x 4 km, the variable ifbl_squares is a concatenation
#' of all nested squares with observations for the taxon in the corresponding
#' year. This can be nested 1 x 1 squares as well as the corresponding 4 x 4
#' square (the latter is the case if the original resolution of the observation
#' is at 4 x 4 resolution). In addition, the variable ifbl_number_squares gives
#' the number of unique nested squares where the taxon was observed for that
#' year and 4 x 4 square combination.
#'
#' @importFrom glue glue_sql
#' @importFrom assertthat assert_that
#' @importFrom dplyr %>% group_by summarize n ungroup sql collect
#' @importFrom rlang .data
#'
#' @export
#' @family florabank
#' @examples
#' \dontrun{
#' library(inbodb)
#' library(DBI)
#' library(odbc)
#' library(dplyr)
#' # connect to florabank
#' db_connectie <- connect_inbo_dbase("D0021_00_userFlora")
#'
#' # get records at 1 km x 1 km resolution for vascular plants from 2010
#' # (default) without collecting all data into memory (default).
#' fb_kwartier <- get_florabank_taxon_ifbl_year(db_connectie)
#' # to collect the data in memory set collect to TRUE or do
#' fb_kwartier <- collect(fb_kwartier)
#'
#' # get records at 4 km x 4 km resolution starting from 2000
#' fb_uur <- get_florabank_taxon_ifbl_year(db_connectie, starting_year = 2000,
#'  ifbl_resolution = "4km-by-4km", taxongroup = "Mossen")
#'
#' # disconnect from florabank
#' dbDisconnect(db_connectie)
#' }

get_florabank_taxon_ifbl_year <- function(connection,
                                      starting_year = 2010,
                                      ifbl_resolution = c("1km-by-1km",
                                                          "4km-by-4km"),
                                      taxongroup = c("Vaatplanten",
                                                     "Mossen",
                                                     "Lichenen (korstmossen)",
                                                     "Kranswieren"),
                                      collect = FALSE) {

  assert_that(inherits(connection, what = "Microsoft SQL Server"),
              msg = "Not a connection object to database.")
  assert_that(connection@info$dbname == "D0021_00_userFlora")

  assert_that(is.numeric(starting_year))
  assert_that(starting_year <= as.numeric(format(Sys.Date(), "%Y")))

  ifbl_resolution <- match.arg(ifbl_resolution)
  taxongroup <- match.arg(taxongroup)



  if (ifbl_resolution == "4km-by-4km") {
    glue_statement <- glue_sql(
      "SELECT DISTINCT
    tblIFBLHok.Code AS hok
    , SUBSTRING(tblIFBLHok.Code, 1, 5) AS ifbl_4by4
    , Year(tblWaarneming.BeginDatum) AS Jaar
    , relTaxonTaxon.TaxonIDParent
    , tblTaxon.Code AS Taxoncode
    FROM
    (((tblMeting INNER JOIN
      (tblIFBLHok INNER JOIN tblWaarneming
        ON tblIFBLHok.ID = tblWaarneming.IFBLHokID)
      ON tblMeting.WaarnemingID = tblWaarneming.ID)
    INNER JOIN relTaxonTaxon ON tblMeting.TaxonID = relTaxonTaxon.TaxonIDChild)
    INNER JOIN tblTaxon ON relTaxonTaxon.TaxonIDParent = tblTaxon.ID)
    INNER JOIN relTaxonTaxonGroep ON tblTaxon.ID = relTaxonTaxonGroep.TaxonID
    INNER JOIN tblTaxonGroep
      ON relTaxonTaxonGroep.TaxonGroepID = tblTaxonGroep.ID
    WHERE
    tblIFBLHok.Code LIKE '%-%' AND
    tblTaxon.Code NOT LIKE '%-sp' AND
    Year([tblWaarneming].[BeginDatum]) >={starting_year} AND
    (Year([tblWaarneming].[BeginDatum])=Year([tblWaarneming].[EindDatum])) AND
    (tblTaxonGroep.Naam={taxongroup}) AND
    (tblMeting.MetingStatusCode='GDGA' OR tblMeting.MetingStatusCode='GDGK')
    ORDER BY
    Year(tblWaarneming.BeginDatum) DESC OFFSET 0 ROWS",
      starting_year = starting_year,
      taxongroup = taxongroup,
      .con = connection)
    glue_statement <- iconv(glue_statement, from =  "UTF-8", to = "latin1")
    query_result <- tbl(connection, sql(glue_statement))


    query_result <- query_result %>%
      group_by(.data$ifbl_4by4, .data$Jaar, .data$TaxonIDParent,
               .data$Taxoncode) %>%
      #paste with collapse does not translate to sql
      #str_flatten() is not available for Microsoft SQL Server
      #sql(STRING_AGG("hok", ",")) also does not work
      #fix this later
      summarize(#ifbl_squares = paste(hok, collapse = '|'),
                ifbl_number_squares = n()) %>%
      ungroup()

    if (!isTRUE(collect)) {
      return(query_result)
    } else {
      query_result <- query_result %>%
        collect()
      return(query_result)
    }
  }

  glue_statement <- glue_sql(
    "SELECT DISTINCT
    tblIFBLHok.Code AS ifbl_1by1
    , SUBSTRING(tblIFBLHok.Code, 1, 5) AS ifbl_4by4
    , Year(tblWaarneming.BeginDatum) AS Jaar
    , relTaxonTaxon.TaxonIDParent
    , tblTaxon.Code AS Taxoncode
    FROM
    (((tblMeting INNER JOIN
      (tblIFBLHok INNER JOIN tblWaarneming
        ON tblIFBLHok.ID = tblWaarneming.IFBLHokID)
      ON tblMeting.WaarnemingID = tblWaarneming.ID)
    INNER JOIN relTaxonTaxon ON tblMeting.TaxonID = relTaxonTaxon.TaxonIDChild)
    INNER JOIN tblTaxon ON relTaxonTaxon.TaxonIDParent = tblTaxon.ID)
    INNER JOIN relTaxonTaxonGroep ON tblTaxon.ID = relTaxonTaxonGroep.TaxonID
    INNER JOIN tblTaxonGroep
      ON relTaxonTaxonGroep.TaxonGroepID = tblTaxonGroep.ID
    WHERE
    tblIFBLHok.Code LIKE '%-%-%' AND
    tblTaxon.Code NOT LIKE '%-sp' AND
    Year([tblWaarneming].[BeginDatum]) >={starting_year} AND
    (Year([tblWaarneming].[BeginDatum])=Year([tblWaarneming].[EindDatum])) AND
    (tblTaxonGroep.Naam={taxongroup}) AND
    (tblMeting.MetingStatusCode='GDGA' OR tblMeting.MetingStatusCode='GDGK')
    ORDER BY
    Year(tblWaarneming.BeginDatum) DESC OFFSET 0 ROWS",
    starting_year = starting_year,
    taxongroup = taxongroup,
    .con = connection)
  glue_statement <- iconv(glue_statement, from =  "UTF-8", to = "latin1")
  query_result <- tbl(connection, sql(glue_statement))
  if (!isTRUE(collect)) {
    return(query_result)
  } else {
    query_result <- query_result %>%
      collect()
    return(query_result)
  }
}
