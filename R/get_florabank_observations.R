#' Get all validated observations for one or more taxa from the florabank
#' database
#'
#' This function takes as input a character vector with one or more names of
#' species either as scientific names and/or Dutch names. By default (fixed =
#' FALSE), partial matching will be used (the names are prepended with appended
#' with %).
#' The function queries the florabank, and returns a dataframe with observation
#' level information about the matching taxa.
#'
#' @param connection A connection to the florabank database. See the example
#' section for how to connect and disconnect to the database.
#'
#' @param names Default missing. A character vector with scientific names
#' and/or Dutch names. If fixed = TRUE, character strings are matched exactly
#' and scientific names must include authorship in order to match.
#'
#' @param fixed Logical. If TRUE, names is to be matched as is (no partial
#' matching)
#' .
#' @param collect If FALSE (the default), a remote tbl object is returned. This
#' is like a reference to the result of the query but the full result of the
#' query is not brought into memory. If TRUE the full result of the query is
#' collected (fetched) from the database and brought into memory of the working
#' environment.
#'
#' @return A dataframe with the following variables: "NaamNederlands",
#' "NaamWetenschappelijk", "Bron", "BeginDatum", "EindDatum", "hok",
#' "Toponiem", "CommentaarTaxon", "CommentaarHabitat",
#' "WaarnemingID", "X_waarneming", "Y_waarneming", "X_meting", "Y_meting"
#'
#' @importFrom glue glue_sql
#' @importFrom assertthat assert_that
#' @importFrom dplyr
#' tbl
#' collect
#' sql
#'
#' @export
#' @family florabank
#' @examples
#' \dontrun{
#' # code can only be run if a connection to the database is possible
#' library(inbodb)
#' library(DBI)
#' library(odbc)
#' # connect to florabank
#' db_connectie <- connect_inbo_dbase("D0021_00_userFlora")
#'
#' # query and collect the data using scientific name
#' succprat1 <-	get_florabank_observations(db_connectie,
#' names = 'Succisa pratensis Moench', collect = TRUE)
#'
#' # the same species but using Dutch name
#' succprat2 <-	get_florabank_observations(db_connectie,
#' names = 'Blauwe knoop', collect = TRUE)
#'
#' # providing both a Dutch name and scientific name will not duplicate records
#' # if they are the same species
#' succprat3 <- get_florabank_observations(db_connectie,
#' names = c("Succisa pratensis Moench", "Blauwe knoop"), collect = TRUE)
#'
#' all.equal(succprat1, succprat2)
#' all.equal(succprat1, succprat3)
#'
#' # passing dutch names and scientific names for different species
#' # is possible (records for each species is returned)
#' myspecies1 <- get_florabank_observations(db_connectie,
#' names = c('Succisa pratensis Moench', 'Gevlekte orchis'), collect = TRUE)
#'
#' # passing multiple dutch names
#' myspecies2 <- get_florabank_observations(db_connectie,
#' names = c('Gevlekte orchis', 'Blauwe knoop'),
#' collect = TRUE)
#'
#' all.equal(myspecies1, myspecies2)
#'
#' # using default for collect will return a lazy query
#' # fixed = TRUE for exact matches only
#' myspecies3 <-	get_florabank_observations(db_connectie,
#' names = c('Succisa pratensis Moench', 'Gevlekte orchis'),
#' fixed = TRUE)
#'
#' # to collect the data for a lazy query you can also use the collect()
#' # function:
#' myspecies3 <- dplyr::collect(myspecies3)
#'
#' # disconnect from florabank
#' dbDisconnect(db_connectie)
#' }

get_florabank_observations <- function(connection, names, fixed = FALSE,
                                   collect = FALSE) {

  assert_that(inherits(connection, what = "Microsoft SQL Server"),
              msg = "Not a connection object to database.")
  assert_that(connection@info$dbname == "D0021_00_userFlora")

  if (missing(names)) {
    stop("Please provide names.")
  }

  sql_statement <- "SELECT DISTINCT
  tblTaxon.NaamNederlands
  , tblTaxon.NaamWetenschappelijk
  , cdeBron.Omschrijving AS Bron
  , tblWaarneming.BeginDatum
  , tblWaarneming.EindDatum
  , tblIFBLHok.Code AS hok
  , tblWaarneming.Opmerking AS Toponiem
  , tblMeting.CommentaarTaxon
  , tblMeting.CommentaarHabitat
  , tblWaarneming.ID AS WaarnemingID
  , tblWaarneming.Cor_X AS X_waarneming
  , tblWaarneming.Cor_Y AS Y_waarneming
  , tblMeting.Cor_X AS X_meting
  , tblMeting.Cor_Y AS Y_meting
  FROM dbo.tblWaarneming
  INNER JOIN dbo.tblMeting ON tblWaarneming.ID = tblMeting.WaarnemingID
  INNER JOIN dbo.relTaxonTaxon ON relTaxonTaxon.TaxonIDChild = tblMeting.TaxonID
  INNER JOIN dbo.tblTaxon ON tblTaxon.ID = relTaxonTaxon.TaxonIDParent
  LEFT JOIN dbo.tblIFBLHok ON tblIFBLHok.ID = tblWaarneming.IFBLHokID
  INNER JOIN dbo.cdeBron ON cdeBron.Code = tblWaarneming.BronCode
  WHERE 1=1
  AND (tblMeting.MetingStatusCode='GDGA' OR tblMeting.MetingStatusCode='GDGK')
  "

  if (!fixed) {
    like_string <-
      paste0("AND (",
             paste0(
               c(paste0("tblTaxon.NaamNederlands", " LIKE ", "'%", names, "%'"),
                 paste0("tblTaxon.NaamWetenschappelijk", " LIKE ", "'%", names,
                        "%'")),
               collapse = " OR "),
             ")")
    sql_statement <- glue_sql(
      sql_statement,
      like_string,
      .con = connection)
  } else {
    sql_statement <- glue_sql(
      sql_statement,
      "AND (tblTaxon.NaamWetenschappelijk IN ({names*}) OR
             tblTaxon.NaamNederlands IN ({names*}))
             ",
      names = names,
      .con = connection)
  }

  sql_statement <- glue_sql(
    sql_statement,
    "ORDER BY tblWaarneming.BeginDatum DESC OFFSET 0 ROWS",
    .con = connection)

  sql_statement <- iconv(sql_statement, from =  "UTF-8", to = "latin1")

  query_result <- tbl(connection, sql(sql_statement))
  if (!isTRUE(collect)) {
    return(query_result)
  } else {
    query_result <- query_result %>%
      collect()
    return(query_result)
  }
}
