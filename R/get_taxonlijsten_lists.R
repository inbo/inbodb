#' @title Query to extract Taxonlijsten from `D0156_00_Taxonlijsten`
#'
#' @description This function queries `D0156_00_Taxonlijsten` and gives an
#' overview of all the taxon lists and list versions currently available in the
#' database. Only the latest version is shown unless specified otherwise
#'
#' @param connection dbconnection with the database `D0156_00_Taxonlijsten`
#' on the inbo-sql07-prd server
#' @param list name of the taxonlist that you want to retrieve. Wildcards %
#' are allowed. Case insensitive.
#' @param version A choice ('latest', 'old', 'all'). If 'latest' (the default)
#' only the most recent version is returned. If 'old' all but the most recent
#' version is returned. If 'all' all versions are returned.
#' @param collect If FALSE (the default), a remote tbl object is returned. This
#' is like a reference to the result of the query but the full result of the
#' query is not brought into memory. If TRUE the full result of the query is
#' collected (fetched) from the database and brought into memory of the working
#' environment.
#'
#' @return A remote tbl object (collect = FALSE) or a tibble dataframe (collect
#' = TRUE) with variables TaxonlijstType, TaxonlijstCode, Taxonlijst,
#' Publicatiejaar, Version, ReferentieURL, Criteria, Validering, Vaststelling.
#'
#' @importFrom glue glue_sql
#' @importFrom DBI dbGetQuery
#' @importFrom assertthat assert_that
#' @importFrom dplyr collect tbl sql
#'
#' @export
#' @family taxonlijsten
#' @examples
#' \dontrun{
#' library(inbodb)
#' con <- connect_inbo_dbase("D0156_00_Taxonlijsten")
#'
#' # get the most recent version of the 'Rode lijst van de Dagvlinders'
#' get_taxonlijsten_lists(con, version = 'latest', list =
#' '%rode%dagvlinders%', collect = FALSE)
#'
#' # get all recent red lists
#' get_taxonlijsten_lists(con, list = '%rode lijst%')
#'
#' # get all taxonlist versions in the database
#' get_taxonlijsten_lists(con, version = 'all', collect = TRUE)
#'
#' # use function with default values (only most recent versions)
#' get_taxonlijsten_lists(con)
#'
#' # status of red lists
#' rl <- get_taxonlijsten_lists(con, list = '%rode lijst%')
#' select(rl,"Taxonlijst", "PublicatieJaar", "Criteria", "Validering",
#' "Vaststelling")
#'
#' # Close the connection when done
#' dbDisconnect(con)
#' rm(con, rl)
#' }

get_taxonlijsten_lists <- function(connection,
                                   list = "%",
                                   version = c("latest", "old", "all"),
                                   collect = FALSE
) {

  assert_that(is.character(list))

  assert_that(is.logical(collect))

  version <- match.arg(version)

  whereclause <- "WHERE tmp.version = {version}"

  if (version == "all") {
    whereclause <- ""
  }

  assert_that(inherits(connection, what = "Microsoft SQL Server"),
              msg = "Not a connection object to database.")

  sql_statement <- glue_sql("SELECT * FROM
      (SELECT tlt.code AS TaxonlijstType
    , tl.code AS TaxonlijstCode
    , tl.Naam AS Taxonlijst
    , tlv.PublicatieJaar
    , CASE WHEN ROW_NUMBER() OVER
    (PARTITION BY tl.id ORDER BY PublicatieJaar DESC) = 1
    THEN 'latest' ELSE 'old' END AS Version
    , tlv.ReferentieURL
    , c.Beschrijving AS Criteria
    , vl.Beschrijving AS Validering
    , vs.Beschrijving AS Vaststelling
  FROM [dbo].[Taxonlijst] tl
    INNER JOIN [dbo].[TaxonlijstVersie] tlv ON tlv.TaxonlijstID = tl.ID
    LEFT JOIN [dbo].[TaxonlijstType] tlt ON tlt.ID = tl.TaxonlijstTypeID
    LEFT JOIN [dbo].[Criteria] c ON c.ID = tlv.CriteriaID
    LEFT JOIN [dbo].[Validering] vl ON vl.ID = tlv.ValideringID
    LEFT JOIN [dbo].[Vaststelling] vs ON vs.ID = tlv.VaststellingID
    WHERE 1 = 1
    AND tl.Naam LIKE {list})tmp ",
                             whereclause,
                             list = list,
                             version = version,
                             .con = connection
  )

  query_result <- tbl(connection, sql(sql_statement))

  if (!isTRUE(collect)) {
    return(query_result)
  } else {
    query_result <- collect(query_result)
    return(query_result)
  }
}
