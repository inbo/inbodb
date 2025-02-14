#' Get all validated observations for one or more taxa from the florabank
#' database
#'
#' This function takes as input a character vector with one or more names of
#' species either as scientific names and/or Dutch names. By default (fixed =
#' FALSE), partial matching will be used (the names are prepended and appended
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
#' @param collect If FALSE (the default), a remote `tbl` object is returned.
#' This
#' is like a reference to the result of the query but the full result of the
#' query is not brought into memory. If TRUE the full result of the query is
#' collected (fetched) from the database and brought into memory of the working
#' environment.
#'
#' @return A dataframe with the following variables:
#' `NaamNederlands`,
#' `NaamWetenschappelijk`,
#' `AcceptedNaamWetenschappelijk`,
#' `Bron`,
#' `BeginDatum`,
#' `EindDatum`,
#' `Hok`,
#' `Toponiem`,
#' `CommentaarEvent`,
#' `CommentaarWaarneming`,
#' `EventID`,
#' `X_event`,
#' `Y_event`,
#' `X_waarneming`,
#' `Y_waarneming`
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
#' # connect to florabank
#' db_connectie <- connect_inbo_dbase("D0152_00_Flora")
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
  assert_that(connection@info$dbname == "D0152_00_Flora")

  if (missing(names)) {
    stop("Please provide names.")
  }

  sql_statement <- "SELECT DISTINCT COALESCE(cte.ParentNaamNederlands
  , cte.NaamNederlands) as NaamNederlands
	, cte.NaamWetenschappelijk
	, cte.ParentNaamWetenschappelijk AS AcceptedNaamWetenschappelijk
	, b.Beschrijving AS Bron
	, e.BeginDatum
	, e.EindDatum
	, h.Code AS Hok
	, e.Toponiem
	, e.Opmerking AS CommentaarEvent
	, w.Commentaar AS CommentaarWaarneming
	, e.id AS EventID
	, e.CorX AS X_event
	, e.CorY AS Y_event
	, w.CorX AS X_waarneming
	, w.CorY AS Y_waarneming
FROM [event] e
	INNER JOIN Bron b ON b.ID = e.BronID
	INNER JOIN Waarneming w ON w.EventID = e.ID
	INNER JOIN waarnemingstatus ws ON ws.id = w.WaarnemingStatusID
	INNER JOIN Hok h ON h.ID = e.HokID
	INNER JOIN (SELECT t.id AS taxonid
					, t.code AS taxoncode
					, t.NaamNederlands
					, t.NaamWetenschappelijk
					, CASE WHEN t.ParentTaxonID IS NULL OR t.TaxonRelatieTypeID = 1
					THEN t.id ELSE t.ParentTaxonID END AS ParentTaxonID
					, CASE WHEN t.ParentTaxonID IS NULL OR t.TaxonRelatieTypeID = 1
					THEN t.code ELSE tp.code END AS ParentTaxoncode
					, CASE WHEN t.ParentTaxonID IS NULL OR t.TaxonRelatieTypeID = 1
					THEN t.NaamNederlands ELSE tp.NaamNederlands
					END AS ParentNaamNederlands
					, CASE WHEN t.ParentTaxonID IS NULL OR t.TaxonRelatieTypeID = 1
					THEN t.NaamWetenschappelijk ELSE tp.NaamWetenschappelijk
					END AS ParentNaamWetenschappelijk
				FROM Taxon t
					LEFT JOIN Taxon tp ON tp.id = t.ParentTaxonID)cte
					ON cte.taxonid = w.TaxonID
WHERE 1=1
	AND ws.Code in ('GDGA','GDGK')
  "

  if (!fixed) {
    like_string <-
      paste0("AND cte.ParentTaxonID in
		(SELECT DISTINCT CASE WHEN t.ParentTaxonID IS NULL
		OR t.TaxonRelatieTypeID = 1 THEN t.id ELSE t.ParentTaxonID
		END AS ParentTaxonID
		 FROM Taxon t
			LEFT JOIN Taxon tp ON tp.id = t.ParentTaxonID
		 WHERE 1=1
		 	AND (",
             paste0(
               c(paste0("t.NaamNederlands", " LIKE ", "'%", names, "%'"),
                 paste0("t.NaamWetenschappelijk", " LIKE ", "'%", names,
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
