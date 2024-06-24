#' @title Query to extract Taxonlist features from `D0156_00_Taxonlijsten`
#'
#' @description This function queries `D0156_00_Taxonlijsten` and gives an
#' overview of all the features associated with a taxonlist version (a red list
#' status or an annex of the Habitat Directive are examples of a feature). This
#' is an auxiliary function to check the accepted values (KenmerkwaardeCodes)
#' of the feature parameter in the core function `get_taxonlijsten_items`
#'
#' @inheritParams get_taxonlijsten_lists
#'
#' @return A remote tbl object (collect = FALSE) or a tibble dataframe (collect
#' = TRUE) with variables Taxonlijst, Publicatiejaar, Version, Kenmerkcode,
#' KenmerkBeschrijving, KenmerkwaardeCode, KenmerkwaardeBeschrijving
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
#' # get features of all versions of the 'Rode lijst van de Dagvlinders'
#' get_taxonlijsten_features(con, version = 'all', list = '%rode%dagvlinders%'
#' , collect = TRUE)
#'
#' # get features of Habitattypical fauna
#' get_taxonlijsten_features(con, list = '%Habitattyp%fauna%')
#'
#' # use function with default values (all features of recent versions)
#' get_taxonlijsten_features(con)
#'
#' # note that function also returns taxonlists without features
#' get_taxonlijsten_features(con, list = '%SBP%')
#'
#' # Close the connection when done
#' dbDisconnect(con)
#' rm(con)
#' }

get_taxonlijsten_features <- function(connection,
                                   list = "%",
                                   version = c("latest", "old", "all"),
                                   collect = FALSE
) {

  assert_that(is.character(list))

  version <- match.arg(version)

  whereclause <- "WHERE tmp.version = {version}"

  if (version == "all") {
    whereclause <- ""
  }

  assert_that(inherits(connection, what = "Microsoft SQL Server"),
              msg = "Not a connection object to database.")

  sql_statement <- glue_sql("SELECT * FROM
      (SELECT DISTINCT tl.Naam AS Taxonlijst
        , tlv.PublicatieJaar
		, v.Version
		, k.code AS KenmerkCode
	    , k.beschrijving AS KenmerkBeschrijving
    	, kw.code AS KenmerkwaardeCode
       	, kw.beschrijving AS KenmerkwaardeBeschrijving
FROM [dbo].[Taxonlijst] tl
       INNER JOIN [dbo].[TaxonlijstVersie] tlv ON tlv.TaxonlijstID = tl.ID
	   INNER JOIN [dbo].[TaxonlijstItem] tli ON tli.TaxonlijstVersieID = tlv.ID
	   INNER JOIN (SELECT tl.id
					, tlv.publicatiejaar
					, CASE WHEN ROW_NUMBER() OVER (PARTITION BY tl.id
					  ORDER BY PublicatieJaar DESC) = 1
						THEN 'latest' ELSE 'old' END AS Version
				   FROM [dbo].[Taxonlijst] tl
					 INNER JOIN [dbo].[TaxonlijstVersie] tlv ON tlv.TaxonlijstID = tl.ID)v
					 ON v.ID = tl.ID and v.PublicatieJaar = tlv.PublicatieJaar
	   LEFT JOIN [dbo].[TaxonlijstItemAttribuut] tlia
	   ON tlia.TaxonlijstItemID = tli.ID
	   LEFT JOIN [dbo].[Kenmerk] k ON k.id = tlia.KenmerkID
       LEFT JOIN [dbo].[Kenmerkwaarde] kw ON kw.ID = tlia.KenmerkwaardeID
	WHERE 1 = 1
       AND tl.Naam LIKE {list}
       )tmp ",
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
