#' Get unique combinations of taxon, IFBL-square and year.
#'
#' This functions queries all validated observations of the florabank database
#' and returns unique combinations of taxon, `IFBL`-square and year.
#' Either a 1 km
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
#' 1km-by-1km `IFBL` squares or 4km-by-4km. Default is 1km-by-1km.
#'
#' @param taxongroup Choose for which taxonomic group you want the unique
#' combinations. One of `"Vaatplanten"` (the default), `"Mossen"`,
#' `"Lichenen (korstmossen)"`
#' or `"Kranswieren"`.
#'
#' @param collect If FALSE (the default), a remote `tbl` object is returned.
#' This
#' is like a reference to the result of the query but the full result of the
#' query is not brought into memory. If TRUE the full result of the query is
#' collected (fetched) from the database and brought into memory of the working
#' environment.
#'
#' @return A dataframe with one line for each combination of taxon,
#' `IFBL`-square
#' (either at 1 km x 1 km or 4 km x 4 km resolution) and year. In case the
#' resolution is 1 km x 1 km, a variable `ifbl_4by4` gives the corresponding
#' `ifbl_4by4` identifier within which the `ifbl_1by1` square is located.
#' In case
#' the resolution is 4 km x 4 km, the variable `ifbl_squares` is a concatenation
#' of all nested squares with observations for the taxon in the corresponding
#' year. This can be nested 1 x 1 squares as well as the corresponding 4 x 4
#' square (the latter is the case if the original resolution of the observation
#' is at 4 x 4 resolution). In addition, the variable `ifbl_number_squares`
#' gives
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
#' # connect to florabank
#' db_connectie <- connect_inbo_dbase("D0152_00_Flora")
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
  assert_that(connection@info$dbname == "D0152_00_Flora")

  assert_that(is.numeric(starting_year))
  assert_that(starting_year <= as.numeric(format(Sys.Date(), "%Y")))

  ifbl_resolution <- match.arg(ifbl_resolution)
  taxongroup <- match.arg(taxongroup)



  if (ifbl_resolution == "4km-by-4km") {
    glue_statement <- glue_sql(
      ";WITH cte AS
(
SELECT t.id AS taxonid
	, t.code AS taxoncode
	, t.NaamNederlands
	, t.NaamWetenschappelijk
	, t.TaxonGroepID
	, CASE WHEN t.ParentTaxonID IS NULL OR t.TaxonRelatieTypeID = 1
	THEN t.id ELSE t.ParentTaxonID END AS ParentTaxonID
	, CASE WHEN t.ParentTaxonID IS NULL OR t.TaxonRelatieTypeID = 1
	THEN t.code ELSE tp.code END AS ParentTaxoncode
	, CASE WHEN t.ParentTaxonID IS NULL OR t.TaxonRelatieTypeID = 1
	THEN t.NaamNederlands ELSE tp.NaamNederlands END AS ParentNaamNederlands
	, CASE WHEN t.ParentTaxonID IS NULL OR t.TaxonRelatieTypeID = 1
	THEN t.NaamWetenschappelijk ELSE tp.NaamWetenschappelijk END AS ParentNaamWetenschappelijk
FROM Taxon t
	LEFT JOIN Taxon tp ON tp.id = t.ParentTaxonID
)

SELECT DISTINCT h.Code AS hok
	, CASE WHEN tmp.code IS NULL THEN h.code ELSE tmp.Code END AS ifbl_4by4
	, DATEPART(year, e.BeginDatum) AS jaar
	, cte.ParentTaxonID
	, cte.ParentTaxoncode
	, cte.ParentNaamWetenschappelijk
	, cte.ParentNaamNederlands
FROM [event] e
	INNER JOIN Hok h ON h.ID = e.HokID
	INNER JOIN Waarneming w ON w.EventID = e.ID
	INNER JOIN waarnemingstatus ws ON ws.id = w.WaarnemingStatusID
	LEFT JOIN (SELECT HokIDChild
					, h.Code
				FROM Hok_Hok hh
					INNER JOIN HokRelatieType hrt ON hrt.ID = hh.HokRelatieTypeID
					INNER JOIN Hok h ON h.ID = hh.HokIDParent
				WHERE hrt.Code = 'DV'
				)tmp ON tmp.HokIDChild = e.hokid
	INNER JOIN cte ON cte.taxonid = w.TaxonID
	INNER JOIN TaxonGroep tg ON tg.ID = cte.TaxonGroepID
WHERE 1=1
	AND cte.ParentTaxoncode NOT LIKE '%-sp'
	AND DATEPART(year, e.BeginDatum) >= {starting_year}
	AND DATEPART(year, e.BeginDatum) = DATEPART(year, e.EindDatum)
	AND tg.Beschrijving = {taxongroup}
	AND ws.code IN ('GDGA','GDGK')
ORDER BY DATEPART(year, e.BeginDatum) desc",
      starting_year = starting_year,
      taxongroup = taxongroup,
      .con = connection)
    glue_statement <- iconv(glue_statement, from =  "UTF-8", to = "latin1")
    query_result <- tbl(connection, sql(glue_statement))

    query_result <- query_result %>%
      group_by(.data$ifbl_4by4, .data$jaar, .data$ParentTaxonID,
               .data$ParentTaxoncode, .data$ParentNaamWetenschappelijk,
               .data$ParentNaamNederlands) %>%
      #paste with collapse does not translate to sql
      #str_flatten() is not available for Microsoft SQL Server
      #sql(STRING_AGG("hok", ",")) also does not work
      #fix this later
      summarize(
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
    ";WITH cte AS
(
SELECT t.id AS taxonid
	, t.code AS taxoncode
	, t.NaamNederlands
	, t.NaamWetenschappelijk
	, t.TaxonGroepID
	, CASE WHEN t.ParentTaxonID IS NULL OR t.TaxonRelatieTypeID = 1
	THEN t.id ELSE t.ParentTaxonID END AS ParentTaxonID
	, CASE WHEN t.ParentTaxonID IS NULL OR t.TaxonRelatieTypeID = 1
	THEN t.code ELSE tp.code END AS ParentTaxoncode
	, CASE WHEN t.ParentTaxonID IS NULL OR t.TaxonRelatieTypeID = 1
	THEN t.NaamNederlands ELSE tp.NaamNederlands END AS ParentNaamNederlands
	, CASE WHEN t.ParentTaxonID IS NULL OR t.TaxonRelatieTypeID = 1
	THEN t.NaamWetenschappelijk ELSE tp.NaamWetenschappelijk END
	AS ParentNaamWetenschappelijk
FROM Taxon t
	LEFT JOIN Taxon tp ON tp.id = t.ParentTaxonID
)

SELECT DISTINCT h.Code AS hok
	, tmp.code AS ifbl_4by4
	, DATEPART(year, e.BeginDatum) AS jaar
	, cte.ParentTaxonID
	, cte.ParentTaxoncode
	, cte.ParentNaamWetenschappelijk
	, cte.ParentNaamNederlands
FROM [event] e
	INNER JOIN Hok h ON h.ID = e.HokID
	INNER JOIN Waarneming w ON w.EventID = e.ID
	INNER JOIN waarnemingstatus ws ON ws.id = w.WaarnemingStatusID
	INNER JOIN (SELECT HokIDChild
					, h.Code
				FROM Hok_Hok hh
					INNER JOIN HokRelatieType hrt ON hrt.ID = hh.HokRelatieTypeID
					INNER JOIN Hok h ON h.ID = hh.HokIDParent
				WHERE hrt.Code = 'DV')tmp ON tmp.HokIDChild = e.hokid
	INNER JOIN cte ON cte.taxonid = w.TaxonID
	INNER JOIN TaxonGroep tg ON tg.ID = cte.TaxonGroepID
WHERE 1=1
	AND cte.ParentTaxoncode NOT LIKE '%-sp'
	AND DATEPART(year, e.BeginDatum) >= {starting_year}
	AND DATEPART(year, e.BeginDatum) = DATEPART(year, e.EindDatum)
	AND tg.Beschrijving = {taxongroup}
	AND ws.code IN ('GDGA','GDGK')
ORDER BY DATEPART(year, e.BeginDatum) desc",
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
