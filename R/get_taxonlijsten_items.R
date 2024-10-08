#' @title Query to extract the taxa on a taxonlist from `D0156_00_Taxonlijsten`
#'
#' @description This function queries `D0156_00_Taxonlijsten` and gives an
#' overview of the taxa that are on a given taxon list version. The interpreted
#' taxa are given by default, but it is possible to add taxa as they were
#' originally published. The taxa of the latest list version are shown
#' unless specified otherwise.
#'
#' @inheritParams get_taxonlijsten_lists
#' @param taxon name of the taxon you want to retrieve. Scientific and
#' vernacular (Dutch) names are allowed. Wildcards % are allowed.
#' Case insensitive.
#' @param feature name of the list feature (actually feature code) you want to
#' retrieve. Wildcards % are allowed. Case insensitive.
#' @param original If FALSE (the default), the function will only retrieve the
#' interpreted taxa. If TRUE, columns with the original taxa will be added
#' to the output. For example, if the originally published taxon on a taxonlist
#' is 'Cicindela spec.', the interpretation will exist of all relevant
#' Cicindela species
#'
#' @return A remote tbl object (collect = FALSE) or a tibble dataframe (collect
#' = TRUE) with variables Lijst, Publicatiejaar, LaatsteVersie, Taxongroep,
#' Naamwet_interpretatie, Auteur, NaamNed_interpretatie, Kenmerk,
#' KenmerkwaardeCode, Kenmerkwaarde and extra variables Taxongroep_origineel,
#' Naamwet_origineel, Naamned_origineel when requested (original = TRUE)
#'
#' @importFrom glue glue_sql
#' @importFrom DBI dbGetQuery
#' @importFrom assertthat assert_that
#' @importFrom dplyr collect tbl sql select
#'
#' @export
#' @family taxonlijsten
#' @examples
#' \dontrun{
#' library(inbodb)
#' library(tidyverse)
#' con <- connect_inbo_dbase("D0156_00_Taxonlijsten")
#'
#' # Get all taxa from list 'Jachtdecreet'
#' get_taxonlijsten_items(con, list =  'Jachtdecreet', collect = TRUE)
#'
#' # Get all taxa on category 2 of 'Soortenbesluit'
#' get_taxonlijsten_items(con, list =  'soortenbesluit', feature = 'cat2')
#'
#' # Get all taxonlist that include 'Gentiaanblauwtje'
#' get_taxonlijsten_items(con, taxon = 'Gentiaanblauwtje', collect = TRUE)
#'
#' # Get all taxa with red list status CR (critically endangered)
#' get_taxonlijsten_items(con, feature = 'CR')
#'
#' # Get original and interpreted Cicindela taxa from list 'Soortenbesluit'
#' get_taxonlijsten_items(con, list = 'Soortenbesluit', taxon = '%Cicindela%'
#' , original = TRUE) %>%
#' select('Naamwet_origineel', 'NaamNed_origineel', 'Naamwet_interpretatie'
#' , 'NaamNed_interpretatie')
#'
#' # Compare red list status on multiple listversions
#' get_taxonlijsten_items(con, version = 'all'
#' , list = 'rode lijst van de dagvlinders') %>%
#' select('Lijst', 'Publicatiejaar', 'Naamwet_interpretatie'
#' , 'NaamNed_interpretatie', 'KenmerkwaardeCode') %>%
#' pivot_wider(names_from = Publicatiejaar, values_from = KenmerkwaardeCode)
#'
#' # Close the connection when done
#' dbDisconnect(con)
#' rm(con)
#' }

get_taxonlijsten_items <- function(connection,
                                       list = "%",
                                       taxon = "%",
                                       feature = "%",
                                       version = c("latest", "old", "all"),
                                       original = FALSE,
                                       collect = FALSE
) {

  assert_that(is.character(list))

  assert_that(is.character(taxon))

  assert_that(is.character(feature))

  assert_that(is.logical(original))

  assert_that(is.logical(collect))

  version <- match.arg(version)

  whereclause <- "AND LaatsteVersie = {version}"

  if (version == "latest") {
    version <- 1
  } else if (version == "old") {
    version <- 0
  } else {
    whereclause <- ""
  }

  if (isTRUE(original)) {
    original <- ", Taxonlijstgroep  as Taxongroep_origineel
	, NaamWet as Naamwet_origineel
	, NaamNed as NaamNed_origineel"
  } else {
    original <- ""
  }

  assert_that(inherits(connection, what = "Microsoft SQL Server"),
              msg = "Not a connection object to database.")

  sql_statement <- glue_sql("SELECT Lijst
	, Publicatiejaar
	, LaatsteVersie
	, Taxongroep
	, Naamwet_interpretatie
	, Auteur
	, NaamNed_interpretatie
	, Kenmerk
	, KenmerkwaardeCode
	, Kenmerkwaarde ",
                             original,
                             " FROM [dbo].[vw_Taxonlijstitem_detail]
    WHERE 1 = 1
    AND lijst LIKE {list}
    AND (Naamwet_interpretatie LIKE {taxon} OR
    Naamned_interpretatie LIKE {taxon})
    AND KenmerkwaardeCode LIKE {feature}",
                             whereclause,
                             list = list,
                             taxon = taxon,
                             feature = feature,
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
