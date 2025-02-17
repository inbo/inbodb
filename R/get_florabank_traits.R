globalVariables("%LIKE%")

#' Query the florabank to get taxon trait values for (a) taxon trait(s)
#'
#' This function takes as input (part of) a taxon trait name, queries the
#' florabank and returns the taxon trait values in a tidy data format
#'
#' @param connection A connection to the florabank database. See the example
#' section for how to connect and disconnect to the database.
#'
#' @param trait_name A (part of) a trait name for which you want to get the
#' associated taxon-specific trait values. If this is missing, the function
#' returns an error and prints a message showing all possible trait names.
#'
#' @param collect If FALSE (the default), a remote `tbl` object is returned.
#' This
#' is like a reference to the result of the query but the full result of the
#' query is not brought into memory. If TRUE the full result of the query is
#' collected (fetched) from the database and brought into memory of the working
#' environment.
#'
#' @return A remote `tbl` object (collect = FALSE) or a `tibble` dataframe
#' (collect = TRUE) containing the trait values for each species and for all
#' partially matched traits. The dataframe contains the variables
#' `TaxonID`,
#' `TaxonAfkorting`,
#' `TaxonWetenschappelijk`,
#' `TaxonNederlands`,
#' `Kenmerk`,
#' `Code`,
#' `Omschrijving`,
#' `Rekenwaarde`,
#' `Bron` and
#' `ExtraOmschrijving`.
#' The first four variables identify the taxon, the latter five variables relate
#' to the taxon traits.
#'
#' @importFrom dplyr
#' tbl
#' collect
#' distinct
#' pull
#' %>%
#' inner_join
#' left_join
#' filter
#' select
#' rename
#' @importFrom rlang .data
#' @importFrom assertthat assert_that
#'
#' @export
#' @family florabank
#' @examples
#' \dontrun{
#' library(inbodb)
#' library(dplyr)
#' # connect to florabank
#' db_connectie <- connect_inbo_dbase("D0152_00_Flora")
#'
#' # get all Ellenberg values via partial matching, return as lazy query
#' fb_ellenberg <- get_florabank_traits(db_connectie, "llenberg")
#' # collect the data
#' fb_ellenberg <- fb_ellenberg %>% collect()
#' # the same can be done by using the collect parameter
#' fb_ellenberg <-
#'   get_florabank_traits(db_connectie, "llenberg", collect = TRUE)
#'
#' # get all red lists via partial matching
#' fb_rodelijsten <- get_florabank_traits(db_connectie, "rode")
#'
#' # get only the red list for vascular plant species
#' fb_rodelijstvaatplanten <-
#'   get_florabank_traits(db_connectie, "Rode lijst Vaatplanten")
#'
#' #if the trait_name argument is missing, a list of possible names is printed
#' get_florabank_traits(db_connectie)
#'
#' #disconnect from florabank
#' dbDisconnect(db_connectie)
#' }

get_florabank_traits <- function(connection, trait_name, collect = FALSE) {

  assert_that(inherits(connection, what = "Microsoft SQL Server"),
              msg = "Not a connection object to database.")
  assert_that(connection@info$dbname == "D0152_00_Flora")

  if (missing(trait_name)) {
    traitnames <- tbl(connection, "Kenmerk") %>%
      distinct(.data$Beschrijving) %>%
      collect() %>%
      pull(.data$Beschrijving)
    message <- paste0("Please provide (part of) a trait name from this list: ",
                      paste(traitnames, collapse = ", "))
    options(warning.length = nchar(message))
    stop(message)
  }

  sql_statement <- "
    SELECT t.ID AS TaxonID
    	, t.Code AS TaxonCode
    	, t.NaamWetenschappelijk
    	, t.NaamNederlands
    	, k.Kenmerk
    	, kc.KenmerkCode
    	, kc.naam AS Omschrijving
    	, kc.Rekenwaarde
    	, k.KenmerkBron AS Bron
    	, tk.Beschrijving AS ExtraOmschrijving
    FROM [dbo].[Taxon_KenmerkWaarde] tk
    	inner join Taxon t on t.ID = tk.TaxonID
    	inner join TaxonGroep tg on tg.ID = t.TaxonGroepID
    	left join vw.vw_kenmerk k on k.kenmerkid = tk.KenmerkID
    	left join vw.vw_kenmerkcategorie kc on
    	kc.kenmerkcategorieid = tk.KenmerkCategorieID
    WHERE 1 = 1"

  like_string <- paste0(" AND k.kenmerk like '%", trait_name, "%'")

  sql_statement <- glue_sql(
    sql_statement,
    like_string,
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
