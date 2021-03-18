globalVariables("%LIKE%")

#' Query the florabank to get taxon trait values for (a) taxon trait(s)
#'
#' This function takes as input (part of) a taxon trait name, queries the florabank
#' and returns the taxon trait values in a tidy data format
#'
#' @param connection A connection to the florabank database. See the example section
#' for how to connect and disconnect to the database.
#'
#' @param trait_name A (part of) a trait name for which you want to get the
#' associated taxon-specific trait values. If this is missing, the function
#' returns an error and prints a message showing all possible trait names.
#'
#' @param collect If FALSE (the default), a remote tbl object is returned. This
#' is like a reference to the result of the query but the full result of the
#' query is not brought into memory. If TRUE the full result of the query is
#' collected (fetched) from the database and brought into memory of the working
#' environment.
#'
#' @return A remote tbl object (collect = FALSE) or a tibble dataframe (collect
#' = TRUE) containing the trait values for each species and for all
#' partially matched traits. The dataframe contains the variables TaxonID,
#' TaxonAfkorting, TaxonWetenschappelijk, TaxonNederlands, Kenmerk, Code,
#' Omschrijving, Rekenwaarde, Bron and ExtraOmschrijving.
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
#' library(DBI)
#' library(odbc)
#' library(dplyr)
#' # connect to florabank
#' db_connectie <- connect_inbo_dbase("D0021_00_userFlora")
#'
#' # get all Ellenberg values via partial matching, return as lazy query
#' fb_ellenberg <- get_florabank_traits(db_connectie, "llenberg")
#' # collect the data
#' fb_ellenberg <- fb_ellenberg %>% collect()
#' # the same can be done by using the collect parameter
#' fb_ellenberg <- get_florabank_traits(db_connectie, "llenberg", collect = TRUE)
#'
#' # get all red lists via partial matching
#' fb_rodelijsten <- get_florabank_traits(db_connectie, "rode")
#'
#' # get only the red list for vascular plant species
#' fb_rodelijstvaatplanten <- get_florabank_traits(db_connectie, "Rode lijst Vaatplanten")
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
  assert_that(connection@info$dbname == "D0021_00_userFlora")

  if (missing(trait_name)) {
    traitnames <- tbl(connection, "tblTaxonKenmerk") %>%
      distinct(.data$Naam) %>%
      collect() %>%
      pull(.data$Naam)
    message <- paste0("Please provide (part of) a trait name from this list: ",
                      paste(traitnames, collapse = ", "))
    options(warning.length = nchar(message))
    stop(message)
  }

  trait_name <- tolower(trait_name)

  fb_taxon <- tbl(connection, "tblTaxon")
  fb_taxon_kenmerk <- tbl(connection, "tblTaxonKenmerk")
  fb_taxon_kenmerk_waarde <- tbl(connection, "tblTaxonKenmerkWaarde")
  rel_taxon_taxon_kenmerk_waarde <- tbl(connection, "relTaxonTaxonKenmerkWaarde")

  query_result <- rel_taxon_taxon_kenmerk_waarde %>%
    inner_join(fb_taxon_kenmerk %>%
                 filter(tolower(.data$Naam) %LIKE%
                          paste0("%", trait_name, "%")) %>%
                 select(.data$ID, .data$Naam, .data$Bron),
               by = c("TaxonKenmerkID" = "ID")) %>%
    rename(ExtraOmschrijving = .data$Omschrijving) %>%
    left_join(fb_taxon_kenmerk_waarde %>%
                distinct(.data$ID, .data$Code, .data$TaxonKenmerkID,
                         .data$Omschrijving, .data$Rekenwaarde),
              by = c("TaxonKenmerkID" = "TaxonKenmerkID",
                     "TaxonKenmerkWaardeID" = "ID")) %>%
    left_join(fb_taxon %>%
                rename(NaamAfkorting = .data$Code),
              by = c("TaxonID" = "ID")) %>%
    distinct(.data$TaxonID,
             TaxonAfkorting = .data$NaamAfkorting,
             TaxonWetenschappelijk = .data$NaamWetenschappelijk,
             TaxonNederlands = .data$NaamNederlands,
             Kenmerk = .data$Naam,
             .data$Code,
             .data$Omschrijving,
             .data$Rekenwaarde,
             .data$Bron,
             .data$ExtraOmschrijving
    )
  if (!isTRUE(collect)) {
    return(query_result)
  } else {
    query_result <- query_result %>%
      collect()
    return(query_result)
  }
}
