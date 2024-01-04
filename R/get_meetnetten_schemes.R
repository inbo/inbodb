#' @title Overview of monitoring schemes in meetnetten database
#'
#' @description This function queries the meetnetten database to give an
#' overview of monitoring schemes that are included.
#'
#' @param connection dbconnection with the database 'S0008_00_Meetnetten'
#' on the inbo-sql08-prd.inbo.be server
#'
#' @return A tibble dataframe with variables species_group, scheme and protocol.
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr collect tbl sql
#'
#' @export
#' @family meetnetten
#' @examples
#' \dontrun{
#' library(inbodb)
#' con <- connect_inbo_dbase("S0008_00_Meetnetten")
#'
#' # get overview of monitoring schemes in meetnetten database
#' meetnetten_schemes <- get_meetnetten_schemes(con)
#'
#' # Close the connection when done
#' dbDisconnect(con)
#' rm(con)
#' }

get_meetnetten_schemes <- function(connection) {

  assert_that(inherits(connection, what = "Microsoft SQL Server"),
              msg = "Not a connection object to database.")

  sql_statement <-
    "SELECT
    pg.name AS species_group
    , p.name AS scheme
    , pr.name AS protocol
    FROM staging_meetnetten.projects_project p
    INNER JOIN staging_meetnetten.projects_projectgroup pg ON pg.id = p.group_id
    INNER JOIN staging_meetnetten.projects_project_protocols ppp ON ppp.project_id = p.id
    INNER JOIN staging_meetnetten.protocols_protocol PR ON PR.id = ppp.protocol_id
    WHERE 1 = 1"

  query_result <- tbl(connection, sql(sql_statement)) %>%
    collect() %>%
    arrange(.data$species_group, .data$scheme, .data$protocol)

  return(query_result)
}
