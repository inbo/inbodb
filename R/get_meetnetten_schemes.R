#' @title Overview of monitoring schemes in the Meetnetten database
#'
#' @description This function queries the Meetnetten database to give an
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
#' @details The species monitoring programme of Flanders
#' (\href{www.meetnetten.be}{Meetnetten}) consists of a series of monitoring
#' schemes. In each monitoring scheme one or more target species are counted
#' based on a specific protocol. For more details we refer to Maes et al. (2023)
#'
#' @references
#' \itemize{
#' \item Maes D, Piesschaert F, Ledegen H, Van De Poel S, Adriaens T, Anselin A,
#' Belpaire C, Breine J, Brosens D, Brys R, De Bruyn L, Decleer K, De Knijf G,
#' Devos K, Driessens G, Feys S, Gouwy J, Gyselings R, Herremans M, Jacobs I,
#' Lewylle I, Leyssen A, Louette G, Onkelinx T, Packet J, Provoost S,
#' Quataert P, Ruyts S, Scheppers T, Speybroeck J, Steeman R, Stienen E,
#' Thomaes A, Van Den Berge K, Van Keer K, Van Landuyt W, Van Thuyne G,
#' Veraghtert W, Verbelen D, Verbeylen G, Vermeersch G, Westra T, Pollet M (2023)
#' Monitoring schemes for species of conservation concern in Flanders
#' (northern Belgium). An overview of established schemes and the design of an
#' additional monitoring scheme. Reports of the Research Institute for Nature and
#'  Forest (INBO) 2023 (15). Research Institute for Nature and Forest (INBO), Brussels.
#' \doi{10.21436/inbor.93332112}.
#' }
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
    ORDER BY pg.name, p.name, pr.name"

  query_result <- dbGetQuery(connection, sql_statement)
    collect() %>%
    arrange(.data$species_group, .data$scheme, .data$protocol)

  return(query_result)
}
