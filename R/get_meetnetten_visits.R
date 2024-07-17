#' @title Query visit data from Meetnetten
#'
#' @description This function queries the meetnetten database for visit
#' data (data about a counting event) for a specified monitoring scheme or
#' for all monitoring schemes within a specified species group. When no
#' monitoring scheme or species group is specified, the visits of all monitoring
#' schemes are returned.
#'
#' @param scheme_name the name of the monitoring scheme for which you want to
#' extract visit data.
#' @param species_group the name of the species group for which you want to
#' extract visit data.
#' @param connection dbconnection with the database 'S0008_00_Meetnetten'
#' on the inbo-sql08-prd.inbo.be server
#' @param collect If \code{FALSE} (the default), a remote tbl object is
#' returned. This is like a reference to the result of the query but the full
#' result of the query is not brought into memory. If \code{TRUE} the full
#' result of the query is collected (fetched) from the database and brought
#' into memory of the working environment.
#'
#' @return A remote \code{tbl} object (\code{collect} = \code{FALSE}) or a
#' \code{tibble} dataframe (\code{collect} = \code{TRUE}) with following
#' variables:
#' \itemize{
#'    \item \code{species_group}
#'    \item \code{scheme}: the name of the monitoring scheme
#'    \item \code{protocol}: the protocol used
#'    \item \code{location}: the name of the location
#'    \item \code{visit_id}: unique id for a count event
#'    \item \code{validation_status}: validation status of the visit (visits
#'    that are validated and not approved are not provided)
#'    \itemize{
#'        \item \code{10}: visit not validated
#'        \item \code{100}: visit validated and approved
#'        }
#'    \item \code{start_date}: the start date of the visit
#'    \item \code{start_time}: the start time of the visit
#'    \item \code{end_date}: the end date of the visit
#'    \item \code{end_time}: the end time of the visit
#'    \item \code{date_created}: the date at which the data was imported in the
#'    database
#'    \item \code{visit_status}: the status of the visit (determined by
#'    the observer) using following categories:
#'    \itemize{
#'        \item \code{conform protocol}: the protocol was applied
#'        \item \code{weersomstandigheden waren ongunstig}: weather conditions
#'        were unfavourable
#'        \item \code{telmethode uit handleiding niet gevolgd}: the protocol was
#'        not applied
#'        \item \code{geen veldwerk mogelijk - locatie ontoegankelijk}: counting
#'        was not possible because the location is inaccessible
#'        \item \code{geen veldwerk mogelijk - locatie is ongeschikt voor de soort}:
#'        counting was not possible because the location is not suitable
#'        for the species
#'        }
#'    \item \code{for_analysis}: whether the data is suited for analysis
#'    (determined by the validator)
#'    \item \code{for_targets}: every year targets are set in terms of the
#'    number of locations that have to be counted per monitoring scheme; when
#'    \code{for_targets} = \code{TRUE} the visit contributes to these targets
#'    \item \code{notes}: notes by the observer
#' }
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr collect tbl sql %>% filter arrange
#' @importFrom rlang .data
#'
#' @export
#' @family meetnetten
#' @examples
#' \dontrun{
#' library(inbodb)
#' con <- connect_inbo_dbase("S0008_00_Meetnetten")
#'
#' # get visits for a specific monitoring scheme and collect data
#' get_meetnetten_visits(con, scheme_name = "Boomkikker", collect = TRUE)
#'
#' # get visits for a specific species_group and collect data
#' get_meetnetten_visits(con, species_group = "libellen", collect = TRUE)
#'
#' # get visits for all species and do not collect data
#' visits_all <- get_meetnetten_visits(con)
#'
#' # Close the connection when done
#' dbDisconnect(con)
#' rm(con)
#' rm(visits_all)
#' }

get_meetnetten_visits <- function(connection,
                               scheme_name = NULL,
                               species_group = NULL,
                               collect = FALSE) {

  species_group_selected <- species_group

  assert_that(inherits(connection, what = "Microsoft SQL Server"),
              msg = "Not a connection object to database.")

  if (!is.null(scheme_name)) {
    assert_that(is.character(scheme_name),
                length(scheme_name) > 0,
                noNA(scheme_name))
    scheme_name <- tolower(scheme_name)
  }

  if (!is.null(species_group_selected)) {
    assert_that(is.character(species_group_selected),
                length(species_group_selected) > 0,
                noNA(species_group_selected))
    species_group_selected <- tolower(species_group_selected)
  }

  sql_statement <-
    "SELECT
    pg.name AS species_group
    , p.name AS scheme
    , pr.name AS protocol
    , l.name AS location
    , v.id AS visit_id
    , v.validation_status
    , v.start_date
    , v.start_time
    , v.end_date
    , v.end_time
    , v.created AS date_created
    , case when v.status = 1 then 'Conform protocol'
           when v.status = -1 then 'Weersomstandigheden ongunstig'
           when v.status = -2 then 'Telmethode niet gevolgd'
           when v.status = -3 then 'Geen veldwerk mogelijk - locatie ontoegankelijk'
           when v.status = -4 then 'Geen veldwerk mogelijk - locatie ongeschikt'
           else Null
      end AS visit_status
    , v.analysis AS for_analysis
    , v.year_target AS for_targets
    , v.notes
    FROM staging_meetnetten.projects_project p
    INNER JOIN staging_meetnetten.projects_projectgroup pg ON pg.id = p.group_id
    INNER JOIN staging_meetnetten.fieldwork_visit v ON v.project_id = P.id
    INNER JOIN staging_meetnetten.Locations_location l ON l.id = v.location_id
    INNER JOIN staging_meetnetten.protocols_protocol PR ON PR.id = v.protocol_id
    WHERE 1 = 1"

  query_result <- tbl(connection, sql(sql_statement))

  if (!is.null(scheme_name)) {

    if (!is.null(species_group_selected)) {

      query_result <- query_result %>%
        filter(str_to_lower(.data$scheme) %in% scheme_name |
                 str_to_lower(.data$species_group) %in% species_group_selected)

    } else {

      query_result <- query_result %>%
        filter(str_to_lower(.data$scheme) %in% scheme_name)
    }
  }else {

    if (!is.null(species_group_selected)) {

      query_result <- query_result %>%
        filter(str_to_lower(.data$species_group) %in% species_group_selected)

    }
  }

  query_result <- query_result %>%
    arrange(.data$species_group, .data$scheme, .data$start_date, .data$location)

  if (!isTRUE(collect)) {
    return(query_result)
  } else {
    query_result <- collect(query_result)
    return(query_result)
  }
}
