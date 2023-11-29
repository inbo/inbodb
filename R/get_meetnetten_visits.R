#' @title Query visit data from meetnetten
#'
#' @description This function queries the meetnetten database for visit
#' data (data about the counting process) for a specified monitoring scheme or
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
#' @param collect If FALSE (the default), a remote tbl object is returned. This
#' is like a reference to the result of the query but the full result of the
#' query is not brought into memory. If TRUE the full result of the query is
#' collected (fetched) from the database and brought into memory of the working
#' environment.
#'
#' @return A remote tbl object (collect = FALSE) or a tibble dataframe (collect
#' = TRUE) with variables pecies_group, scheme, protocol, location, visit_id,
#' validation_status, start_date, start_time, end_date, end_time, date_created,
#' visit_status, for_analysis, for_targets, notes.
#'
#' @importFrom glue glue_sql
#' @importFrom DBI dbGetQuery
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
#' # get visits for a specific monitoring scheme and collect data
#' visits_treefrog <- get_meetnetten_visist(con, scheme_name = "Boomkikker",
#' collect = TRUE)
#'
#' # get visits for a specific species_group and collect data
#' visits_amphibians <- get_meetnetten_visist(con, species_group = "amfibieën",
#' collect = TRUE)
#'
#' # get visits for all species and do not collect data
#' visits_all <- get_meetnetten_visist(con)
#'
#' # Close the connection when done
#' dbDisconnect(con)
#' rm(con)
#' }

get_meetnetten_visits <- function(connection,
                               scheme_name = "%",
                               species_group = "%",
                               collect = FALSE) {

  assert_that(inherits(connection, what = "Microsoft SQL Server"),
              msg = "Not a connection object to database.")

  assert_that(is.character(scheme_name))
  assert_that(is.character(species_group))

  sql_statement <- glue_sql(
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
    WHERE 1 = 1
    AND p.name LIKE {scheme_name}
    AND pg.name LIKE {species_group}",
    scheme_name = scheme_name,
    species_group = species_group,
    .con = connection)

  query_result <- tbl(connection, sql(sql_statement))

  if (!isTRUE(collect)) {
    return(query_result)
  } else {
    query_result <- collect(query_result)
    return(query_result)
  }
}
