#' @title Query observation data from meetnetten
#'
#' @description This function queries the meetnetten database for observation
#' data (standardized counts) for a specified monitoring scheme or
#' for all monitoring schemes within a specified species group. When no
#' monitoring scheme or species group is specified, the observations of all
#' monitoring schemes are returned.
#'
#' @param scheme_name the name of the monitoring scheme for which you want to
#' extract visit data. Data from multiple schemes can be selected by providing
#' a vector with the names of the schemes.
#' @param species_group the name of the species group for which you want to
#' extract visit data. Data from multiple species groups can be selected by
#' providing a vector with the names of the species groups.
#' @param connection dbconnection with the database 'S0008_00_Meetnetten'
#' on the inbo-sql08-prd.inbo.be server
#' @param collect If FALSE (the default), a remote tbl object is returned. This
#' is like a reference to the result of the query but the full result of the
#' query is not brought into memory. If TRUE the full result of the query is
#' collected (fetched) from the database and brought into memory of the working
#' environment.
#'
#' @return A remote tbl object (collect = FALSE) or a tibble dataframe (collect
#' = TRUE) with variables species_group, scheme, protocol, visit_id, start_date,
#' location, checklist_complete, sublocation, not_counted, sample_id,
#' target_species, name_nl, scientific_name, sex, activity, life_stage, count,
#' count_type,notes, x, y
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter arrange collect tbl sql
#' @importFrom stringr str_to_lower
#'
#' @export
#' @family meetnetten
#' @examples
#' \dontrun{
#' library(inbodb)
#' con <- connect_inbo_dbase("S0008_00_Meetnetten")
#'
#' # get observations for a specific monitoring scheme and collect data
#' observations_treefrog <- get_meetnetten_observation(con,
#' scheme_name = "Boomkikker", collect = TRUE)
#'
#' # get observations for a specific species_group and collect data
#' observations_dragonflies <- get_meetnetten_observations(con,
#' species_group = "libellen", collect = TRUE)
#'
#' # get observations for all species and do not collect data
#' observations_all <- get_meetnetten_observations(con)
#'
#' # Close the connection when done
#' dbDisconnect(con)
#' rm(con)
#' }

get_meetnetten_observations <- function(connection,
                               scheme_name = NULL,
                               species_group = NULL,
                               collect = FALSE) {

  species_group_selected <- species_group

  assert_that(inherits(connection, what = "Microsoft SQL Server"),
              msg = "Not a connection object to database.")

  if (!is.null(scheme_name)) {
    assert_that(is.character(scheme_name))
    scheme_name <- str_to_lower(scheme_name)
  }

  if (!is.null(species_group_selected)) {
    assert_that(is.character(species_group_selected))
    species_group_selected <- str_to_lower(species_group_selected)
  }

  sql_statement <-
    "SELECT
    PG.name AS species_group
    , P.name AS scheme
    , PR.name AS protocol
    , V.id AS visit_id
    , V.start_date
    , L.name AS location
    , SA.is_complete AS checklist_complete
    , L1.name AS sublocation
    , SA.Not_Counted AS not_counted
    , O.sample_id
    , PS.is_primary AS target_species
    , SP.name AS name_nl
    , SP.scientific_name
    , O.sex
    , ACT.name AS activity
    , LS.name AS life_stage
    , O.number_min AS count
    , PSC.name AS count_type
    , SA.notes
    , o.geom.STX AS x
    , o.geom.STY AS y
    from staging_meetnetten.Projects_project P
    INNER JOIN staging_meetnetten.projects_projectgroup PG
      ON PG.id = P.group_id
    INNER JOIN staging_meetnetten.fieldwork_visit V ON V.project_ID = P.ID
    LEFT OUTER JOIN staging_meetnetten.protocols_protocol PR
      ON PR.id = V.protocol_id
    LEFT OUTER JOIN staging_meetnetten.fieldwork_sample SA ON SA.visit_ID = V.ID
    LEFT OUTER JOIN staging_meetnetten.fieldwork_observation O
      ON O.sample_ID = SA.ID
    LEFT OUTER JOIN staging_meetnetten.protocols_scale PSC
      ON O.scale_ID = PSC.ID
    LEFT OUTER JOIN staging_meetnetten.Species_species SP
      ON SP.ID = O.species_id
    LEFT OUTER JOIN staging_meetnetten.species_activity ACT
      ON ACT.id = O.activity_id
    LEFT OUTER JOIN staging_meetnetten.species_lifestage LS
      ON LS.id = O.life_stage_id
    LEFT OUTER JOIN staging_meetnetten.Locations_location L
      ON L.ID = V.location_ID
    LEFT OUTER JOIN staging_meetnetten.Locations_location L1
      ON L1.ID = SA.location_id
    LEFT OUTER JOIN staging_meetnetten.projects_projectspecies PS
      ON PS.species_ID = O.species_ID and PS.project_ID = P.ID
    where 1=1
    and SA.notes <> 'dummy sample'
    and V.validation_status <> -1"

  query_result <- tbl(connection, sql(sql_statement))

  if (!is.null(scheme_name) & !is.null(species_group_selected)) {

    query_result <- query_result %>%
      filter(str_to_lower(.data$scheme) %in% scheme_name |
               str_to_lower(.data$species_group) %in% species_group_selected)

  } else if (!is.null(scheme_name)) {

    query_result <- query_result %>%
      filter(str_to_lower(.data$scheme) %in% scheme_name)

  } else if (!is.null(species_group_selected)) {

    query_result <- query_result %>%
      filter(str_to_lower(.data$species_group) %in% species_group_selected)

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
