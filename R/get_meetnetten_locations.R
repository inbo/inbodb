#' @title Query monitoring scheme locations from meetnetten
#'
#' @description This function queries the meetnetten database for the locations
#' for a specified monitoring scheme or for all monitoring schemes within a
#' specified species group. When no monitoring scheme or species group is
#' specified, the observations of all monitoring schemes are returned.
#'
#' @param scheme_name the name of the monitoring scheme for which you want to
#' extract location data. Data from multiple schemes can be selected by providing
#' a vector with the names of the schemes.
#' @param species_group the name of the species group for which you want to
#' extract location data. Data from multiple species groups can be selected by
#' providing a vector with the names of the species groups.
#' @param connection dbconnection with the database 'S0008_00_Meetnetten'
#' on the inbo-sql08-prd.inbo.be server
#'
#' @return A list with two sf objects:
#'
#'\itemize{
#' \item main_locations: the main locations of the selected monitoring schemes
#' \item sublocations: the sublocations (for example the sections of a transect)
#' for each of the selected main locations
#' }
#'
#' Not all main locations are subdivided in sublocations.
#' So in some cases the sublocations object is empty.
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter arrange collect tbl sql mutate select
#' @importFrom stringr str_to_lower
#' @importFrom sf st_as_sf st_drop_geometry
#'
#' @export
#' @family meetnetten
#' @examples
#' \dontrun{
#' library(inbodb)
#' con <- connect_inbo_dbase("S0008_00_Meetnetten")
#'
#' # get locations for a specific monitoring scheme
#' locations_treefrog <- get_meetnetten_locations(con,
#' scheme_name = "Heivlinder")
#'
#' main_locations <- locations_heivlinder$main_locations
#' sublocations <- locations_heivlinder$sublocations
#'
#' # get locations for a specific species_group
#' locations_dragonflies <- get_meetnetten_locations(con,
#' species_group = "libellen")
#'
#' main_locations <- locations_dragonflies$main_locations
#' sublocations <- locations_dragonflies$sublocations
#'
#' # Close the connection when done
#' dbDisconnect(con)
#' rm(con)
#' }

get_meetnetten_locations <- function(connection,
                                    scheme_name = NULL,
                                    species_group = NULL) {

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

  sql_statement <- "SELECT
    PG.name as species_group,
    P.name as scheme,
    L.name as location,
    L.id,
    L.parent_id,
    PL.is_active,
    L.geom.STAsText() as geom
    FROM staging_meetnetten.locations_location L
    INNER JOIN staging_meetnetten.projects_projectlocation PL ON PL.location_id = L.id
    INNER JOIN staging_meetnetten.projects_project P ON P.id = PL.project_id
    INNER JOIN staging_meetnetten.projects_projectgroup PG on PG.id = P.group_id"

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

  locations_sf <- query_result %>%
    collect() %>%
    mutate(location_type = ifelse(.data$parent_id == -1,
                                  "main_location",
                                  "sublocation")) %>%
    st_as_sf(wkt = "geom", crs = 4326)

  locations_main <- locations_sf %>%
    filter(.data$location_type == "main_location") %>%
    arrange(.data$species_group, .data$scheme, .data$location)

  locations_main_names <- locations_main %>%
    st_drop_geometry() %>%
    select(scheme, location, location_id = id)

  locations_sublocations <- locations_sf %>%
    filter(.data$location_type == "sublocation") %>%
    rename(sublocation = location, location_id = parent_id) %>%
    left_join(locations_main_names, by = c("scheme", "location_id")) %>%
    arrange(.data$species_group, .data$scheme, .data$location,
            .data$sublocation) %>%
    filter(!is.na(.data$location))

  locations_main <- locations_main %>%
    select(species_group, scheme, location, is_active)

  locations_sublocations <- locations_sublocations %>%
    select(species_group, scheme, location, sublocation, is_active)

  result <- list(main_locations = locations_main,
                 sublocations = locations_sublocations)

  return(result)

}
