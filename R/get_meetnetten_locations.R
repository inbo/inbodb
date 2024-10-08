#' @title Query monitoring scheme locations from Meetnetten
#'
#' @description This function queries the Meetnetten database for the locations
#' and sublocations for a specified monitoring scheme or for all monitoring
#' schemes within a specified species group.
#' When no monitoring scheme or species group is specified, the observations of
#' all monitoring schemes are returned.
#'
#' @param scheme_name the name of the monitoring scheme for which you want to
#' extract location data. Data from multiple schemes can be selected by
#' providing a vector with the names of the schemes.
#' @param species_group the name of the species group for which you want to
#' extract location data. Data from multiple species groups can be selected by
#' providing a vector with the names of the species groups.
#' @param connection dbconnection with the database 'S0008_00_Meetnetten'
#' on the inbo-sql08-prd.inbo.be server
#'
#' @return When the \code{sf} package is installed, a list with two \code{sf}
#' objects is returned:
#'
#' \itemize{
#'  \item \code{main_locations}: the main locations of the
#'    selected monitoring schemes, with following attribute variables:
#'    \itemize{
#'    \item \code{species_group}
#'    \item \code{scheme}: name of the monitoring scheme
#'    \item \code{location}: name of the location
#'    \item \code{is_sample}: whether the location belongs to the sample of
#'    locations for the monitoring scheme (see details)
#'    \item \code{is_active}: when a location is not suited for counting any
#'    more, the location becomes inactive (\code{is_active} = \code{FALSE})
#'    }
#' \item \code{sublocations}: the sublocations (for example
#' the sections of a transect) for each of the selected main locations, with
#' following attribute variables:
#'    \itemize{
#'    \item \code{species_group}
#'    \item \code{scheme}: name of the monitoring scheme
#'    \item \code{location}: name of the main location
#'    \item \code{sublocation}: name of the sublocation
#'    \item \code{is_active}: whether the sublocation is counted or not
#'    }
#' }
#'
#' When the \code{sf} package is not installed, a list with two
#' \code{tibble} objects is returned, with the same attribute variables as above
#' and an additional variable \code{geom} that contains the geometry information
#' in \code{wkt} (well-known text) format.
#'
#' Not all main locations are subdivided in sublocations.
#' So in some cases the sublocations object is empty.
#'
#' @details
#' Each monitoring scheme of the species monitoring programme of Flanders
#' \href{https://www.meetnetten.be}{Meetnetten} consists of a fixed set of
#' locations.
#' A monitoring scheme for rare species includes all locations where the species
#' occurs.
#' For more common species a sample of locations is drawn and the the selected
#' locations are included in the monitoring scheme.
#' In some cases, the monitoring project in
#' \href{https://www.meetnetten.be}{Meetnetten} also contains locations that are
#' not part of the sample. These locations can be counted optionally and are
#' indicated by (\code{is_sample} = \code{FALSE}).
#'
#' It also occurs that a location becomes inaccessible or that the target
#' species disappears.
#' Then, a locations can be made inactive (\code{is_active} = \code{FALSE}),
#' which means that no observations can be recorded any more.
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter arrange collect tbl sql mutate select %>% left_join
#' rename
#' @importFrom rlang .data
#'
#' @export
#' @family meetnetten
#' @examples
#' \dontrun{
#' library(inbodb)
#' con <- connect_inbo_dbase("S0008_00_Meetnetten")
#'
#' # get locations for a specific monitoring scheme
#' locations_heivlinder <- get_meetnetten_locations(con,
#'                                                  scheme_name = "Heivlinder")
#'
#' locations_heivlinder$main_locations
#' locations_heivlinder$sublocations
#'
#' # get locations for a specific species_group
#' locations_dragonflies <- get_meetnetten_locations(con,
#'                                                   species_group = "libellen")
#'
#' locations_dragonflies$main_locations
#' locations_dragonflies$sublocations
#'
#' # Close the connection when done
#' dbDisconnect(con)
#' rm(con)
#' rm(locations_heivlinder)
#' rm(locations_dragonflies)
#' }

get_meetnetten_locations <- function(connection,
                                    scheme_name = NULL,
                                    species_group = NULL) {

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

  sql_statement <- "SELECT
    PG.name as species_group,
    P.name as scheme,
    L.name as location,
    L.id,
    L.parent_id,
    PL.is_sample,
    PL.is_active,
    L.geom.STAsText() as geom
    FROM staging_meetnetten.locations_location L
    INNER JOIN staging_meetnetten.projects_projectlocation PL
    ON PL.location_id = L.id
    INNER JOIN staging_meetnetten.projects_project P
    ON P.id = PL.project_id
    INNER JOIN staging_meetnetten.projects_projectgroup PG
    ON PG.id = P.group_id"

  query_result <- tbl(connection, sql(sql_statement))

  if (!is.null(scheme_name)) {
    if (!is.null(species_group_selected)) {
      query_result <- query_result %>%
        filter(tolower(.data$scheme) %in% scheme_name |
                 tolower(.data$species_group) %in% species_group_selected)
    } else {
      query_result <- query_result %>%
        filter(tolower(.data$scheme) %in% scheme_name)
    }
  } else if (!is.null(species_group_selected)) {
    query_result <- query_result %>%
      filter(tolower(.data$species_group) %in% species_group_selected)
  }

  locations <- query_result %>%
    collect() %>%
    mutate(location_type = ifelse(.data$parent_id == -1,
                                  "main_location",
                                  "sublocation"))

  locations_main <- locations %>%
    filter(.data$location_type == "main_location") %>%
    arrange(.data$species_group, .data$scheme, .data$location)

  locations_main_names <- locations_main %>%
    select(.data$scheme, .data$location, location_id = .data$id)

  locations_sublocations <- locations %>%
    filter(.data$location_type == "sublocation") %>%
    rename(sublocation = .data$location, location_id = .data$parent_id) %>%
    left_join(locations_main_names, by = c("scheme", "location_id")) %>%
    arrange(.data$species_group, .data$scheme, .data$location,
            .data$sublocation) %>%
    filter(!is.na(.data$location))

  locations_main <- locations_main %>%
    select(.data$species_group, .data$scheme, .data$location, .data$is_sample,
          .data$is_active, .data$geom)

  locations_sublocations <- locations_sublocations %>%
    select(.data$species_group, .data$scheme, .data$location, .data$sublocation,
           .data$is_active, .data$geom)

  if (requireNamespace("sf", quietly = TRUE)) {

    locations_main <- locations_main %>%
      sf::st_as_sf(wkt = "geom", crs = 4326)

    locations_sublocations <- locations_sublocations %>%
      sf::st_as_sf(wkt = "geom", crs = 4326)

  }

  result <- list(main_locations = locations_main,
                 sublocations = locations_sublocations)

  return(result)

}
