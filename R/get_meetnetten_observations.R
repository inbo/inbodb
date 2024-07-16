#' @title Query observation data from Meetnetten
#'
#' @description This function queries the Meetnetten database for observation
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
#' @param collect If \code{FALSE} (the default), a remote \code{tbl} object is
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
#'    \item \code{scheme}
#'    \item \code{protocol}: the protocol used
#'    \item \code{visit_id}: unique id for a count event
#'    \item \code{start_date}:date of the observation
#'    \item \code{location}: the name of the location
#'    \item \code{sublocation}: the name of the sublocation
#'    \item \code{not_counted}: \code{TRUE} when the sublocation is not counted
#'    \item \code{sample_id}: unique id for a count subevent (see details)
#'    \item \code{target_species}: \code{TRUE} when the observed species is the
#'    target species, \code{FALSE} when the observed species is a secondary
#'    species (another species than the target species that can be counted with
#'    the same protocol, see details)
#'    \item \code{checklist_complete}: whether all secondary species, defined
#'    in the monitoring scheme, are counted
#'    \item \code{name_nl}: Dutch name of the observed species
#'    \item \code{scientific_name}: scientific name of the observed species
#'    \item \code{sex}: M (male), F (female), U (undefined)
#'    \item \code{activity}: activity of the observed species
#'    \item \code{life_stage}: live stage of the observed species
#'    \item \code{count}: number of individuals counted
#'    \item \code{count_type}: most of the time the number of individuals are
#'    counted (\code{count_type} = \code{exact count}), however for some
#'    monitoring schemes different type of counts are performed. Check the
#'    protocol for more information when this is the case.
#'    \item \code{notes}: notes of the observed
#'    \item \code{x} and \code{y}: when the Meetnetten-app is used,
#'    GPS coordinates (longitude and latitude, \code{crs} = \code{WGS84}) of
#'    each observation is recorded
#'    }
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter arrange collect tbl sql %>%
#' @importFrom rlang .data
#'
#' @details The species monitoring programme of Flanders
#' (\href{www.meetnetten.be}{Meetnetten}) consists of a series of monitoring
#' schemes in which one or more target species are counted based on a specific
#' protocol. Optionally, other species, that can be counted using the same
#' protocol, can be recorded as well. When \code{checklist_complete} = \code{TRUE},
#' all secondary species were counted, and we can assume that the secondary
#' species that were not recorded are absent.
#'
#' Depending on the protocol, counting has to be done at the location or
#' the sublocation level. Sublocations are, for example, different sections
#' of a transect. For some monitoring schemes, it is necessary to record
#' several count subevents at the location level. This is, for example,
#' the case for the crested newt fyke count protocol, where two fykes are used
#' per location and the counts are recorded per fyke. For every count subevent
#' a unique \code{sample_id} is created.
#'
#' The protocol of a monitoring scheme also defines for which combinations of
#' sex, life stage, and activity type the counts have to be recorded. For
#' example, for the crested newt fyke counts the number of female adults, male
#' adults and juveniles (sex undefined) are counted. Another example:
#' in the alcon blue monitoring scheme only the number of eggs are counted.
#'
#' It is also important to know that counts can be recorded in the
#' \href{www.meetnetten.be}{Meetnetten} website or by using the Meetnetten app.
#' When using the Meetnetten app, the GPS coordinates of all observations are
#' recorded and the observations are assigned to a location or sublocation
#' based on the coordinates. For example, when you record a butterfly transect
#' count in the website, you will enter the total number of indiviudals per
#' species for each section (the sublocation) of the transect. When you use the
#' app, you can record the position of every individual separately in the
#' Meetnetten database. So when you want to know the total number of individuals
#' per section, you will have to aggregate the data.
#'
#' To conclude, it is important to understand how the data is organised for a
#' certain monitoring scheme, before you start analysing the data.
#' For more details on the monitoring schemes we refer to Maes et al. (2023)
#'
#' @references
#' \itemize{
#' \item Maes D, Piesschaert F, Ledegen H, Van De Poel S, Adriaens T, Anselin A,
#' Belpaire C, Breine J, Brosens D, Brys R, De Bruyn L, Decleer K, De Knijf G,
#' Devos K, Driessens G, Feys S, Gouwy J, Gyselings R, Herremans M, Jacobs I,
#' Lewylle I, Leyssen A, Louette G, Onkelinx T, Packet J, Provoost S,
#' Quataert P, Ruyts S, Scheppers T, Speybroeck J, Steeman R, Stienen E,
#' Thomaes A, Van Den Berge K, Van Keer K, Van Landuyt W, Van Thuyne G,
#' Veraghtert W, Verbelen D, Verbeylen G, Vermeersch G, Westra T,
#' Pollet M (2023). Monitoring schemes for species of conservation concern in
#' Flanders (northern Belgium). An overview of established schemes and the
#' design of an additional monitoring scheme. Reports of the Research Institute
#' for Nature and Forest (INBO) 2023 (15). Research Institute for Nature and
#' Forest (INBO), Brussels. \doi{10.21436/inbor.93332112}.
#' }
#'
#' @export
#' @family meetnetten
#' @examples
#' \dontrun{
#' library(inbodb)
#' con <- connect_inbo_dbase("S0008_00_Meetnetten")
#'
#' # get observations for a specific monitoring scheme and collect data
#' observations_treefrog <- get_meetnetten_observations(con,
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
    PG.name AS species_group
    , P.name AS scheme
    , PR.name AS protocol
    , V.id AS visit_id
    , V.start_date
    , L.name AS location
    , L1.name AS sublocation
    , SA.Not_Counted AS not_counted
    , O.sample_id
    , PS.is_primary AS target_species
    , SA.is_complete AS checklist_complete
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

  if (!is.null(scheme_name)) {

    if (!is.null(species_group_selected)) {

      query_result <- query_result %>%
        filter(to_lower(.data$scheme) %in% scheme_name |
                 to_lower(.data$species_group) %in% species_group_selected)

    } else {

      query_result <- query_result %>%
        filter(to_lower(.data$scheme) %in% scheme_name)

    }
    else {

      if (!is.null(species_group_selected)) {

        query_result <- query_result %>%
          filter(to_lower(.data$species_group) %in% species_group_selected)

      }
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
