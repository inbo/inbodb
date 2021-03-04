#' @title Query layer - qualifier information of recordings (releve) from INBOVEG
#'
#' @description This function queries the INBOVEG database for layer
#' qualifier information on recordings  for one or more surveys.
#'
#' @param survey_name A character string or a character vector, depending on
#' multiple parameter, giving the name or names of the
#' survey(s) for which you want to extract recordings information. If missing, all
#' surveys are returned.
--#' @param qualifier_type A character vector giving the name of qualifier type for which
--#' you want to extract  information e.g. 'SQ' (site qualifier), 'MQ' (management qualifier).
--#' If missing, all qualifier types are returned.
#' @param connection dbconnection with the database 'Cydonia'
#' on the inbo-sql07-prd server
#' @param multiple If TRUE, survey_name can take a character vector with
#' multiple survey names that must match exactly. If FALSE (the default),
#' survey_name must be a single character string (one survey name) that can
#' include wildcards to allow partial matches
#'
#' @return A dataframe with variables RecordingGivid (unique Id), UserReference,
#' Observer, LayerCode, Description, QualifierCode, 2nd Description, Elucidation, in case
#' qualifier is 'NotSure
#'
#' @importFrom glue glue_sql
#' @importFrom DBI dbGetQuery
#' @importFrom assertthat assert_that
#'
#' @export
#' @family inboveg
#' @examples
#' \dontrun{
#' library(inbodb)
#' library(DBI)
#' library(odbc)
#' library(dplyr)
#' con <- connect_inbo_dbase("D0010_00_Cydonia")
#'
#' # get the layer qualifiers from one survey
#' qualifiers_heischraal2012 <- get_inboveg_layer(con, survey_name =
#' "MILKLIM_Heischraal2012")
#'
#' # get all layer qualifiers from MILKLIM surveys (partial matching)
#' qualifiers_milkim <- get_inboveg_layer(con, survey_name = "%MILKLIM%",
#' qualifier_type = "SQ")
#'
#' # get layer qualifiers from several specific surveys
#' qualifiers_severalsurveys <- get_inboveg_layer(con, survey_name =
#' c("MILKLIM_Heischraal2012", "NICHE Vlaanderen"), multiple = TRUE)
#'
#' # get all layer qualifiers of all surveys
#' allqualifiers <- get_inboveg_layer(con)
#'
#' # Close the connection when done
#' dbDisconnect(con)
#' rm(con)
#' }
#'


get_inboveg_layer <- function(connection,
                                   survey_name,
                                   multiple = FALSE) {

  assert_that(inherits(connection, what = "Microsoft SQL Server"),
              msg = "Not a connection object to database.")

  if (missing(survey_name) & !multiple) {
    survey_name <- "%"
  }

  if (missing(survey_name) & multiple) {
    stop("Please provide one or more survey names to survey_name when multiple
         = TRUE")
  }

  if (!missing(survey_name)) {
    if (!multiple) {
      assert_that(is.character(survey_name))
    } else {
      assert_that(is.vector(survey_name, mode = "character"))
    }
  }

common_part <- "SELECT ivRecording.RecordingGivid
  , ivRecording.UserReference
  , ivRecording.Observer
  , ivRLLayer.LayerCode
  , qry_01ACvalues_1.oms
  , ivRLQualifier.QualifierCode
  , qry_01ACvalues.oms
  , ivRLQualifier.Elucidation
  , ivRLQualifier.NotSure
FROM ivRLLayer
RIGHT JOIN ivRecording ON ivRLLayer.RecordingID = ivRecording.Id
LEFT JOIN ivRLQualifier ON ivRLLayer.ID = ivRLQualifier.LayerID
LEFT JOIN (SELECT ivRLResources.ResourceGIVID
    , ivRLResources.ActionGroup
    , ivRLResources.ListName
    , ftActionGroupList.ListGIVID
    , qry_00ActionGroups.Code
    , qry_00ActionGroups.oms
    FROM (ivRLResources
    LEFT JOIN ftActionGroupList ON (ivRLResources.ListName = ftActionGroupList.ListName)
    AND (ivRLResources.ActionGroup = ftActionGroupList.ActionGroup))
    LEFT JOIN (select [Code]
    , [Description] as oms
    , [ListGIVID]
    from [ftQualifierValues] ---- > [syno].[Futon_dbo_ftActionGroupValues] ftACV
  ORDER BY  [ListGIVID],[Code]
  UNION select [Code], [Description] as oms, [ListGIVID] from [ftDQualifierValues]
  ORDER BY  [ListGIVID],[Code]
  UNION select [Code], [Description] as oms, [ListGIVID] from [ftAbiotiekValues]
  ORDER BY  [ListGIVID],[Code]
  UNION select [Code], [Description] as oms, [ListGIVID] from [ftBWKValues]
  ORDER BY  [ListGIVID],[Code]
  UNION select [Code], [PctValue] as oms, [ListGIVID] from [ftCoverValues]
  ORDER BY  [ListGIVID],[Code]
  UNION select [Code], [Description] as oms, [ListGIVID] from [ftFenoValues]
  ORDER BY  [ListGIVID],[Code]
  UNION select [Code], [Description] as oms, [ListGIVID] from [ftgebiedValues]
  ORDER BY  [ListGIVID],[Code]
  UNION select [Code], "-" as oms, [ListGIVID] from [ftGHCValues]
  ORDER BY  [ListGIVID],[Code]
  UNION select [Code], [Description] as oms, [ListGIVID] from [ftLayerValues]
  ORDER BY  [ListGIVID],[Code]
  UNION select [Code], [Description] as oms, [ListGIVID] from [ftLFValues]
  ORDER BY  [ListGIVID],[Code]
  UNION select [Code], [Description] as oms, [ListGIVID] from [ftMngmtValues]
  ORDER BY  [ListGIVID],[Code]
  UNION select [Code], [Description] as oms, [ListGIVID] from [ftN2kValues]
  ORDER BY  [ListGIVID],[Code]
  UNION select [Code], [Description] as oms, [ListGIVID] from [ftPatchValues]
  ORDER BY  [ListGIVID],[Code]
  UNION select [Code], [Description] as oms, [ListGIVID] from [ftSociaValues]
  ORDER BY  [ListGIVID],[Code]
  UNION select [Code], [Description] as oms, [ListGIVID] from [ftSoilValues]
  ORDER BY  [ListGIVID],[Code]
  UNION select [Code], [Description] as oms, [ListGIVID] from [ftVitaValues]
  ORDER BY [ListGIVID], [Code]) qry_00ActionGroups ON ftActionGroupList.ListGIVID = qry_00ActionGroups.ListGIVID
    ORDER BY ftActionGroupList.ListGIVID, qry_00ActionGroups.Code;
    ) qry_01ACvalues ON ivRLQualifier.QualifierCode = qry_01ACvalues.Code
    AND ivRLQualifier.QualifierResource = qry_01ACvalues.ResourceGIVID
LEFT JOIN (SELECT ivRLResources.ResourceGIVID
, ivRLResources.ActionGroup
, ivRLResources.ListName
, ftActionGroupList.ListGIVID
, qry_00ActionGroups.Code
, qry_00ActionGroups.oms
FROM (ivRLResources
LEFT JOIN ftActionGroupList ON (ivRLResources.ListName = ftActionGroupList.ListName)
AND (ivRLResources.ActionGroup = ftActionGroupList.ActionGroup))
LEFT JOIN qry_00ActionGroups ON ftActionGroupList.ListGIVID = qry_00ActionGroups.ListGIVID
ORDER BY ftActionGroupList.ListGIVID, qry_00ActionGroups.Code;
) AS qry_01ACvalues_1 ON
  ivRLLayer.LayerCode = qry_01ACvalues_1.Code
    AND ivRLLayer.LayerResource = qry_01ACvalues_1.ResourceGIVID
"


  if (!multiple) {
    sql_statement <- glue_sql(common_part,
                              "AND ivS.Name LIKE {survey_name}",
                              survey_name = survey_name,
                              .con = connection)

  } else {
    sql_statement <- glue_sql(common_part,
                              "AND ivS.Name IN {survey_name*}",
                              survey_name = survey_name,
                              .con = connection)
  }

  sql_statement <- glue_sql(
    sql_statement,
   "ORDER BY ivRecording.RecordingGivid, ivRLLayer.LayerCode",
    .con = connection)

  #sql_statement <- iconv(sql_statement, from =  "UTF-8", to = "latin1")

  query_result <- dbGetQuery(connection, sql_statement)

  return(query_result)

}

