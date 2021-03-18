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
#' qualifiers_milkim <- get_inboveg_layer(con, survey_name = "%MILKLIM%")
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

common_part <- "SELECT ivS.Name
, ivRecording.RecordingGivid
, ivRecording.UserReference
, ivRLLayer.LayerCode
, ftAGV.Description as LayerDescription
, ivRLQualifier.QualifierCode
, ftAGV.Description as QualifierDescription
, ivRLQualifier.Elucidation
, ivRLQualifier.NotSure
FROM ivRecording
INNER JOIN ivSurvey ivS on ivS.Id = ivRecording.SurveyId
LEFT JOIN  ivRLLayer on ivRLLayer.RecordingID = ivRecording.Id
LEFT JOIN ivRLQualifier ON ivRLLayer.ID = ivRLQualifier.LayerID
LEFT JOIN ivRLResources on ivRLResources.ResourceGIVID = ivRLLayer.LayerResource
LEFT JOIN [syno].[Futon_dbo_ftActionGroupValues] ftAGV ON ivRLResources.ListName = ftAGV.ListName COLLATE Latin1_General_CI_AI
AND ivRLResources.ActionGroup = ftAGV.ActionGroup COLLATE Latin1_General_CI_AI
WHERE 1 = 1"


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

