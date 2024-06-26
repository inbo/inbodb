#' @title Query qualifier information of recordings (relevé) from INBOVEG
#'
#' @description This function queries the INBOVEG database for
#' qualifier information (site qualifier or management qualifier)on recordings
#' for one or more surveys.
#'
#' @param survey_name A character string or a character vector, depending on
#' multiple parameter, giving the name or names of the
#' survey(s) for which you want to extract recordings information. If missing,
#' all surveys are returned.
#' @param qualifier_type A character vector giving the name of qualifier type
#' for which you want to extract  information e.g. `'SQ'` (site qualifier),
#' `'MQ'`
#' (management qualifier).
#' If missing, all qualifier types are returned.
#' @param connection `dbconnection` with the database 'Cydonia'
#' on the `inbo-sql07-prd` server
#' @param multiple If TRUE, survey_name can take a character vector with
#' multiple survey names that must match exactly. If FALSE (the default),
#' survey_name must be a single character string (one survey name) that can
#' include wildcards to allow partial matches
#'
#' @return A dataframe with variables
#' `RecordingGivid` (unique Id),
#' `UserReference`,
#' `Observer`,
#' `QualifierType`,
#' `QualifierCode`,
#' `Description`,
#' `2nd QualifierCode`,
#' `2nd Description`,
#' `3rd QualifierCode`,
#' `3rd Description`,
#' `Elucidation`, in case qualifier is `'NotSure'`,
#' `ParentID`,
#' `QualifierResource`
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
#' con <- connect_inbo_dbase("D0010_00_Cydonia")
#'
#' # get the qualifiers from one survey
#' qualifiers_heischraal2012 <- get_inboveg_qualifier(con, survey_name =
#' "MILKLIM_Heischraal2012")
#'
#' # get all site qualifiers (SQ) from MILKLIM surveys (partial matching)
#' qualifiers_milkim <- get_inboveg_qualifier(con, survey_name = "%MILKLIM%",
#' qualifier_type = "SQ")
#'
#' # get qualifiers from several specific surveys
#' qualifiers_severalsurveys <- get_inboveg_qualifier(con, survey_name =
#' c("MILKLIM_Heischraal2012", "NICHE Vlaanderen"), multiple = TRUE)
#'
#' # get all qualifiers of all surveys
#' allqualifiers <- get_inboveg_qualifier(con)
#'
#' # Close the connection when done
#' dbDisconnect(con)
#' rm(con)
#' }
#'


get_inboveg_qualifier <- function(connection,
                               survey_name,
                               qualifier_type,
                               multiple = FALSE) {

  assert_that(inherits(connection, what = "Microsoft SQL Server"),
              msg = "Not a connection object to database.")

  if (missing(survey_name) && !multiple) {
    survey_name <- "%"
  }

  if (missing(survey_name) && multiple) {
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

  if (missing(qualifier_type)) {
    qualifier_type <- "%"
  } else {
    assert_that(is.character(qualifier_type))
  }


  common_part <- "SELECT ivS.Name
  , ivR.RecordingGivid
  , ivR.UserReference
  , ivR.Observer
  , ivRLQ.QualifierType
  , ivRLQ.QualifierCode as Q1Code
  , ftACV.Description as Q1Description
  , ivRLQ_P.QualifierCode as Q2Code
  , ftACV_P.Description as Q2Description
  , ivRLQ_GP.QualifierCode as Q3Code
  , ftACV_GP.Description as Q3Description
  , ivRLQ.Elucidation
  , ivRLQ.NotSure
  , ivRLQ.ParentID
  , ivRLQ.QualifierResource
  FROM  dbo.ivSurvey ivS
  INNER JOIN dbo.ivRecording ivR  ON ivR.SurveyId = ivS.Id
  LEFT JOIN dbo.ivRLQualifier ivRLQ ON ivRLQ.RecordingID = ivR.Id
  LEFT JOIN dbo.ivRLResources ivRLR ON
  ivRLR.ResourceGIVID = ivRLQ.QualifierResource
  LEFT JOIN dbo.ivRLQualifier ivRLQ_P ON ivRLQ_P.ParentID = ivRLQ.ID
  LEFT JOIN dbo.ivRLResources ivRLR_P ON
  ivRLR_P.ResourceGIVID = ivRLQ_P.QualifierResource
  LEFT JOIN dbo.ivRLQualifier ivRLQ_GP ON ivRLQ_GP.ParentID = ivRLQ_P.ID
  LEFT JOIN dbo.ivRLResources ivRLR_GP ON
  ivRLR_GP.ResourceGIVID = ivRLQ_GP.QualifierResource
  LEFT JOIN [syno].[Futon_dbo_ftActionGroupValues] ftACV ON
  ftACV.Code = ivRLQ.QualifierCode COLLATE Latin1_General_CI_AI
  AND ftACV.ActionGroup = ivRLR.ActionGroup  COLLATE Latin1_General_CI_AI
  AND ftACV.ListName = ivRLR.ListName  COLLATE Latin1_General_CI_AI

  LEFT JOIN [syno].[Futon_dbo_ftActionGroupValues] ftACV_P ON
  ftACV_P.Code = ivRLQ_P.QualifierCode  COLLATE Latin1_General_CI_AI
  AND ftACV_P.ActionGroup = ivRLR_P.ActionGroup  COLLATE Latin1_General_CI_AI
  AND ftACV_P.ListName = ivRLR_P.ListName  COLLATE Latin1_General_CI_AI

  LEFT JOIN [syno].[Futon_dbo_ftActionGroupValues] ftACV_GP ON
  ftACV_GP.Code = ivRLQ_GP.QualifierCode  COLLATE Latin1_General_CI_AI
  AND ftACV_GP.ActionGroup = ivRLR_GP.ActionGroup  COLLATE Latin1_General_CI_AI
  AND ftACV_GP.ListName = ivRLR_GP.ListName  COLLATE Latin1_General_CI_AI

  WHERE ivRLQ.ParentID Is Null
  "


  if (!multiple) {
    sql_statement <- glue_sql(common_part,
                              "AND ivS.Name LIKE {survey_name}
                              AND ivRLQ.QualifierType LIKE {qualifier_type}",
                              survey_name = survey_name,
                              qualifier_type = qualifier_type,
                              .con = connection)

  } else {
    sql_statement <- glue_sql(common_part,
                              "AND ivS.Name IN ({survey_name*})
                              AND ivRLQ.QualifierType LIKE {qualifier_type}",
                              survey_name = survey_name,
                              qualifier_type = qualifier_type,
                              .con = connection)
  }

  sql_statement <- glue_sql(
    sql_statement,
    "ORDER BY ivR.UserReference, ivRLQ.QualifierType, ivRLQ.QualifierCode
    OFFSET 0 ROWS",
    .con = connection)

  sql_statement <- iconv(sql_statement, from =  "UTF-8", to = "latin1")

  query_result <- dbGetQuery(connection, sql_statement)

  return(query_result)

}


get_inboveg_qualifiers <- function(connection,
                                   survey_name,
                                   qualifier_type,
                                   multiple = FALSE) {

  .Deprecated("get_inboveg_qualifier")
  get_inboveg_qualifier(connection = connection,
                        survey_name = survey_name,
                        qualifier_type = qualifier_type,
                        multiple = multiple)
}
