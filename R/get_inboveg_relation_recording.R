#' @title Query relation (Parent - Child) information of recordings (relevé)
#' from INBOVEG
#'
#' @description This function queries the INBOVEG database for
#' relation information on recordings for one or more surveys based on
#' Parent (classic-chain/bucket) and Child (classic) relationship.
#'
#' @param survey_name A character string or a character vector, depending on
#' multiple parameter, giving the name or names of the
#' survey(s) for which you want to extract recordings information. If missing,
#' all surveys are returned.
#' @param connection `dbconnection` with the database 'Cydonia'
#' on the `inbo-sql07-prd` server
#' @param multiple If TRUE, survey_name can take a character vector with
#' multiple survey names that must match exactly. If FALSE (the default),
#' survey_name must be a single character string (one survey name) that can
#' include wildcards to allow partial matches
#' @param collect If FALSE (the default), a remote `tbl` object is returned.
#' This
#' is like a reference to the result of the query but the full result of the
#' query is not brought into memory. If TRUE the full result of the query is
#' collected (fetched) from the database and brought into memory of the working
#' environment.
#'
#' @return A dataframe with variables
#' `RecordingId`,
#' `Child_GIVID` (unique `RecordingGIVID`),
#' `Child_UserRef` (`UserReference`),
#' `ParentId` (`RecordingId`),
#' `Parent_GIVID` (unique `RecordingGIVID`) and
#' `Parent_UserRef` (`UserReference`)
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
#' # get the Parent-Child-relations from one survey
#' relations_N2000meetnet_Grasland <- get_inboveg_relation_recording(con,
#'     survey_name = "N2000meetnet_Grasland")
#'
#' # get all Parent-Child-relations from N2000meetnet surveys (partial matching)
#' relations_N2000meetnet <-
#'     get_inboveg_relation_recording(con, survey_name = "%N2000meetnet%")
#'
#' # get Parent-Child-relations from several specific surveys
#' relations_severalsurveys <-
#'    get_inboveg_relation_recording(con,
#'    survey_name = c("DeBlankaart-1985-Beheer", "N2000meetnet_Grasland"),
#'    multiple = TRUE)
#'
#' # get all Parent-Child-relations of all relevant surveys
#' allrelations <- get_inboveg_relation_recording(con)
#'
#' # Close the connection when done
#' dbDisconnect(con)
#' rm(con)
#' }
#'

get_inboveg_relation_recording <- function(connection,
                             survey_name,
                             multiple = FALSE,
                             collect = FALSE) {

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

  common_part <-
    "SELECT
  ivSurvey.Name
  , ivRecordingRelation.RecordingId
  , ivRecording.RecordingGivid AS Child_GIVID
  , ivRecording.UserReference AS Child_UserRef
  , ivRecordingRelation.ParentId
  , ivRecording_1.RecordingGivid AS Parent_GIVID
  , ivRecording_1.UserReference AS Parent_UserRef
  FROM (
  (ivRecordingRelation
  RIGHT JOIN ivRecording ON ivRecordingRelation.RecordingId = ivRecording.Id)
  LEFT JOIN ivRecording AS ivRecording_1
      ON ivRecordingRelation.ParentId = ivRecording_1.Id
  )
  INNER JOIN ivSurvey ON ivRecording.SurveyId = ivSurvey.Id
  WHERE (((ivRecordingRelation.ParentId) Is Not Null))"

if (!multiple) {
  sql_statement <- glue_sql(common_part,
                            "AND ivSurvey.Name LIKE {survey_name}",
                            survey_name = survey_name,
                            .con = connection)

} else {
  sql_statement <- glue_sql(common_part,
                            "AND ivSurvey.Name IN ({survey_name*})",
                            survey_name = survey_name,
                            .con = connection)
}

query_result <- tbl(connection, sql(sql_statement))

if (!isTRUE(collect)) {
  return(query_result)
} else {
  query_result <- collect(query_result)
  return(query_result)
}
}

get_inboveg_relation <- function(connection,
                                 survey_name,
                                 multiple = FALSE,
                                 collect = FALSE) {

  .Deprecated("get_inboveg_relation_recording")
  get_inboveg_relation_recording(connection = connection,
                                 survey_name = survey_name,
                                 multiple = multiple,
                                 collect = collect)
}
