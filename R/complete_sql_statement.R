#' INBOVEG helper function
#'
#' Helper function to construct the `WHERE` part of the `SQL` statement and glue
#' it to a common part of the `SQL`. It is used by `get_inboveg_recording()` and
#' `get_inboveg_ppa()`.
#'
#'
#' @importFrom assertthat assert_that
#' @importFrom glue glue_sql
#' @noRd
complete_sql_statement <- function(
    survey_name,
    recording_givid,
    user_reference,
    common_part,
    connection) {
  survey_where <- ifelse(
    length(survey_name) != 1,
    " AND ivS.Name IN ({survey_name*})",
    " AND ivS.Name LIKE {survey_name}"
  )

  user_reference_where <- ifelse(
    length(user_reference) != 1,
    "ivR.UserReference IN ({user_reference*})",
    "ivR.UserReference LIKE {user_reference}"
  )

  recording_givid_where <- ifelse(
    length(recording_givid) != 1,
    "ivR.[RecordingGivid] IN ({recording_givid*})",
    "ivR.[RecordingGivid] LIKE {recording_givid}"
  )

  combined_where_clauses <-
    paste0(
      survey_where,
      " AND ",
      user_reference_where,
      " AND ",
      recording_givid_where
    )

  sql_statement <- glue_sql(
    common_part,
    combined_where_clauses,
    survey_name = survey_name,
    recording_givid = recording_givid,
    user_reference = user_reference,
    .con = connection)

  return(sql_statement)
}
