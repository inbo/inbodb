#' @title Query PPA (point-plant distance) information from INBOVEG
#'
#' @description This function queries the INBOVEG database for
#' PPA-type relevé information (which species were recorded at what distance
#' from a point location) for one or more surveys,
#' or in combination with the unique ID (`recordingGIVID`) or user reference
#' Wildcards in `survey_name`, `user_reference` or `recording_givid`
#' should only be used if a character string (a length one character vector),
#' otherwise values are assumed to match exactly.
#'
#' @inheritParams get_inboveg_recording
#'
#' @return A remote `tbl` object (collect = FALSE) or a `tibble` dataframe
#' (collect = TRUE) with variables
#' `SurveyName`,
#' `RecordingGivid`,
#' `UserReference`,
#' `DateRecording`,
#' `LocationCode`,
#' `CoordinateRefSystem`,
#' `GivenLatitude`,
#' `GivenLongitude`,
#' `GivenLatitude2`,
#' `GivenLongitude2`,
#' `MaxSearchEffortUnit`,
#' `MaxSearchEffortLabel`,
#' `Indirect`,
#' `NotSure`,
#' `LayerCode`,
#' `LayerCover`,
#' `OriginalName`,
#' `ScientificName`,
#' `TaxonGroupCode`,
#' `PhenologyCode`,
#' `Distance`,
#' `Comment`
#' `DateIdentification`,
#' `RecordTypeName`
#'
#' @importFrom glue glue_sql
#' @importFrom DBI dbGetQuery
#' @importFrom assertthat assert_that
#' @importFrom dplyr collect tbl sql
#'
#' @export
#' @family inboveg
#' @examples
#' \dontrun{
#' library(inbodb)
#' con <- connect_inbo_dbase("D0010_00_Cydonia")
#'
#' # get the recordings from one survey and collect the data
#' specifieke_survey <- get_inboveg_ppa(con, survey_name =
#' "LEN_sinusmaaiproject_ppa", collect = TRUE)
#'
#' # get all recordings from with partial matching, don't collect
#' partial_match <- get_inboveg_ppa(con, survey_name = "%LEN%",
#' collect = FALSE)
#'
#' # get recordings from several specific recordinggivid
#' recording_severalgivids <- get_inboveg_ppa(con,
#' recording_givid = c("IV2024040411243457","IV2024040411263782"),
#' collect = TRUE)
#'
#' # get all PPA-type recordings of all surveys,  don't collect the data
#' all_ppa <- get_inboveg_ppa(con)
#'
#' # Close the connection when done
#' dbDisconnect(con)
#' rm(con)
#' }

get_inboveg_ppa <- function(
  connection,
  survey_name = "%",
  user_reference = "%",
  recording_givid = "%",
  collect = FALSE) {

  assert_that(inherits(connection, what = "Microsoft SQL Server"),
              msg = "Not a connection object to database.")
  assert_that(is.character(survey_name))
  assert_that(is.character(user_reference))
  assert_that(is.character(recording_givid))

  common_part <- "SELECT ivS.Name as SurveyName
  , ivR.[RecordingGivid]
  , ivR.UserReference
  , ivR.VagueDateBegin as DateRecording
  , ivR.LocationCode
  , ivR.CoordinateRefSystem
  , ivR.GivenLatitude
  , ivR.GivenLongitude
  , ivR.GivenLatitude2
  , ivR.GivenLongitude2
  --, ivRL_Qual.QualifierCode as MaxSearchEffort
  , ftQualifier.Description as MaxSearchEffortUnit
  , ftQualifier.Elucidation as MaxSearchEffortLabel
  , ivRL_Qual.Indirect
  , ivRL_Qual.NotSure
  , ivRL_Layer.LayerCode
  , ivRL_Layer.CoverCode as LayerCover
  , ivRL_Iden.TaxonFullText as OriginalName
  , Synoniem.ScientificName
  , ivRL_Iden.TaxonGroup as TaxonGroupCode
  , ivRL_Iden.PhenologyCode
  , ivRL_Taxon.CustomField1Code as Distance
  , ivRL_Iden.Comment
  , ivRL_Iden.VagueDateBegin as DateIdentification
  , ivRec.Name as RecordTypeName
  FROM  dbo.ivSurvey ivS
  INNER JOIN [dbo].[ivRecording] ivR  ON ivR.SurveyId = ivS.Id
  INNER JOIN [dbo].[ivRecTypeD] ivRec on ivRec.ID = ivR.RecTypeID
  -- Deel met soortenlijst en synoniem
  INNER JOIN [dbo].[ivRLLayer] ivRL_Layer on ivRL_Layer.RecordingID = ivR.Id
  INNER JOIN [dbo].[ivRLTaxonOccurrence] ivRL_Taxon on
  ivRL_Taxon.LayerID = ivRL_Layer.ID
  INNER JOIN [dbo].[ivRLIdentification] ivRL_Iden on
  ivRL_Iden.OccurrenceID = ivRL_Taxon.ID
  LEFT JOIN (SELECT ftTaxon.TaxonName AS TaxonFullText
  , COALESCE([GetSyn].TaxonName, ftTaxon.TaxonName) AS ScientificName
  , COALESCE([GetSyn].TaxonGIVID, ftTaxon.TaxonGIVID) AS TAXON_LIST_ITEM_KEY
  , COALESCE([GetSyn].TaxonQuickCode, ftTaxon.TaxonQuickCode) AS QuickCode
  FROM [syno].[Futon_dbo_ftTaxon] ftTaxon
  INNER JOIN [syno].[Futon_dbo_ftTaxonListItem] ftTLI ON
  ftTLI.TaxonGIVID = ftTaxon.TaxonGIVID
  LEFT JOIN (SELECT ftTaxonLI.TaxonListItemGIVID
  , ftTaxon.TaxonGIVID
  , ftTaxon.TaxonName
  , ftTaxon.TaxonQuickCode
  , ftAGL.ListName
  , ftTaxonLI.PreferedListItemGIVID
  FROM [syno].[Futon_dbo_ftActionGroupList] ftAGL
  INNER JOIN [syno].[Futon_dbo_ftTaxonListItem] ftTaxonLI ON
  ftTaxonLI.TaxonListGIVID = ftAGL.ListGIVID
  LEFT JOIN [syno].[Futon_dbo_ftTaxon] ftTaxon ON
  ftTaxon.TaxonGIVID = ftTaxonLI.TaxonGIVID
  WHERE 1=1
  AND ftAGL.ListName = 'INBO-2011 Sci'
  ) GetSyn
  ON GetSyn.TaxonListItemGIVID = ftTLI.PreferedListItemGIVID
  WHERE ftTLI.TaxonListGIVID = 'TL2011092815101010'
  ) Synoniem on
  ivRL_Iden.TaxonFullText = Synoniem.TaxonFullText collate Latin1_General_CI_AI
  -- KVE: Opvragen max. zoekafstand
  LEFT JOIN [dbo].[ivRLQualifier] ivRL_Qual ON ivRL_Qual.LayerID = ivRL_Layer.ID
  -- KVE: Opvragen eenheid max. zoekinspanning
  LEFT JOIN [dbo].[ivRLResources] ivRL_Res on
  ivRL_Res.ResourceGIVID = ivRL_Qual.QualifierResource
  LEFT JOIN [syno].[Futon_dbo_ftActionGroupList] ftAGL on
  ftAGL.ActionGroup = ivRL_Res.ActionGroup collate Latin1_General_CI_AI
  AND ftAGL.ListName = ivRL_Res.ListName collate Latin1_General_CI_AI
  LEFT JOIN [syno].[Futon_dbo_ftQualifierValues] ftQualifier on
  ftQualifier.ListGIVID = ftAGL.ListGIVID
  AND ivRL_Qual.QualifierCode = ftQualifier.Code collate Latin1_General_CI_AI
  WHERE 1=1
  AND ivRL_Iden.Preferred = 1
  AND ivREc.Name LIKE 'PPA'
  "

  sql_statement <- complete_sql_statement(
    survey_name = survey_name,
    recording_givid = recording_givid,
    user_reference = user_reference,
    common_part = common_part,
    connection = connection)

  query_result <- tbl(connection, sql(sql_statement))

  if (isTRUE(collect)) {
    query_result <- collect(query_result)
  }
  return(query_result)
}
