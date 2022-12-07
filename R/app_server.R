#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
# cleanpheno
allFilesInRaw <- dir("DB/files_raw")
allFilesInRawList <- as.list(allFilesInRaw)
# sta
allFilesInCleaned <-dir("DB/files_cleaned") #unique(modeling$analysisId)
cleanedFilesPheno <- allFilesInCleaned[grep("clp",allFilesInCleaned)]
cleanedFilesPhenoList <- as.list(cleanedFilesPheno)
baseN <- c("pipeline","year","season","location","country", "trial", "design","geno","genoCode","genoYearOrigin","rep","block","rowcoord","colcoord","entryType","entryType2","stage","rowindex")
baseN <- c(baseN, paste0(baseN,"F"), "fieldinstF")
# cleanmarker
cleanedFilesMarker <- allFilesInCleaned[grep("clm",allFilesInCleaned)]
cleanedFilesMarkerList <- as.list(cleanedFilesMarker)
# grm
cleanedFilesGrm <- allFilesInCleaned[grep("grm",allFilesInCleaned)]
cleanedFilesPed <- allFilesInCleaned[grep("num",allFilesInCleaned)]
cleanedFilesGrmPedList <- as.list(c(cleanedFilesGrm,cleanedFilesPed))
# met
allAnalysisInPredictions <- dir("DB/predictions")
predcols <- c("analysisId", "pipeline","trait","genoCode","geno","genoType","genoYearOrigin",
              "genoYearTesting", "fieldinst","predictedValue","stdError","rel","stage")
# predictions <- read.csv(file.path("DB/predictions.csv"))
allEffectsPredictions <- as.list(predcols)
allEffectsPredictionsPlus <- as.list(c("fieldinstF","genoF","genoYearOriginF","genoYearTestingF", setdiff(predcols,c("geno","fieldinst"))))
# allAnalysisInPredictions <- unique(predictions$analysisId)
staAnalysisInPredictions <- allAnalysisInPredictions[grep("sta",allAnalysisInPredictions)]
staAnalysisInPredictionsList <- as.list(staAnalysisInPredictions)
# ocs
ocsAnalysisInPredictions <- allAnalysisInPredictions[c(grep("idx",allAnalysisInPredictions),grep("met",allAnalysisInPredictions))]
ocsAnalysisInPredictionsList <- as.list(ocsAnalysisInPredictions)
# index
metAnalysisInPredictions <- allAnalysisInPredictions[grep("met",allAnalysisInPredictions)]
metAnalysisInPredictionsList <- as.list(metAnalysisInPredictions)
idxAnalysisInPredictions <- allAnalysisInPredictions[grep("idx",allAnalysisInPredictions)]
idxAnalysisInPredictionsList <- as.list(idxAnalysisInPredictions)
metAndIdxAnalysisInPredictions <- c(metAnalysisInPredictions,idxAnalysisInPredictions)
metAndIdxAnalysisInPredictionsList <- as.list(metAndIdxAnalysisInPredictions)
## VISUAL
allFilesPhenotypeInCleaned <- allFilesInCleaned[grep("clp",allFilesInCleaned)] ################
allFilesPhenotypeInCleanedList <- as.list(allFilesPhenotypeInCleaned) #########################
allAnalysisInPredictionsList <- as.list(allAnalysisInPredictions) ################
#
allAnalysisInMetrics <- dir("DB/metrics") ###############################
metrCols <- c("value","stdError","fieldinst","trait","analysisId", "method","traitUnits", "stage","parameter","pipeline")
allEffectsMetrics <- as.list(metrCols) ##################################
allAnalysisInMetricsList <- as.list(allAnalysisInMetrics) ########################

app_server <- function(input, output, session) {
  mod_home1_server("home1_1")
  # Cleaning options
  mod_cleanpApp_server("cleanpApp_1")
  mod_cleanmApp_server("cleanmApp_1")
  mod_cleanrApp_server("cleanrApp_1")
  # Relationship options
  mod_nrmApp_server("nrmApp_1")
  mod_grmApp_server("grmApp_1")
  # Prediction options
  mod_staApp_server("staApp_1")
  mod_metApp_server("metApp_1")
  mod_indexApp_server("indexApp_1")
  mod_ocsApp_server("ocsApp_1")
  mod_gwaApp_server("gwaApp_1")
  # Metrics options
  mod_rggApp_server("rggApp_1")
  mod_pggApp_server("pggApp_1")
}
