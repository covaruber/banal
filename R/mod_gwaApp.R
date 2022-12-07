#' gwaApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_gwaApp_ui <- function(id){
  ns <- NS(id)
  tagList(

    tags$div(
      fluidRow(
        shinydashboard::box(width = 12, status = "primary", background =  "orange", collapsible = TRUE,
            tags$body(
              p(strong("Requirements:"),strong("1)")," 'Multi-Trial Analysis' or 'Selection Index' predictions to be the phenotype input File.",strong("2)"),"Cleaned markers file to be the marker input file." )
            )
        ),
        shinydashboard::box(status="primary", width = 4,
            # Input file (widget)
            selectInput(ns("phenoDTfileGwa"),
                        label = "Phenotype file for the analysis",
                        choices = metAndIdxAnalysisInPredictionsList, multiple=FALSE,
                        selected = NULL
            ),# ifelse(length(metAnalysisInPredictionsList) > 0,metAnalysisInPredictionsList[[1]],NULL ),
            # Input file (widget)
            selectInput(ns("markerDTfileGwa"),
                        label = "Marker file for the analysis",
                        choices = cleanedFilesMarkerList, multiple=FALSE,
                        selected = NULL,# ifelse(length(cleanedFilesMarkerList) > 0,cleanedFilesMarkerList[[1]],NULL ),

            )
        ),
        shinydashboard::box(status="primary", width = 4,
            # Input trait (widget)
            uiOutput(ns('traitGwa')),
            # Input fieldinst (widget)
            uiOutput(ns('fieldinstGwa')),
        ),
        shinydashboard::box(status="primary", width = 4,
            # Input verbose
            selectInput(ns("verboseGwa"),
                        label = "Noisy run?",
                        choices = list(TRUE,FALSE),
                        selected = FALSE, multiple=FALSE),
            # action button
            actionButton(ns("runGwa"), "Run")
        ),
        shinydashboard::box(status="primary", width = 12,
            title = "Analysis status:", solidHeader = TRUE,
            textOutput(ns("outGwa"))
        ),
        shinydashboard::box(status="primary", width = 12,
            title = "Phenotypes preview of chosen dataset:", solidHeader = TRUE,
            DT::dataTableOutput(ns("tablePhenoGwaAnalysis")) # Output: Table ----
        ),
        shinydashboard::box(status="primary", width = 12,
            title = "GRM preview of chosen dataset:", solidHeader = TRUE,
            DT::dataTableOutput(ns("tableMarkerGwaAnalysis")) # Output: Table ----
        )
      ),
      class = "tab-content"
    )

  )
}

#' gwaApp Server Functions
#'
#' @noRd
mod_gwaApp_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ## render result of selected file
    output$tablePhenoGwaAnalysis <- DT::renderDataTable(readRDS(file.path(getwd(),paste0("DB/predictions/",input$phenoDTfileGwa))),
                                                        options = list(
                                                          pageLength = 3,
                                                          initComplete = I("function(settings, json) {alert('Done.');}")
                                                        )
    )
    ## render result of selected file
    output$tableMarkerGwaAnalysis <- DT::renderDataTable((readRDS(file.path(getwd(),paste0("DB/files_cleaned/",input$markerDTfileGwa))))$M[,1:30],
                                                         options = list(
                                                           pageLength = 3,
                                                           initComplete = I("function(settings, json) {alert('Done.');}")
                                                         )
    )
    ##
    output$traitGwa = renderUI({
      mydata = readRDS(file.path(getwd(),paste0("DB/predictions/",input$phenoDTfileGwa))) #%>% subset(trait %in% input$traitFilterMetricsTable2)
      selectInput(ns('trait2Gwa'), 'Trait for cross prediction', unique(mydata$trait), multiple = FALSE)
    })
    ##
    output$fieldinstGwa = renderUI({
      mydata = readRDS(file.path(getwd(),paste0("DB/predictions/",input$phenoDTfileGwa))) #%>% subset(trait %in% input$traitFilterMetricsTable2)
      selectInput(ns('fieldinst2Gwa'), 'Fieldinst to analyze', unique(mydata$fieldinst), multiple = FALSE)
    })
    ## render result of "run" button click
    outGwa <- eventReactive(input$runGwa, {
      cgiarPIPE::gwas(phenoDTfile = input$phenoDTfileGwa, markerDTfile=input$markerDTfileGwa, wd="DB",
           trait=input$trait2Gwa, fieldinst = input$fieldinstGwa,verbose=input$verboseGwa )
    })
    output$outGwa <- renderPrint({
      outGwa()
    })

  })
}

## To be copied in the UI
# mod_gwaApp_ui("gwaApp_1")

## To be copied in the server
# mod_gwaApp_server("gwaApp_1")
