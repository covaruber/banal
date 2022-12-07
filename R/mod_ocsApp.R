#' ocsApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_ocsApp_ui <- function(id){
  ns <- NS(id)
  tagList(

    tags$div(
      fluidRow(
        shinydashboard::box(width = 12, status = "primary", background =  "orange", collapsible = TRUE,
            tags$body(
              p(strong("Requirements:"),strong("1)"),"Either the STA-predictions analyzed with the 'Multi-Trial Analysis' option or the MET-predictions analyzed with the 'Selection Index' option to be the phenotype input File.",strong("2)"),"Markers or pedigree analyzed with either the 'Numerator' or 'Genomic' relationship options to be the relationship input file." )
            )
        ),
        shinydashboard::box(status="primary", width = 4,
            # Input file (widget)
            selectInput(ns("phenoDTfileOcs"),
                        label = "Phenotype file for the analysis",
                        choices = ocsAnalysisInPredictionsList, multiple=FALSE,
                        selected = NULL),
            # Input file (widget)
            selectInput(ns("relmatDTfileOcs"),
                        label = "Relationship file",
                        choices = cleanedFilesGrmPedList, multiple=FALSE,
                        selected = NULL),
            # Input trait (widget)
            uiOutput(ns('traitOcs'))
        ),
        shinydashboard::box(status="primary", width = 4,
            # Input fieldinst (widget)
            uiOutput(ns('fieldinstOcs')),
            # Input #crosses (widget)
            numericInput(ns("nCrossOcs"), label = "Number of crosses", value = 20),
            # Input target angle (widget)
            numericInput(ns("targetAngleOcs"), label = "Target angle", value = 30)
        ),
        shinydashboard::box(status="primary", width = 4,
            # Input verbose
            selectInput(ns("verboseOcs"),
                        label = "Noisy run?",
                        choices = list(TRUE,FALSE),
                        selected = FALSE, multiple=FALSE),
            # action button
            actionButton(ns("runOcs"), "Run")
        ),
        shinydashboard::box(status="primary", width = 12,
            title = "Analysis status:", solidHeader = TRUE,
            textOutput(ns("outOcs"))
        ),
        shinydashboard::box(status="primary", width = 12,
            title = "Phenotypes preview of chosen dataset:", solidHeader = TRUE,
            DT::dataTableOutput(ns("tablePhenoOcsAnalysis"))
        ),
        shinydashboard::box(status="primary", width = 12,
            title = "GRM preview of chosen dataset:", solidHeader = TRUE,
            DT::dataTableOutput(ns("tableGrmOcsAnalysis"))
        )
      ),
      class = "tab-content"
    )

  )
}

#' ocsApp Server Functions
#'
#' @noRd
mod_ocsApp_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ## render result of selected file
    output$tablePhenoOcsAnalysis <- DT::renderDataTable(readRDS(file.path(getwd(),paste0("DB/predictions/",input$phenoDTfileOcs))),
                                                        options = list(
                                                          pageLength = 3,
                                                          initComplete = I("function(settings, json) {alert('Done.');}")
                                                        )
    )
    ## render result of selected file
    output$tableGrmOcsAnalysis <- DT::renderDataTable((readRDS(file.path(getwd(),paste0("DB/files_cleaned/",input$relmatDTfileOcs)))),
                                                      options = list(
                                                        pageLength = 3,
                                                        initComplete = I("function(settings, json) {alert('Done.');}")
                                                      )
    )
    ##
    output$traitOcs = renderUI({
      mydata = readRDS(file.path(getwd(),paste0("DB/predictions/",input$phenoDTfileOcs)))
      selectInput(ns('trait2Ocs'), 'Trait for cross prediction', unique(mydata$trait), multiple = FALSE)
    })
    ##
    output$fieldinstOcs = renderUI({
      mydata = readRDS(file.path(getwd(),paste0("DB/predictions/",input$phenoDTfileOcs)))
      selectInput(ns('fieldinst2Ocs'), 'Fieldinst to analyze', unique(mydata$fieldinst), multiple = FALSE)
    })
    ## render result of "run" button click
    outOcs <- eventReactive(input$runOcs, {
      cgiarPIPE::ocs(phenoDTfile = input$phenoDTfileOcs, relDTfile=input$relmatDTfileOcs, wd="DB",
          trait=input$trait2Ocs, fieldinst = input$fieldinst2Ocs, nCross=input$nCrossOcs,
          targetAngle=input$targetAngleOcs, verbose=input$verboseOcs )
    })
    output$outOcs <- renderPrint({
      outOcs()
    })

  })
}

## To be copied in the UI
# mod_ocsApp_ui("ocsApp_1")

## To be copied in the server
# mod_ocsApp_server("ocsApp_1")
