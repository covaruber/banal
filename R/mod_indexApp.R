#' indexApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_indexApp_ui <- function(id){
  ns <- NS(id)
  tagList(

    tags$div(
      fluidRow(
        shinydashboard::box(width = 12, status = "primary", background =  "orange", collapsible = TRUE,
            tags$body(
              p(strong("Requirements:"),strong("1)"),"Phenotype file processed with the 'Clean pheno' option",strong("2)"),"Cleaned phenotypes analyzed with the 'Single-Trial Analysis' option", strong("3)"),"STA-predictions analyzed with the 'Multi-Trial Analysis' option to be the phenotype input file." )
            )
        ),
        shinydashboard::box(status="primary", width = 6,
            # Input file (widget)
            selectInput(ns("phenoDTfileIndex"),
                        label = "File to be analyzed",
                        choices = metAnalysisInPredictionsList, multiple=FALSE,
                        selected = NULL), #
            # Input trait (widget)
            uiOutput(ns('traitIndex')),
            # Input lower bound h2 (widget)
            textInput(ns("desirev"), label = "Desire vector [Enter a numeric vector (comma delimited): e.g: 0,1,2 ]", value=NULL)
        ),
        shinydashboard::box(status="primary", width = 6,
            # Input scaled desire (widget)
            selectInput(ns("scaledIndex"),
                        label = "Scale predictions",
                        choices = list(TRUE,FALSE),
                        selected = TRUE, multiple=FALSE),
            # Input verbose
            selectInput(ns("verboseIndex"),
                        label = "Noisy run",
                        choices = list(TRUE,FALSE),
                        selected = FALSE, multiple=FALSE),
            # action button
            actionButton(ns("runIndex"), "Run")
        ),
        shinydashboard::box(status="primary", width = 12,
            title = "Analysis status:", solidHeader = TRUE,
            textOutput(ns("outIndex"))
        ),
        shinydashboard::box(status="primary", width = 12,
            title = "Data preview of chosen dataset:", solidHeader = TRUE,
            DT::dataTableOutput(ns("tableIndexAnalysis"))
        )
      ),
      class = "tab-content"
    )

  )
}

#' indexApp Server Functions
#'
#' @noRd
mod_indexApp_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ## render result of selected file
    output$tableIndexAnalysis <- DT::renderDataTable(readRDS(file.path(getwd(),paste0("DB/predictions/",input$phenoDTfileIndex))),
                                                     options = list(
                                                       pageLength = 3,
                                                       initComplete = I("function(settings, json) {alert('Done.');}")
                                                     )
    )
    ## get the traits for the specific dataset
    output$traitIndex = renderUI({
      mydata = readRDS(file.path(getwd(),paste0("DB/predictions/",input$phenoDTfileIndex)))
      selectInput(ns('trait2Index'), 'Trait(s) to analyze', unique(mydata$trait), multiple = TRUE)
    })
    ## render result of "run" button click
    outIndex <- eventReactive(input$runIndex, {
      x <- as.numeric(unlist(strsplit(input$desirev,",")))
      cgiarPIPE::index(phenoDTfile = input$phenoDTfileIndex, wd="DB",
            trait=input$trait2Index, desirev=x,
            scaled=input$scaledIndex, verbose=input$verboseIndex )
    })
    output$outIndex <- renderPrint({
      outIndex()
    })

  })
}

## To be copied in the UI
# mod_indexApp_ui("indexApp_1")

## To be copied in the server
# mod_indexApp_server("indexApp_1")
