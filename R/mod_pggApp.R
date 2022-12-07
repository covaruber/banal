#' pggApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_pggApp_ui <- function(id){
  ns <- NS(id)
  tagList(

    tags$div(
      fluidRow(
        shinydashboard::box(width = 12, status = "primary", background =  "orange", collapsible = TRUE,
            tags$body(
              p(strong("Requirements:"),strong("1)"),"Either the STA-predictions analyzed with the
                          'Multi-Trial Analysis' option or the MET-predictions analyzed with the 'Selection Index'
                          option to be the phenotype input File.", strong("2)"),"Column 'genotype year of origin'
                          provided when the raw file is cleaned in the 'Clean pheno' option."  )
            )
        ),
        shinydashboard::box(status="primary", width = 4,
            selectInput(ns("phenoDTfilePredicted"),
                        label = "File to be analyzed",
                        choices = metAnalysisInPredictionsList, multiple=FALSE,
                        selected = NULL),
            # Input trait (widget)
            uiOutput(ns('traitPredicted'))
        ),
        shinydashboard::box(status="primary", width = 4,
            # Input trait (widget)
            uiOutput(ns('yearTestingPredicted')),
            # Input trait (widget)
            uiOutput(ns('fieldinstPredicted')),
            # Input scaled desire (widget)
            numericInput(ns("percentagePredicted"),
                         label = "Percentage selected",
                         value = 10)
        ),
        shinydashboard::box(status="primary", width = 4,
            # Input scaled desire (widget)
            numericInput(ns("lIdealPredicted"),
                         label = "Cycle time (idealized)",
                         value = NA),
            # Input verbose
            selectInput(ns("verbosePredicted"),
                        label = "Noisy run?",
                        choices = list(TRUE,FALSE),
                        selected = FALSE, multiple=FALSE),
            # action button
            actionButton(ns("runPredicted"), "Run")
        ),
        shinydashboard::box(status="primary", width = 12,
            title = "Analysis status:", solidHeader = TRUE,
            textOutput(ns("outPredicted"))
        ),
        shinydashboard::box(status="primary", width = 12,
            title = "Data preview of chosen dataset:", solidHeader = TRUE,
            DT::dataTableOutput(ns("tablePredictedAnalysis"))
        )
      ),
      class = "tab-content"
    )

  )
}

#' pggApp Server Functions
#'
#' @noRd
mod_pggApp_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ## render result of selected file
    output$tablePredictedAnalysis <- DT::renderDataTable(readRDS(file.path(getwd(),paste0("DB/predictions/",input$phenoDTfilePredicted))),
                                                         options = list(
                                                           pageLength = 3,
                                                           initComplete = I("function(settings, json) {alert('Done.');}")
                                                         )
    )
    ## get the traits for the specific dataset
    output$traitPredicted = renderUI({
      mydata = readRDS(file.path(getwd(),paste0("DB/predictions/",input$phenoDTfilePredicted)))
      selectInput(ns('trait2Predicted'), 'Trait(s) to analyze', unique(mydata$trait), multiple = TRUE)
    })
    ## get the traits for the specific dataset
    output$fieldinstPredicted = renderUI({
      mydata = readRDS(file.path(getwd(),paste0("DB/predictions/",input$phenoDTfilePredicted)))
      selectInput(ns('fieldinst2Predicted'), 'Field instance(s) to analyze', unique(mydata$fieldinst), multiple = FALSE)
    })
    ## get the traits for the specific dataset
    output$yearTestingPredicted = renderUI({
      mydata = readRDS(file.path(getwd(),paste0("DB/predictions/",input$phenoDTfilePredicted)))
      selectInput(ns('yearTesting2Predicted'), 'Year(s) to analyze', unique(mydata$genoYearTesting), multiple = FALSE)
    })
    ## render result of "run" button click
    outPredicted <- eventReactive(input$runPredicted, {
      cgiarPIPE::pgg(
        phenoDTfile = input$phenoDTfilePredicted, wd="DB",
        trait=input$trait2Predicted, fieldinst=input$fieldinst2Predicted, year=input$yearTesting2Predicted,
        percentage=input$percentagePredicted,
        lIdeal=input$lIdealPredicted, verbose=TRUE
      )
    })
    output$outPredicted <- renderPrint({
      outPredicted()
    })

  })
}

## To be copied in the UI
# mod_pggApp_ui("pggApp_1")

## To be copied in the server
# mod_pggApp_server("pggApp_1")
