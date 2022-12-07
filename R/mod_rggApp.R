#' rggApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_rggApp_ui <- function(id){
  ns <- NS(id)
  tagList(

    tags$div(
      fluidRow(
        shinydashboard::box(width = 12, status = "primary", background =  "orange", collapsible = TRUE,
            tags$body(
              p(strong("Requirements:"),strong("1)"),"Either the STA-predictions analyzed with the
                          'Multi-Trial Analysis' option or the MET-predictions analyzed with the 'Selection Index'
                          option to be the phenotype input File.", strong("2)"),"Column 'genotype year of origin'
                          provided when the raw file is cleaned in the 'Clean pheno' option." )
            )
        ),
        shinydashboard::box(status="primary", width = 4,
            # Input file (widget)
            selectInput(ns("phenoDTfileReal"),
                        label = "File to be analyzed",
                        choices = c(metAnalysisInPredictionsList,idxAnalysisInPredictionsList), multiple=FALSE,
                        selected = NULL),
            # Input trait (widget)
            uiOutput(ns('traitReal'))
        ),
        shinydashboard::box(status="primary", width = 4,
            # Input modeling parameters (widget)
            selectInput(ns("fixedTermReal"),
                        label = "Fixed effects",
                        choices = allEffectsPredictions,
                        selected = c("genoYearOrigin"), multiple=TRUE),
            # Input verbose
            selectInput(ns("deregressReal"),
                        label = "Deregress data?",
                        choices = list(TRUE,FALSE),
                        selected = FALSE, multiple=FALSE),
        ),
        shinydashboard::box(status="primary", width = 4,
            # Input verbose
            selectInput(ns("partitionReal"),
                        label = "Partition the data?",
                        choices = list(TRUE,FALSE),
                        selected = FALSE, multiple=FALSE),
            # Input scaled desire (widget)
            numericInput(ns("deregressWeightReal"),
                         label = "Deregress weight",
                         value = 1),
            # action button
            actionButton(ns("runReal"), "Run")
        ),
        shinydashboard::box(status="primary", width = 12,
            title = "Analysis status:", solidHeader = TRUE,
            textOutput(ns("outReal"))
        ),
        shinydashboard::box(status="primary", width = 12,
            title = "Data preview of chosen dataset:", solidHeader = TRUE,
            DT::dataTableOutput(ns("tableRealAnalysis"))
        )
      ),
      class = "tab-content"
    )

  )
}

#' rggApp Server Functions
#'
#' @noRd
mod_rggApp_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$tableRealAnalysis <- DT::renderDataTable(readRDS(file.path(getwd(),paste0("DB/predictions/",input$phenoDTfileReal))),
                                                    options = list(
                                                      pageLength = 3,
                                                      initComplete = I("function(settings, json) {alert('Done.');}")
                                                    )
    )
    ## get the traits for the specific dataset
    output$traitReal = renderUI({
      mydata = readRDS(file.path(getwd(),paste0("DB/predictions/",input$phenoDTfileReal)))
      selectInput(ns('trait2Real'), 'Trait(s) to analyze', unique(mydata$trait), multiple = TRUE)
    })
    ## render result of "run" button click
    outReal <- eventReactive(input$runReal, {
      cgiarPIPE::rgg(phenoDTfile = input$phenoDTfileReal, wd="DB",
          trait=input$trait2Real, fixedTerm=input$fixedTermReal,
          deregressWeight=input$deregressWeightReal,
          deregress=as.logical(input$deregressReal),
          partition=as.logical(input$partitionReal) )
    })
    output$outReal <- renderPrint({
      outReal()
    })

  })
}

## To be copied in the UI
# mod_rggApp_ui("rggApp_1")

## To be copied in the server
# mod_rggApp_server("rggApp_1")
