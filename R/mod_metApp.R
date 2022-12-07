#' metApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_metApp_ui <- function(id){
  ns <- NS(id)
  tagList(

    tags$div(
      fluidRow(
        shinydashboard::box(width = 12, status = "primary", background =  "orange", collapsible = TRUE,
                            tags$body(
                              p(strong("Requirements:"),strong("1)"),"Phenotype file processed with the 'Clean pheno' option",strong("2)"),"Cleaned phenotypes analyzed with the 'Single-Trial Analysis' option to be the phenotype input file." )
                            )
        ),
        shinydashboard::box(status="primary", width = 4,
                            selectInput(ns("phenoDTfileMet"),
                                        label = "File to be analyzed",
                                        choices = staAnalysisInPredictionsList, multiple=FALSE,
                                        selected = NULL), #
                            # Input trait (widget)
                            uiOutput(ns('traitMet')),
                            # Input modeling parameters (widget)
                            selectInput(ns("fixedTermMet"),
                                        label = "Fixed effects",
                                        choices = c("1",allEffectsPredictionsPlus),
                                        selected = c("1"), multiple=TRUE),
                            selectInput(ns("randomTermMet"),
                                        label = "Random effects",
                                        choices = allEffectsPredictionsPlus,
                                        selected = c("fieldinstF","genoF"), multiple=TRUE),
                            selectInput(ns("interactionsWithGenoMet"),
                                        label = "Interactions to fit with genotype:",
                                        choices = list("fieldinstF","genoYearOriginF","pipelineF","stageF"),
                                        selected = NULL, multiple=TRUE)
        ),
        shinydashboard::box(status="primary", width = 4,
                            selectInput(ns("residualTermMet"),
                                        label = "Residuals by:",
                                        choices = list("fieldinstF","genoYearOriginF","pipelineF","stageF"),
                                        selected = NULL, multiple=FALSE),
                            selectInput(ns("sparseTermMet"),
                                        label = "Sparse fixed effects",
                                        choices = allEffectsPredictionsPlus,
                                        selected = NULL, multiple=TRUE),
                            # Input lower bound h2 (widget)
                            numericInput(ns("heritLBMet"), label = "H2(lower bound)", value = 0.15),
                            # Input lower bound h2 (widget)
                            numericInput(ns("heritUBMet"), label = "H2(upper bound)", value = 0.95)
        ),
        shinydashboard::box(status="primary", width = 4,
                            # Input workspace (widget)
                            textInput(ns("workspaceMet"), label = "Workspace (Mb)", value = "600"),
                            # Input pworkspace (widget)
                            textInput(ns("pworkspaceMet"), label = "Pworkspace (Mb)", value = "600"),
                            # Input scaled desire (widget)
                            selectInput(ns("scaledDesireMet"),
                                        label = "Scale desire file",
                                        choices = list(TRUE,FALSE),
                                        selected = TRUE, multiple=FALSE),
                            # Input verbose
                            selectInput(ns("verboseMet"),
                                        label = "Noisy run",
                                        choices = list(TRUE,FALSE),
                                        selected = FALSE, multiple=FALSE),
                            # action button
                            actionButton(ns("runMet"), "Run")
        ),
        shinydashboard::box(status="primary", width = 12,
                            title = "Analysis status:", solidHeader = TRUE,
                            textOutput(ns("outMet"))
        ),
        shinydashboard::box(status="primary", width = 12,
                            title = "Data preview of chosen dataset:", solidHeader = TRUE,
                            DT::dataTableOutput(ns("tableMetAnalysis"))
        )
      ),
      class = "tab-content"
    )

  )
}

#' metApp Server Functions
#'
#' @noRd
mod_metApp_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # met $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    output$tableMetAnalysis <- DT::renderDataTable(readRDS(file.path(getwd(),paste0("DB/predictions/",input$phenoDTfileMet))),
                                                   options = list(
                                                     pageLength = 3,
                                                     initComplete = I("function(settings, json) {alert('Done.');}")
                                                   )
    )
    output$traitMet = renderUI({
      mydata = readRDS(file.path(getwd(),paste0("DB/predictions/",input$phenoDTfileMet)))
      selectInput(ns('trait2Met'), 'Trait(s) to analyze', unique(mydata$trait), multiple = TRUE)
    })
    ## render result of "run" button click
    outMet <- eventReactive(input$runMet, {
      cgiarPIPE::met(phenoDTfile = input$phenoDTfileMet, wd="DB",
                     fixedTerm = input$fixedTermMet, randomTerm = input$randomTermMet, interactionsWithGeno = input$interactionsWithGenoMet,
                     sparseTerm = input$sparseTermMet, residualBy = input$residualTermMet,
                     trait=input$trait2Met, heritLB=input$heritLBMet, heritUB=input$heritUBMet,
                     workspace = paste0(input$workspaceMet,"mb"), pworkspace = paste0(input$pworkspaceMet,"mb"),
                     scaledDesire=input$scaledDesireMet, verbose=input$verboseMet )
    })
    output$outMet <- renderPrint({
      outMet()
    })
  })
}

## To be copied in the UI
# mod_metApp_ui("metApp_1")

## To be copied in the server
# mod_metApp_server("metApp_1")
