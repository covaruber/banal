#' staApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_staApp_ui <- function(id){
  ns <- NS(id)
  tagList(

    tags$div(
      fluidRow(
        shinydashboard::box(width = 12, status = "primary", background =  "orange", collapsible = TRUE,
            tags$body(
              p(strong('Requirements:'),strong("1)"),"Phenotype file processed with the 'Clean pheno' option to be the phenotype input file." )
            )
        ),
        shinydashboard::box(status="primary", width = 6,
            # Input file (widget)
            selectInput(ns("phenoDTfileSta"),
                        label = "File to be analyzed",
                        choices = cleanedFilesPhenoList, multiple=FALSE,
                        selected = NULL),
            # Input modeling parameters (widget)
            selectInput(ns("fixedTermSta"),
                        label = "Fixed effects",
                        choices = list("1", "genoF", "fieldinstF", "pipelineF","yearF","seasonF","locationF","countryF", "trialF", "rowcoordF", "colcoordF","repF","blockF","stageF"),
                        selected = c("1","genoF"), multiple=TRUE),
            # Input trait (widget)
            uiOutput(ns('traitSta')),
        ),
        shinydashboard::box(status="primary", width = 6,
            # Input workspaceSta (widget)
            textInput(ns("workspaceSta"), label = "Workspace (Mb)", value = "500"),
            # Input pworkspaceSta (widget)
            textInput(ns("pworkspaceSta"), label = "pworkspaceSta (Mb)", value = "500"),
            # Input verbose
            selectInput(ns("verboseSta"),
                        label = "Noisy run",
                        choices = list(TRUE,FALSE),
                        selected = FALSE, multiple=FALSE),
            # action button
            actionButton(ns("runSta"), "Run")
        ),
        shinydashboard::box(status="primary", width = 12,
            title = "Analysis status:", solidHeader = TRUE,
            textOutput(ns("outSta"))
        ),
        shinydashboard::box(status="primary", width = 12,
            title = "Data preview of chosen dataset:", solidHeader = TRUE,
            DT::dataTableOutput(ns("tableStaAnalysis"))
        )
      ),
      class = "tab-content"
    )
  )
}

#' staApp Server Functions
#'
#' @noRd
mod_staApp_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # sta $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    ## render result of selected file
    output$tableStaAnalysis <- DT::renderDataTable(readRDS(file.path(getwd(),paste0("DB/files_cleaned/",input$phenoDTfileSta))),
                                                   options = list(
                                                     pageLength = 3,
                                                     initComplete = I("function(settings, json) {alert('Done.');}")
                                                   )
    )
    #
    output$traitSta = renderUI({
      mydata = readRDS(file.path(getwd(),paste0("DB/files_cleaned/",input$phenoDTfileSta)))
      selectInput(ns('trait2Sta'), 'Trait(s) to analyze', setdiff(names(mydata), baseN), multiple = TRUE)
    })
    ## render result of "run" button click
    outSta <- eventReactive(input$runSta, {
      cgiarPIPE::sta(phenoDTfile = paste0(input$phenoDTfileSta),
          trait=input$trait2Sta, fixedTerm = input$fixedTermSta, wd="DB",
          workspace = paste0(input$workspaceSta,"mb"), pworkspace = paste0(input$pworkspaceSta,"mb"),
          verbose = input$verboseSta)
    })
    output$outSta <- renderPrint({
      outSta()
    })
  })
}

## To be copied in the UI
# mod_staApp_ui("staApp_1")

## To be copied in the server
# mod_staApp_server("staApp_1")
