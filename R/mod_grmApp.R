#' grmApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_grmApp_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$div(
      fluidRow(
        shinydashboard::box(width = 12, status = "primary", background =  "orange", collapsible = TRUE,
            tags$body(
              p(strong('Requirements:'),strong("1)"),"Marker file processed with the 'Clean marker' option to be the input file." )
            )
        ),
        shinydashboard::box(status="primary", width = 4,
            selectInput(ns("markerDTfileGrm"),
                        label = "File to be analyzed",
                        choices = cleanedFilesMarkerList, multiple=FALSE,
                        selected = NULL)
        ),
        shinydashboard::box(status="primary", width = 4,
            selectInput(ns("verboseGrm"),
                        label = "Noisy run?",
                        choices = list(TRUE,FALSE),
                        selected = FALSE, multiple=FALSE)
        ),
        shinydashboard::box(status="primary", width = 4,
            actionButton(ns("runGrm"), "Run")
        ),
        shinydashboard::box(status="primary", width = 12,
            title = "Analysis status:", solidHeader = TRUE,
            textOutput(ns("outGrm"))
        ),
        shinydashboard::box(status="primary", width = 12,
            title = "Data preview of chosen dataset:", solidHeader = TRUE,
            DT::dataTableOutput(ns("tableGrmAnalysis"))
        )
      ),
      class = "tab-content"
    )
  )
}

#' grmApp Server Functions
#'
#' @noRd
mod_grmApp_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$tableGrmAnalysis <- DT::renderDataTable((readRDS(file.path(getwd(),paste0("DB/files_cleaned/",input$markerDTfileGrm))))$M,
                                                   options = list(
                                                     pageLength = 3,
                                                     initComplete = I("function(settings, json) {alert('Done.');}")
                                                   )
    )
    ## render result of "run" button click
    outGrm <- eventReactive(input$runGrm, {
      cgiarPIPE::grm(markerDTfile = input$markerDTfileGrm, verbose=input$verboseGrm, wd="DB")
    })
    output$outGrm <- renderPrint({
      outGrm()
    })
  })
}

## To be copied in the UI
# mod_grmApp_ui("grmApp_1")

## To be copied in the server
# mod_grmApp_server("grmApp_1")
