#' cleanmApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

mod_cleanmApp_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$div(
      fluidRow(
        shinydashboard::box(width = 12, status = "primary", background =  "orange", collapsible = TRUE,
            tags$body(
              p(strong('Requirements:'),strong("1)"),"Marker file in csv format stored in the 'files_raw' folder to be the input file.", strong("2)"),"A column with genotype IDs that will match the genotype column in the phenotypes." )
            )
        ),
        shinydashboard::box(status="primary", width = 6,
            selectInput(ns('markerDTfileCleanMarker'), 'Choose Dataset', allFilesInRawList),
            # Input genotype column (widget)
            uiOutput(ns('genoCleanMarker')),
            # Input workspace (widget)
            numericInput(ns("firstMarkerColumn"), label = "First marker column", value = 2)
        ),
        shinydashboard::box(status="primary", width = 6,
            # Input missing data (widget)
            textInput(ns("missingData"), label = "Missing data code(s) [Enter a vector (comma delimited): e.g: NN,NA,999 ]",
                      value="NN"),
            # action button
            actionButton(ns("runCleanMarker"), "Run")
        ),
        shinydashboard::box(status="primary", width = 12,
            title = "Analysis status:", solidHeader = TRUE,
            textOutput(ns("outCleanMarker"))
        ),
        shinydashboard::box(status="primary", width = 12,
            title = "Data preview of chosen dataset:", solidHeader = TRUE,
            DT::dataTableOutput(ns("tableCleanMarker"))
        )
      ),
      class = "tab-content"
    )
  )
}

#' cleanmApp Server Functions
#'
#' @noRd
mod_cleanmApp_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # cleanmarker $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    ## render result of selected file
    output$tableCleanMarker <- DT::renderDataTable(utils::read.csv(file.path(getwd(),paste0("DB/files_raw/",input$markerDTfileCleanMarker))),
                                                   options = list(
                                                     pageLength = 3,
                                                     initComplete = I("function(settings, json) {alert('Done.');}")
                                                   )
    )
    output$genoCleanMarker = renderUI({
      mydata = utils::read.csv(file.path(getwd(),paste0("DB/files_raw/",input$markerDTfileCleanMarker))) # get(input$dataset)
      selectInput(ns('geno2CleanMarker'), 'Genotype column name', choices=c(names(mydata)), selected = "ABSCENT" )
    })
    outCleanMarker <- eventReactive(input$runCleanMarker, {
      mydata = utils::read.csv(file.path(getwd(),paste0("DB/files_raw/",input$markerDTfileCleanMarker))) # get(input$dataset)
      v1 <- which(colnames(mydata) == input$geno2CleanMarker)
      v2 <- input$firstMarkerColumn
      v3 <- ncol(mydata)
      cgiarPIPE::cleanm(markerDTfile = input$markerDTfileCleanMarker,
             geno=input$geno2CleanMarker,
             missingData=input$missingData, useColumns=c(v1,v2:v3),
             wd="DB"
      )
    })
    output$outCleanMarker <- renderPrint({
      outCleanMarker()
    })

  })
}

## To be copied in the UI
# mod_cleanmApp_ui("cleanmApp_1")

## To be copied in the server
# mod_cleanmApp_server("cleanmApp_1")
