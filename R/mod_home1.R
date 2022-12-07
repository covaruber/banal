#' home1 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_home1_ui <- function(id){
  ns <- NS(id)
  tagList(

    tags$div(
      fluidRow(
        shinydashboard::box(width = 12, status = "primary", collapsible = FALSE,
            tags$body(
              p(strong("Description:"),strong(""),"the analytics framework has been designed
                          to be database agnostic. It requires an initial raw phenotype, marker or pedigree file that can be used
                          to match the present columns to expected columns in the analytical pipeline.
                          After that, the workflow can be observed in the figure below moving in a top-down direction " )
            )
        ),
        shinydashboard::box(status="primary", width = 12,
            title = "Workflow for the Biometrics Analyses Tool", solidHeader = TRUE,
            img(src = "www/processAnalysis.jpg", height = 400, width = 700), # add an image
        )
      ),
      class = "tab-content"
    )

  )
}

#' home1 Server Functions
#'
#' @noRd
mod_home1_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_home1_ui("home1_1")

## To be copied in the server
# mod_home1_server("home1_1")
