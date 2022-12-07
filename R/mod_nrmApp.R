#' nrmApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_nrmApp_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$div(
      fluidRow(
        shinydashboard::box(status="primary", width = 12,
            h3("Under development")
        )
      ),
      class = "tab-content"
    )
  )
}

#' nrmApp Server Functions
#'
#' @noRd
mod_nrmApp_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_nrmApp_ui("nrmApp_1")

## To be copied in the server
# mod_nrmApp_server("nrmApp_1")
