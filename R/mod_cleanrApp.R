#' cleanrApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_cleanrApp_ui <- function(id){
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

#' cleanrApp Server Functions
#'
#' @noRd
mod_cleanrApp_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_cleanrApp_ui("cleanrApp_1")

## To be copied in the server
# mod_cleanrApp_server("cleanrApp_1")
