#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # PAGE #########################################################################
    shinydashboard::dashboardPage(
      ##############################################################################
      # HEADER #####################################################################
      shinydashboard::dashboardHeader(title="IRRI Biometrics Analyses Tool"),
      ##############################################################################
      # SIDEBAR ####################################################################
      shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(
          shinydashboard::menuItem(strong("HOME ANALYTICS"), tabName = "homeAnalysis", icon = icon("house")
          ),
          shinydashboard::menuItem("Cleaning analyses", tabName = "cleaning", icon = icon("soap"),
                                   shinydashboard::menuSubItem("Clean/match phenotypes", tabName = "cleanpheno", icon = icon("soap")),
                                   shinydashboard::menuSubItem("Clean/match markers", tabName = "cleanmarker", icon = icon("soap")),
                                   shinydashboard::menuSubItem("Clean/match pedigree", tabName = "cleanpedigree", icon = icon("soap"))
          ),
          shinydashboard::menuItem("Relationship analyses", tabName = "relationships", icon = icon("diagram-project"),
                                   shinydashboard::menuSubItem("Numerator relationship", tabName = "nrm", icon = icon("diagram-project")),
                                   shinydashboard::menuSubItem("Genomic relationship", tabName = "grm", icon = icon("diagram-project"))
          ),
          shinydashboard::menuItem("Prediction analyses", tabName = "predictions", icon = icon("network-wired"),
                                   shinydashboard::menuSubItem("Single-Trial Analysis", tabName = "sta", icon = icon("network-wired")),
                                   shinydashboard::menuSubItem("Multi-Trial Analysis", tabName = "met", icon = icon("network-wired")),
                                   shinydashboard::menuSubItem("Selection Index", tabName = "index", icon = icon("network-wired")),
                                   shinydashboard::menuSubItem("Optimal Crossing", tabName = "ocs", icon = icon("network-wired")),
                                   shinydashboard::menuSubItem("Marker Effects", tabName = "gwa", icon = icon("network-wired"))
          ),
          shinydashboard::menuItem("Metrics analyses", tabName = "metrics", icon = icon("chart-line"),
                                   shinydashboard::menuSubItem("Realized Gain", tabName = "rgg", icon = icon("chart-line")),
                                   shinydashboard::menuSubItem("Predicted Gain", tabName = "pgg", icon = icon("chart-line"))
          )
        )
      ),
      ##############################################################################
      # BODY #######################################################################
      shinydashboard::dashboardBody(
        shinydashboard::tabItems(
          ## CLEAN/MATCH ANALYSES
          shinydashboard::tabItem(tabName = "homeAnalysis", # $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
                                  mod_home1_ui("home1_1")
          ),
          shinydashboard::tabItem(tabName = "cleanpheno", # $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
                                  mod_cleanpApp_ui("cleanpApp_1")
          ),
          shinydashboard::tabItem(tabName = "cleanmarker", # $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
                                  mod_cleanmApp_ui("cleanmApp_1")
          ),
          shinydashboard::tabItem(tabName = "cleanpedigree", # $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
                                  mod_cleanrApp_ui("cleanrApp_1")
          ),
          ## RELATONSHIP ANALYSES
          shinydashboard::tabItem(tabName = "nrm", # $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
                                  mod_nrmApp_ui("nrmApp_1")
          ),
          shinydashboard::tabItem(tabName = "grm", # $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
                                  mod_grmApp_ui("grmApp_1")
          ),
          ## PREDICTION ANALYSES
          shinydashboard::tabItem(tabName = "sta", # $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
                                  mod_staApp_ui("staApp_1")
          ),
          shinydashboard::tabItem(tabName = "met", # $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
                                  mod_metApp_ui("metApp_1")
          ),
          shinydashboard::tabItem(tabName = "index", # $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
                                  mod_indexApp_ui("indexApp_1")
          ),
          shinydashboard::tabItem(tabName = "ocs", # $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
                                  mod_ocsApp_ui("ocsApp_1")
          ),
          shinydashboard::tabItem(tabName = "gwa", # $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
                                  mod_gwaApp_ui("gwaApp_1")
          ),
          ## METRICS ANALYSES
          shinydashboard::tabItem(tabName = "rgg", # $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
                                  mod_rggApp_ui("rggApp_1")
          ),
          shinydashboard::tabItem(tabName = "pgg", # $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
                                  mod_pggApp_ui("pggApp_1")
          )
        )
      )
    ) # PAGE END ###################################################################
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "banal"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
