#' cleanpApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'

mod_cleanpApp_ui <- function(id){
  ns <- NS(id)
  tagList(
    ##############################################################################
    # BODY #######################################################################

    tags$div(
      fluidRow(
        shinydashboard::box(width = 12, status = "primary", background =  "orange", collapsible = TRUE,
                            tags$body(
                              p(strong('Requirements:'),strong("1)"),"Phenotype file in csv format stored in the 'files_raw' folder to be the input file.", strong("2)"), "Select traits to be cleaned.")
                            )
        ),
        shinydashboard::box(status="primary", width = 4,
                            selectInput(ns('phenoDTfileCleanPheno'), 'Choose Dataset', allFilesInRawList),
                            uiOutput(ns('pipelineCleanPheno')),
                            uiOutput(ns('stageCleanPheno')),
                            uiOutput(ns('yearCleanPheno')),
                            uiOutput(ns('seasonCleanPheno')),
                            uiOutput(ns('locationCleanPheno')),
                            uiOutput(ns('countryCleanPheno'))
        ),
        shinydashboard::box(status="primary", width = 4,
                            uiOutput(ns('trialCleanPheno')),
                            uiOutput(ns('designCleanPheno')),
                            uiOutput(ns('genoCleanPheno')),
                            uiOutput(ns('genoCodeCleanPheno')),
                            uiOutput(ns('genoYearOriginCleanPheno')),
                            uiOutput(ns('repCleanPheno')),
                            uiOutput(ns('blockCleanPheno')),
                            uiOutput(ns('rowcoordCleanPheno'))
        ),
        shinydashboard::box(status="primary", width = 4,
                            uiOutput(ns('colcoordCleanPheno')),
                            uiOutput(ns('entryTypeCleanPheno')),
                            uiOutput(ns('traitCleanPheno')),
                            selectInput(ns("fieldinstCleanPheno"),
                                        label = "Field instance forming columns",
                                        choices = list("year", "season", "location", "country", "trial","stage","pipeline","design"),
                                        selected = c("year", "season", "location"), multiple=TRUE),
                            # Input workspace (widget)
                            numericInput(ns("outlierCoefCleanPheno"), label = "Outlier coefficient", value = 1.5),
                            # Input workspace (widget)
                            numericInput(ns("traitLBCleanPheno"), label = "Trait(s) lower bound(s)", value = 0.01),
                            # Input workspace (widget)
                            numericInput(ns("traitUBCleanPheno"), label = "Trait(s) upper bound(s)", value = Inf),
                            # action button
                            actionButton(ns("runCleanPheno"), "Run")
        ),
        shinydashboard::box(status="primary", width = 12,
                            title = "Analysis status:", solidHeader = TRUE,
                            textOutput(ns("outCleanPheno"))
        ),
        shinydashboard::box(status="primary", width = 12,
                            title = "Data preview of chosen dataset:", solidHeader = TRUE,
                            DT::dataTableOutput(ns("tableCleanPheno"))
        )
      ),
      class = "tab-content"
    )

  )
}

#' cleanpApp Server Functions
#'
#' @noRd
mod_cleanpApp_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # cleanpheno $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    ## render result of selected file
    output$tableCleanPheno <- DT::renderDataTable(utils::read.csv(file.path(getwd(),paste0("DB/files_raw/",input$phenoDTfileCleanPheno))),
                                                  options = list(
                                                    pageLength = 3,
                                                    initComplete = I("function(settings, json) {alert('Done.');}")
                                                  )
    )
    ## render result of selected file
    output$pipelineCleanPheno = renderUI({
      mydata = utils::read.csv(file.path(getwd(),paste0("DB/files_raw/",input$phenoDTfileCleanPheno))) # get(input$dataset)
      selectInput(ns('pipeline2CleanPheno'), 'Pipeline column name', choices=c("ABSCENT",names(mydata)), selected = "ABSCENT" )
    })
    output$stageCleanPheno = renderUI({
      mydata = utils::read.csv(file.path(getwd(),paste0("DB/files_raw/",input$phenoDTfileCleanPheno))) # get(input$dataset)
      selectInput(ns('stage2CleanPheno'), 'Stage column name', choices=c("ABSCENT",names(mydata)), selected = "ABSCENT" )
    })
    output$yearCleanPheno = renderUI({
      mydata = utils::read.csv(file.path(getwd(),paste0("DB/files_raw/",input$phenoDTfileCleanPheno))) # get(input$dataset)
      selectInput(ns('year2CleanPheno'), 'Year column name', choices=c("ABSCENT",names(mydata)), selected = "ABSCENT" )
    })
    output$seasonCleanPheno = renderUI({
      mydata = utils::read.csv(file.path(getwd(),paste0("DB/files_raw/",input$phenoDTfileCleanPheno))) # get(input$dataset)
      selectInput(ns('season2CleanPheno'), 'Season column name', choices=c("ABSCENT",names(mydata)), selected = "ABSCENT" )
    })
    output$locationCleanPheno = renderUI({
      mydata = utils::read.csv(file.path(getwd(),paste0("DB/files_raw/",input$phenoDTfileCleanPheno))) # get(input$dataset)
      selectInput(ns('location2CleanPheno'), 'Location column name', choices=c("ABSCENT",names(mydata)), selected = "ABSCENT" )
    })
    output$countryCleanPheno = renderUI({
      mydata = utils::read.csv(file.path(getwd(),paste0("DB/files_raw/",input$phenoDTfileCleanPheno))) # get(input$dataset)
      selectInput(ns('country2CleanPheno'), 'Country column name', choices=c("ABSCENT",names(mydata)), selected = "ABSCENT" )
    })
    output$trialCleanPheno = renderUI({
      mydata = utils::read.csv(file.path(getwd(),paste0("DB/files_raw/",input$phenoDTfileCleanPheno))) # get(input$dataset)
      selectInput(ns('trial2CleanPheno'), 'Trial column name',choices=c("ABSCENT",names(mydata)), selected = "ABSCENT" )
    })
    output$designCleanPheno = renderUI({
      mydata = utils::read.csv(file.path(getwd(),paste0("DB/files_raw/",input$phenoDTfileCleanPheno))) # get(input$dataset)
      selectInput(ns('design2CleanPheno'), 'Design column name', choices=c("ABSCENT",names(mydata)), selected = "ABSCENT" )
    })
    output$genoCleanPheno = renderUI({
      mydata = utils::read.csv(file.path(getwd(),paste0("DB/files_raw/",input$phenoDTfileCleanPheno))) # get(input$dataset)
      selectInput(ns('geno2CleanPheno'), 'Genotype column name', choices=c("ABSCENT",names(mydata)), selected = "ABSCENT" )
    })
    output$genoCodeCleanPheno = renderUI({
      mydata = utils::read.csv(file.path(getwd(),paste0("DB/files_raw/",input$phenoDTfileCleanPheno))) # get(input$dataset)
      selectInput(ns('genoCode2CleanPheno'), 'Genotype Code column name',choices=c("ABSCENT",names(mydata)), selected = "ABSCENT" )
    })
    output$genoYearOriginCleanPheno = renderUI({
      mydata = utils::read.csv(file.path(getwd(),paste0("DB/files_raw/",input$phenoDTfileCleanPheno))) # get(input$dataset)
      selectInput(ns('genoYearOrigin2CleanPheno'), 'Genotype year of origin',choices=c("ABSCENT",names(mydata)), selected = "ABSCENT" )
    })
    output$repCleanPheno = renderUI({
      mydata = utils::read.csv(file.path(getwd(),paste0("DB/files_raw/",input$phenoDTfileCleanPheno))) # get(input$dataset)
      selectInput(ns('rep2CleanPheno'), 'Replicate column name', choices=c("ABSCENT",names(mydata)), selected = "ABSCENT" )
    })
    output$blockCleanPheno = renderUI({
      mydata = utils::read.csv(file.path(getwd(),paste0("DB/files_raw/",input$phenoDTfileCleanPheno))) # get(input$dataset)
      selectInput(ns('block2CleanPheno'), 'Block column name', choices=c("ABSCENT",names(mydata)), selected = "ABSCENT" )
    })
    output$rowcoordCleanPheno = renderUI({
      mydata = utils::read.csv(file.path(getwd(),paste0("DB/files_raw/",input$phenoDTfileCleanPheno))) # get(input$dataset)
      selectInput(ns('rowcoord2CleanPheno'), 'Row coordinate column', choices=c("ABSCENT",names(mydata)), selected = "ABSCENT" )
    })
    output$colcoordCleanPheno = renderUI({
      mydata = utils::read.csv(file.path(getwd(),paste0("DB/files_raw/",input$phenoDTfileCleanPheno))) # get(input$dataset)
      selectInput(ns('colcoord2CleanPheno'), 'Col coordinate column', choices=c("ABSCENT",names(mydata)), selected = "ABSCENT" )
    })
    output$entryTypeCleanPheno = renderUI({
      mydata = utils::read.csv(file.path(getwd(),paste0("DB/files_raw/",input$phenoDTfileCleanPheno))) # get(input$dataset)
      selectInput(ns('entryType2CleanPheno'), 'Entry type column', choices=c("ABSCENT",names(mydata)), selected = "ABSCENT" )
    })
    output$traitCleanPheno = renderUI({
      mydata = utils::read.csv(file.path(getwd(),paste0("DB/files_raw/",input$phenoDTfileCleanPheno))) # get(input$dataset)
      selectInput(ns('trait2CleanPheno'), 'Trait(s) to clean', names(mydata), multiple = TRUE)
    })
    outCleanPheno <- eventReactive(input$runCleanPheno, {
      cgiarPIPE::cleanp(phenoDTfile = input$phenoDTfileCleanPheno,
             pipeline=input$pipeline2CleanPheno,
             stage= input$stage2CleanPheno, year= input$year2CleanPheno, season= input$season2CleanPheno,
             location= input$location2CleanPheno, country= input$country2CleanPheno,
             trial= input$trial2CleanPheno, design= input$design2CleanPheno,geno= input$geno2CleanPheno,
             genoCode= input$genoCode2CleanPheno, genoYearOrigin= input$genoYearOrigin2CleanPheno,
             rep= input$rep2CleanPheno, block= input$block2CleanPheno,rowcoord= input$rowcoord2CleanPheno,
             colcoord= input$colcoord2CleanPheno,entryType= input$entryType2CleanPheno,
             fieldinst = input$fieldinstCleanPheno,
             # trait parameters
             trait= input$trait2CleanPheno, wd="DB", verbose=FALSE
      )
    })
    output$outCleanPheno <- renderPrint({
      outCleanPheno()
    })

  })
}

## To be copied in the UI
# mod_cleanpApp_ui("cleanpApp_1")

## To be copied in the server
# mod_cleanpApp_server("cleanpApp_1")
