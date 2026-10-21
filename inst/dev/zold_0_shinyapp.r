#' #' @include EmStanS-package.r
#' NULL
#'
#' #' The Shiny App UI
#' #'
#' #' @noRd
#' shiny_ui <- function() {
#'   fluidPage(
#'     # useShinydashboard(),
#'     # tags$head(
#'     #   tags$link(rel = "stylesheet", type = "text/css",
#'     #             href = file.path("inst","extdata", "theme.css"))
#'     # ),
#'     theme=bs_theme(version = 4,
#'                    bootswatch = "journal",
#'                    primary = "#ED79F9",
#'                    base_font = font_google("Work Sans") #
#'     ),
#'
#'     navbarPage(
#'
#'       # title -------------------------------------------------
#'       title = "EmStanS Lite (0.0.0)",
#'
#'       #---------------------------------------------------------
#'       # Data Import
#'       #---------------------------------------------------------
#'       tabPanel("Data Import",
#'                fluidRow(
#'                  column(2,
#'                         prettyRadioButtons("empirical",
#'                                            label = "Empiricial or Simulated",
#'                                            choices = c("empirical", "simulated"),
#'                                            selected = "empirical",
#'                                            status = "danger",
#'                                            icon = icon("check"),
#'                                            bigger = TRUE,
#'                                            animation = 'smooth')
#'                  ),
#'                  column(10,
#'                         uiOutput("data_import")
#'                  )
#'                )
#'       ),
#'
#'       tabPanel("Analysis",
#'
#'                fluidRow(
#'                  column(1,
#'                         actionButton("run1", "Run")
#'                  ),
#'
#'                  column(1,
#'                         materialSwitch(
#'                           inputId = "WESS",
#'                           label = strong("Count"),
#'                           value = TRUE,
#'                           status = "danger")
#'                  ),
#'
#'
#'                  column(1,strong("Weight")),
#'
#'                  column(1,
#'                         switchInput(
#'                           inputId = "gamest",
#'                           label = "Smoothing",
#'                           value = F,
#'                           onStatus = "danger",
#'                           offStatus = "sucess",
#'                           labelWidth = "80px")
#'                  )
#'                ),
#'
#'                fluidRow(
#'
#'
#'                  column(width = 5,align = 'left',
#'                         dataTableOutput("result0")
#'
#'                  ),
#'                  column(width = 5,offset = 1, align = 'right',
#'                         plotlyOutput("result2",
#'                                      width = "100%",
#'                                      height = "600px",inline = TRUE)
#'                  )
#'
#'                )
#'       ),
#'
#'       tabPanel("Review",
#'
#'                DTOutput("result1")
#'       )
#'     )
#'   )##########################  shiny UI last line  #######################
#' }
#'
#' #' The Shiny App Server
#' #'
#' #' @param input a shiny input
#' #' @param output a shiny output
#' #' @param session a session
#' #' @noRd
#' shiny_server <- function(input, output, session) {
#'   #
#'   # Data part ------------------------------------------
#'   output$data_import <- renderUI({
#'     if(input$empirical == "empirical") {
#'       fluidRow(
#'         column(4,
#'                fileInput("setups", "Choose csv file for Setup",
#'                          multiple = FALSE, accept = c("csv")),
#'                textInput("lvname", "Level name (comma separated)"),
#'
#'
#'                actionButton("import", "IMPORT")
#'         ),
#'
#'         column(8,
#'                tabsetPanel(
#'                  # verbatimTextOutput("gen_data")
#'                  tabPanel("Imported data",DTOutput("gen_data"))
#'                ))
#'       )
#'     } else {
#'       fluidRow(
#'         column(2,
#'                numericInput(
#'                  inputId = "nitem",
#'                  label = "Num of Items (Max:1000)",
#'                  value = 30, min = 10, max = 1000, step = 1),
#'
#'                numericInput(
#'                  inputId = "nlevel",
#'                  label = "Num of levels (Max:5)",
#'                  value = 3, min = 2,max = 5, step = 1),
#'
#'                numericInput(
#'                  inputId = "cor_value",
#'                  label = "Correlation bw Loc and ALD",
#'                  value = 0.6, min = 0, max = 1),
#'
#'                numericInput(
#'                  inputId = "sd",
#'                  label = "SD",
#'                  value = 1, min = 1, max = 100, step = 1),
#'
#'                numericInput(
#'                  inputId = "ec",
#'                  label = "EC",
#'                  value = 0, min = 0, max = 100, step = 1),
#'
#'                actionButton("import", "Import data")),
#'
#'         column(10,
#'                downloadButton("sim_data_down", "Download"),
#'                tabsetPanel(
#'                  tabPanel("Imported data",DTOutput("gen_data"))
#'                )
#'         )
#'       )
#'     }
#'   })
#'
#'
#'   imprt_data <- eventReactive(input$import, {
#'     if(input$empirical == "empirical") {
#'
#'       inpdata <- read.csv(input$setups$datapath)
#'       inpdata[[1]] <- 1:nrow(inpdata)
#'       names(inpdata) <- c("OOD","location","ALD")
#'
#'       return(inpdata)
#'
#'     } else {
#'       nitem       <- input$nitem
#'       nlevel      <- input$nlevel
#'       cor_value   <- input$cor_value
#'       sd          <- input$sd
#'       ec          <- input$ec
#'
#'       inpdata <-
#'         genFakeData(
#'           fun = 'runif',
#'           cor_value = cor_value,
#'           nlevel = nlevel,
#'           n = nitem,
#'           100,
#'           300)
#'     }
#'
#'     return(inpdata)
#'   })
#'
#'   observeEvent(imprt_data(), {
#'
#'     output$gen_data <- renderDT({
#'       imprt_data()})
#'   })
#'
#'
#'   output$sim_data_down <- downloadHandler(
#'     filename = function() {
#'       paste("simulated_data", ".csv", sep = "")
#'     },
#'     content = function(file) {
#'       write.csv(imprt_data(), file, row.names = F)
#'     }
#'   )
#'
#'
#'   res_emstans <- eventReactive(input$run1, {
#'
#'     if(input$empirical == "empirical") {
#'       lvname <- input$lvname
#'       # lvname <- c("Level1,Level2,Level3")
#'       lvname <- str_remove_all(lvname, " ")
#'       lvname <- str_split(lvname, ",", simplify = T)[1,]
#'       # lvname <- c("Level1","Level2","Level3")
#'
#'     } else {
#'       lvname <- paste0("Level",1:input$nlevel)
#'     }
#'
#'     WESS   <- input$WESS
#'     gamest <- input$gamest
#'
#'     res <- emstans(data = imprt_data(), lvname = lvname, WESS = WESS, GAM = gamest)
#'     # res <- emstans(data = inpdata, lvname = lvname)
#'     # res[[1]]
#'     return(res)
#'   })
#'
#'
#'   observeEvent(input$run1, {
#'
#'     output$result0 <- renderDataTable({
#'       res_emstans()[[1]]
#'     })
#'
#'     output$result1 <- renderDataTable({
#'       res_emstans()[[2]]
#'     })
#'
#'
#'     output$result2 <- renderPlotly({
#'
#'       res_emstans()[[3]]
#'     })
#'
#'   })
#'
#'
#'   output$ess_weight <- downloadHandler(
#'     filename = function() {
#'       paste("ess_weight", ".csv", sep = "")
#'     },
#'     content = function(file) {
#'       write.csv(res_emstans()[[1]], file, row.names = F)
#'     }
#'   )
#'
#'   output$review <- downloadHandler(
#'     filename = function() {
#'       paste("review", ".csv", sep = "")
#'     },
#'     content = function(file) {
#'       write.csv(res_emstans()[[2]], file, row.names = F)
#'     }
#'   )
#' }##################  Shiny Server last line   ############################
