#' Shiny app server object
#'
#' @importFrom graphics hist
#' @import shiny

# create the shiny application user interface



library(shinyFiles)
library(data.table)
library(shinythemes)
library(shinyjs)

#library(ggrepel, rio, astrochron, ggiraph, shinyjs, caret, shinyFiles, stringr, data.table, tibble, ggrepel, shinythemes, shinyWidgets, stats, utils)


jscode <- "shinyjs.closeWindow = function() { window.close(); }"



shinyAppUI <- fluidPage(
  fluidPage(theme = shinythemes::shinytheme("spacelab"),
            useShinyjs(),
            extendShinyjs(text = jscode, functions = c("closeWindow")),
            navbarPage("MatchApp App", id = "tabs",
                       tabPanel("Top Data Import",
                                h3("Import Data to the Top Plot"),
                                sidebarLayout(
                                  sidebarPanel(
                                    fileInput("Top", "Choose Data File", multiple = FALSE, accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
                                    tags$hr(),
                                    checkboxInput("header", "Header", TRUE),
                                    radioButtons("sep", "Separator",
                                                 choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                                                 selected = ","),
                                    radioButtons("quote", "Quote",
                                                 choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"),
                                                 selected = '"'),
                                    tags$hr(),
                                    radioButtons("disp", "Display",
                                                 choices = c(Head = "head", All = "all"), selected = "head"),
                                    #checkboxGroupInput("FinalTopTransformations", "Data Transformations:",
                                    #                   choiceNames = list(("Zero-Mean"), ("Any Other?")),
                                    #                   choiceValues = list("ZeroMean", "AnyOther") #https://shiny.rstudio.com/reference/shiny/latest/checkboxGroupInput.html
                                    #),
                                    
                                    #~~~~~~~~~~~~~~~
                                    
                                    uiOutput("FinalTopVarX"),
                                    uiOutput("FinalTopVarY"),
                                    br(),
                                    actionButton("FinalShowTopPlot", "Update Top Plot"),
                                  ),
                                  
                                  mainPanel(
                                    tableOutput("FinalTopContents"),
                                  )
                                )
                       ),
                       
                       tabPanel("Bottom Data Import",
                                h3("Import Data to the Bottom Plot"),
                                
                                sidebarLayout(
                                  sidebarPanel(
                                    fileInput("Bottom", "Choose Data File", multiple = FALSE, accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
                                    tags$hr(),
                                    checkboxInput("headerBottom", "Header", TRUE),
                                    radioButtons("sepBottom", "Separator",
                                                 choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                                                 selected = ","),
                                    radioButtons("quoteBottom", "Quote",
                                                 choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"),
                                                 selected = '"'),
                                    tags$hr(),
                                    radioButtons("dispBottom", "Display",
                                                 choices = c(Head = "headBottom", All = "allBottom"), selected = "headBottom"),
                                    #heckboxGroupInput("FinalBottomTransformations", "Data Transformations:",
                                    #                  choiceNames = list(("Zero-Mean"), ("Any Other?")),
                                    #                  choiceValues = list("ZeroMeanBottom", "AnyOther") #https://shiny.rstudio.com/reference/shiny/latest/checkboxGroupInput.html
                                    
                                    
                                    #~~~~~~~~~~~~~~~
                                    
                                    uiOutput("FinalBottomVarX"),
                                    uiOutput("FinalBottomVarY"),
                                    br(),
                                    actionButton("FinalShowBottomPlot", "Update Bottom Plot"),
                                  ),
                                  
                                  mainPanel(
                                    tableOutput("FinalBottomContents"),
                                  )
                                )
                       ),
                       tabPanel("Graphs",
                                h3("Graphs"),
                                fluidRow(
                                  column(6,numericInput("FinalRowNumber", label = h3("Tie Point Number"), value = 1)),
                                  downloadButton("downloadEmptyData", "Download New Tie File"),
                                  #column(6, fileInput("TiePointFile", label = "Upload Tie Point File", multiple = FALSE, accept = c(".tie")),) #Old Tie upload
                                  shinyFilesButton('files', 'File select', 'Please select a file', FALSE), #New Tie file import
                                ),
                                tableOutput("TiePShow"),
                                actionButton(
                                  inputId = "launchDelete",
                                  label = "Clear Tie Point Data (This Row Only)"
                                ),
                                actionButton(
                                  inputId = "FinalCheck",
                                  label = "Finalize Tie File"
                                ),
                                fluidRow(
                                  column(6,
                                         
                                  ),
                                  column(6,
                                         
                                  ),
                                ),
                                uiOutput("SliderTopX"),
                                uiOutput("SliderTopY"),
                                plotOutput("FullTopPlot", click = "TopPlot_click"),
                                uiOutput("SliderBotX"),
                                uiOutput("SliderBotY"),
                                plotOutput("FullBottomPlot", click = "BottomPlot_click"),
                                
                                
                                
                       ),
                       tabPanel("Relative Accumulation Rate or C code",
                                h3("This is an extra panel. If the C++ can be integrated, the button to run it can go here."),
                                h3("Likely, there will be a button to run the C++ then open a new window depending on how the C++ code is written"),
                       ),
                       tabPanel("Close")
            )
  )
)



