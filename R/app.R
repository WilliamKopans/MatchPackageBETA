#library(ggplot2, warn.conflicts = FALSE) #The conflicts that I am turning off are those which occur when libraries share function names
#library(dplyr, warn.conflicts = FALSE) #For more information, check: https://stackoverflow.com/questions/39137110/what-does-the-following-object-is-masked-from-packagexxx-mean
#library(shiny, warn.conflicts = FALSE)
#library(rio, warn.conflicts = FALSE)
#library(astrochron, warn.conflicts = FALSE)
#library(plotly, warn.conflicts = FALSE)
#library(ggiraph, warn.conflicts = FALSE)
#library(shinyjs, warn.conflicts = FALSE)
#library(caret, warn.conflicts = FALSE)
#library(shinyFiles, warn.conflicts = FALSE)
#library(stringr, warn.conflicts = FALSE)
#library(data.table, warn.conflicts = FALSE)
#library(tibble, warn.conflicts = FALSE)
#library(ggrepel, warn.conflicts = FALSE)
#library(shinythemes, warn.conflicts = FALSE)
#library(shinyWidgets, warn.conflicts = FALSE)
                      #   **Main App File**
#' @import ggrepel
#' @import rio
#' @import astrochron
#' @import ggiraph
#' @import shinyjs
#' @import caret
#' @import shinyFiles
#' @import stringr
#' @import data.table
#' @import tibble
#' @import ggrepel
#' @import shinythemes
#' @import shinyWidgets
#' @import stats
#' @import utils
#' @import dplyr



###

# launches the shinyAppDemo app
#'
#' @export launchApp
#'
#' @return shiny application object
#'
#' @example \dontrun {Match::launchApp()}
#'
#' @import shiny


# wrapper for shiny::shinyApp()
launchApp <- function() {
  message('Dev Version 0.1')
  shiny::shinyApp(ui = ui, server = server)
}



###
  
  
  

library(shinyFiles)
library(data.table)
library(shinythemes)
library(shinyjs)
library(dplyr) 
library(tibble)

#library(ggrepel, rio, astrochron, ggiraph, shinyjs, caret, shinyFiles, stringr, data.table, tibble, ggrepel, shinythemes, shinyWidgets, stats, utils)

jscode <- "shinyjs.closeWindow = function() { window.close(); }"

ui <- fluidPage(theme = shinythemes::shinytheme("spacelab"),
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
                                    uiOutput("SliderTopX"),
                                    uiOutput("SliderTopY"),
                                    column(3,numericInput("CoreTop", label = ("Core Number"), value = 0)),
                                    plotOutput("FullTopPlot", click = "TopPlot_click"),
                                    br(),
                                    br(),
                                    uiOutput("SliderBotX"),
                                    uiOutput("SliderBotY"),
                                    column(3,numericInput("CoreBottom", label = ("Core Number"), value = 1)),
                                    plotOutput("FullBottomPlot", click = "BottomPlot_click"),
                                    
                                    
                           ),
                           tabPanel("Relative Accumulation Rate or C code",
                                    h3("This is an extra panel. If the C++ can be integrated, the button to run it can go here."),
                                    h3("Likely, there will be a button to run the C++ then open a new window depending on how the C++ code is written"),
                           ),
                           tabPanel("Exit Application")
                )
)



server <- function(input, output) {
  
  StructuredTopData <- reactive({
    req(input$Top)
    tryCatch(
      {dfTop <- read.csv(input$Top$datapath, header = input$header, sep = input$sep, quote = input$quote)},
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))})
    if("ZeroMean" %in% input$FinalTopTransformations){
      dfTop <- stats::predict(preProcess(dfTop, method=c("center", "scale")), dfTop) #Zero-Meaned data
      #INDEX <- which(colnames(dfTop)==input$dynamicTopY)
      #dfTop[, INDEX] <- predict(preProcess(as.data.frame(dfTop[, INDEX])), as.data.frame(dfTop[, INDEX])) #Zero-Meaned data
      #print(summary(dfTop))
      return(dfTop)
    } else {return(dfTop)}
    
    
    
  })
  
  output$FinalTopContents <- renderTable({
    req(input$Top)
    
    if(input$disp == "head") {
      TopData <- (head(StructuredTopData()))}
    else {
      TopData <- StructuredTopData()
    }
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~
  observeEvent(input$Top, {
    req(input$Top)
    
    output$FinalTopVarX <- renderUI({
      selectInputTopX = selectInput("dynamicTopX", "X", choices = c(as.list(names(StructuredTopData()))))
    })
    output$FinalTopVarY <- renderUI({
      selectInputTopY = selectInput("dynamicTopY", "Y", choices = c(as.list(names(StructuredTopData()))))
    })
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  observeEvent(input$FinalShowTopPlot, {
 
    output$FullTopPlot <- renderPlot({
      ggplot2::ggplot()+
        ggplot2::geom_line(StructuredTopData(), mapping = ggplot2::aes_string(x= input$dynamicTopX, y = input$dynamicTopY)) +
        ggplot2::geom_point(StructuredTopData(), mapping = ggplot2::aes_string(x= input$dynamicTopX, y = input$dynamicTopY),alpha = 0.3) +
        ggplot2::theme_bw()+
        ggplot2::coord_cartesian(ylim = c(input$YRangeTop[1], input$YRangeTop[2]), xlim = c(input$XrangeTop[1], input$XrangeTop[2]), expand = FALSE) + TopGeom()
      
      
    })
  })
  
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  StructuredBottomData <- reactive({
    req(input$Bottom)
    tryCatch(
      {dfBottom <- read.csv(input$Bottom$datapath, header = input$headerBottom, sep = input$sepBottom, quote = input$quoteBottom)},
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))})
    if("ZeroMeanBottom" %in% input$FinalBottomTransformations){
      dfBottom <- predict(preProcess(dfBottom, method=c("center", "scale")), dfBottom) #Zero-Meaned data
      return(dfBottom)
    } else {return(dfBottom)}
    
    
    
  })
  
  output$FinalBottomContents <- renderTable({
    req(input$Bottom)
    
    if(input$dispBottom == "headBottom") {
      BottomData <- (head(StructuredBottomData()))}
    else {
      BottomData <- StructuredBottomData()
    }
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~
  observeEvent(input$Bottom, {
    req(input$Bottom)
    
    output$FinalBottomVarX <- renderUI({
      selectInputBottomX = selectInput("dynamicBottomX", "X", choices = c(as.list(names(StructuredBottomData()))))
    })
    output$FinalBottomVarY <- renderUI({
      selectInputBottomY = selectInput("dynamicBottomY", "Y", choices = c(as.list(names(StructuredBottomData()))))
    })
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  TopGeom <- reactive({
    
    if (isTruthy(input$files) == TRUE && isTruthy(input$Bottom$datapath) == TRUE && isTruthy(input$Top$datapath) == TRUE) {
      if (length(shinyFiles::parseFilePaths(roots, input$files)$datapath)!=0) { #New tie file import
        #print("Finding Tie Point Location")
        
        
        #Remove Scientific Notation to check:
        #Revert <- as.data.frame(lapply(lapply(ScientificNotation,as.numeric), format, scientific = F)) #Removes scientific notation
        #Revert <- as.data.frame(lapply(lapply(Revert,as.numeric), sprintf, fmt = "%s")) #Removes trailing zeros
        
        TopOriginal <- StructuredTopData()
        INDEX <- which(colnames(StructuredTopData())==input$dynamicTopX)
        TieData <- TiePointData()[,2]
        #TieData <-TieData[!is.na(TieData)]
        SameTop <- as.data.frame(StructuredTopData()[,INDEX] %in% TieData) #Check the row of Tie File
        names(SameTop) <- "Shared"
        
        TopShared <- TopOriginal %>%
          tibble::add_column(SameTop) %>%
          dplyr::filter(SameTop == TRUE)
        
        
        Main <- as.data.frame(StructuredTopData())
        names(Main)[INDEX] <- "Shared1"
        TieComp <- as.data.frame(TiePointData())
        TieComp <- format(TieComp, scientific = FALSE)
        names(TieComp)[2] <- "Shared2"
        TieComp <- tibble::rowid_to_column(TieComp, "ID")
        total <- merge(Main,TieComp, by.x="Shared1", by.y="Shared2")
        
        
        if (nrow(total)>0) {
          INDEXFin <- which(colnames(total)==input$dynamicTopY)
          INDEXFinSh <- which(colnames(total)=="Shared1")
          TopGeom <- list(ggplot2::geom_point(data = total, mapping = ggplot2::aes(x = total[,INDEXFinSh], y = total[,INDEXFin]), color ='dodgerblue', shape = 13, size = 9),
                          ggrepel::geom_label_repel(ggplot2::aes(x = total[,INDEXFinSh], y = total[,INDEXFin], label = total$ID), box.padding   = 0.35,  point.padding = 0.5, segment.color = 'grey50'),
                          ggplot2::geom_point(data = total, mapping = ggplot2::aes(x = total[,INDEXFinSh], y = total[,INDEXFin]), color ='dodgerblue', shape = 18, size = 3.5)
          )
          return(TopGeom)
        } else {
          TopGeom <- ggplot2::geom_blank()
          return(TopGeom)
        }
        
        
      }
    } else {
      TopGeom <- ggplot2::geom_blank()
      return(TopGeom)
    }
  })
  
  BottomGeom <- reactive({
    
    if (isTruthy(input$files) == TRUE && isTruthy(input$Bottom$datapath) == TRUE && isTruthy(input$Top$datapath) == TRUE) {
      if (length(parseFilePaths(roots, input$files)$datapath)!=0) { #New tie file import
        
        
        INDEX <- which(colnames(StructuredBottomData())==input$dynamicBottomX)
        
        Main <- as.data.frame(StructuredBottomData())
        names(Main)[INDEX] <- "Shared1"
        TieComp <- as.data.frame(TiePointData())
        TieComp <- format(TieComp, scientific = FALSE)
        names(TieComp)[4] <- "Shared2"
        TieComp <- tibble::rowid_to_column(TieComp, "ID")
        total <- merge(Main,TieComp, by.x="Shared1", by.y="Shared2")
        
        
        if (nrow(total)>0) {
          INDEXFin <- which(colnames(total)==input$dynamicBottomY)
          INDEXFinSh <- which(colnames(total)=="Shared1")
          BottomGeom <- list(ggplot2::geom_point(data = total, mapping = ggplot2::aes(x = total[,INDEXFinSh], y = total[,INDEXFin]), color ='dodgerblue', shape = 13, size = 9),
                             ggrepel::geom_label_repel(ggplot2::aes(x = total[,INDEXFinSh], y = total[,INDEXFin], label = total$ID), box.padding   = 0.35,  point.padding = 0.5, segment.color = 'grey50'),
                             ggplot2::geom_point(data = total, mapping = ggplot2::aes(x = total[,INDEXFinSh], y = total[,INDEXFin]), color ='dodgerblue', shape = 18, size = 3.5)
          )
          
          return(BottomGeom)
        }
        
      }
    } else {
      BottomGeom <- ggplot2::geom_blank()
      return(BottomGeom)
    }
  })
  
  observeEvent(input$FinalShowBottomPlot, {
    output$FullBottomPlot <- renderPlot({
      
      ggplot2::ggplot()+
        ggplot2::geom_line(StructuredBottomData(), mapping = ggplot2::aes_string(x= input$dynamicBottomX, y = input$dynamicBottomY)) +
        ggplot2::geom_point(StructuredBottomData(), mapping = ggplot2::aes_string(x= input$dynamicBottomX, y = input$dynamicBottomY),alpha = 0.3) +
        ggplot2::theme_bw() +
        ggplot2::coord_cartesian(ylim = c(input$YRangeBot[1], input$YRangeBot[2]), xlim = c(input$XRangeBot[1], input$XRangeBot[2]), expand = FALSE) + BottomGeom()
      
      
    })
  })
  
  
  
  
  
  
  #~~~~~~~~~
  
  
  selectedDataTop <- reactive({
    TopClickTemp <- nearPoints(StructuredTopData(), input$TopPlot_click, maxpoints = 1)
    TopClickTemp[[input$dynamicTopX]]
  })
  observeEvent(input$TopPlot_click, {
    print(paste0("Top clicked: ",selectedDataTop()))
  })
  
  selectedDataBot <- reactive({
    BotClickTemp <- nearPoints(StructuredBottomData(), input$BottomPlot_click, maxpoints = 1)
    BotClickTemp[[input$dynamicBottomX]]
  })
  observeEvent(input$BottomPlot_click, {
    print(paste0("Bottom clicked: ",selectedDataBot()))
  })
  
  #~~~~~~~~~~~
  
  observeEvent(input$FinalShowBottomPlot, {
    req(input$Bottom)
    
    output$SliderBotX <- renderUI({
      
      SliderBotX <- sliderInput("XRangeBot", "X Range Bottom Plot:",
                                min = min(StructuredBottomData()[[input$dynamicBottomX]]), max = max(StructuredBottomData()[[input$dynamicBottomX]]),
                                value = c(min(StructuredBottomData()[[input$dynamicBottomX]]), max(StructuredBottomData()[[input$dynamicBottomX]])), width = '90%')
    })
    output$SliderBotY <- renderUI({
      SliderBotY <- sliderInput("YRangeBot", "Y Range Bottom Plot:",
                                min = min(StructuredBottomData()[[input$dynamicBottomY]]), max = max(StructuredBottomData()[[input$dynamicBottomY]]),
                                value = c(min(StructuredBottomData()[[input$dynamicBottomY]]), max(StructuredBottomData()[[input$dynamicBottomY]])), width = '90%')
    })
  })
  
  
  observeEvent(input$FinalShowTopPlot, {
    req(input$Top)
    
    output$SliderTopX <- renderUI({
      
      SliderTopX <- sliderInput("XrangeTop", "X Range Top Plot:",
                                min = min(StructuredTopData()[[input$dynamicTopX]]), max = max(StructuredTopData()[[input$dynamicTopX]]),
                                value = c(min(StructuredTopData()[[input$dynamicTopX]]), max(StructuredTopData()[[input$dynamicTopX]])), width = '90%')
    })
    output$SliderTopY <- renderUI({
      SliderTopY <- sliderInput("YRangeTop", "Y Range Top Plot:",
                                min = min(StructuredTopData()[[input$dynamicTopY]]), max = max(StructuredTopData()[[input$dynamicTopY]]),
                                value = c(min(StructuredTopData()[[input$dynamicTopY]]), max(StructuredTopData()[[input$dynamicTopY]])), width = '90%')
    })
  })
  
  
  TiePointEditTop <- observeEvent(input$TopPlot_click, {
    if (length(selectedDataTop()) != 0) {
      
      df <- TiePointData()
      df[input$FinalRowNumber,2] <- selectedDataTop()
      
      
      df[input$FinalRowNumber,1] <- input$CoreTop 
      
      
      pathtie <- toString(TieDataFilePath())
      write.table(x = df, file = pathtie, sep = " ", col.names = FALSE, row.names = FALSE, quote = FALSE)
      
    }
    
  })
  TiePointEditBottom <- observeEvent(input$BottomPlot_click, {
    if (length(selectedDataBot()) != 0) {
      df <- TiePointData()
      #numeric(0) if click off point
      df[input$FinalRowNumber,4] <- selectedDataBot()
      
      
      df[input$FinalRowNumber,3] <- input$CoreBottom 
      
      
      pathtie <- toString(TieDataFilePath())
      write.table(x = df, file = pathtie, sep = " ", col.names = FALSE, row.names = FALSE, quote = FALSE)
    }
  })
  
  
  
  
  TiePointData <- reactive({
    req(input$files)
    #invalidateLater(5000)
    React <- selectedDataTop()
    React <- selectedDataBot()
    if (isTruthy(input$files) == TRUE && isTruthy(input$Bottom$datapath) == TRUE && isTruthy(input$Top$datapath) == TRUE) {
      
      if (length(parseFilePaths(roots, input$files)$datapath)!=0) { #New tie file import
        pathtie <- toString(TieDataFilePath())
        read.table(pathtie, sep = "" , header = F,na.strings ="", stringsAsFactors= F)
      }
    }
    #read.table(file = pathtie,na.strings ="", stringsAsFactors= F, header = F)
    
  })
  #~~~
  
  
  
  output$TiePShow <- renderTable({
    req(input$files)
    TieP <- TiePointData()[input$FinalRowNumber,]
    return(TieP)
  })
  
  observeEvent(input$launchDelete, {
    shinyWidgets::ask_confirmation(
      inputId = "myconfirmation",
      title = "Confirm Row Deletion",
      
    )
  })
  
  
  
  observeEvent(input$myconfirmation, {
    if (isTRUE(input$myconfirmation)) {
      showNotification(paste("Point(s) will disapear after next plot action"), duration = 7, type = "message")
      
      df <- TiePointData()
      #df[input$FinalRowNumber,2] <- 555 #Double check this line
      
      print(df[input$FinalRowNumber,])
      df[input$FinalRowNumber,1] <- NA
      df[input$FinalRowNumber,2] <- NA
      df[input$FinalRowNumber,3] <- NA
      df[input$FinalRowNumber,4] <- NA
      
      print(df[input$FinalRowNumber,])
      pathtie <- toString(TieDataFilePath())
      write.table(x = df, file = pathtie, sep = " ", col.names = FALSE, row.names = FALSE)
      
      output$TiePShow <- renderTable({
        TieP <- TiePointData()[input$FinalRowNumber,]
        return(TieP)
      })
      
      print("Yes!")
    } else {
      #false
      print("No!")
    }
  }, ignoreNULL = TRUE)
  
  
  observeEvent(input$FinalCheck, {
    if (length(parseFilePaths(roots, input$files)$datapath)!=0) {
      
      #Make tie file use scientific notation
      
      pathtie <- toString(TieDataFilePath())
      
      FinTieToSci <- read.table(paste(pathtie), quote="\"", comment.char="")
      FinTieToSci <- as.data.frame(FinTieToSci)
      exportTieRemNA <- FinTieToSci[rowSums(is.na(FinTieToSci)) != ncol(FinTieToSci), ]
      
      if (identical(FinTieToSci, exportTieRemNA)==FALSE) { #If any row has an NA value it removes the row assuming it was clicked in error. Can add a second confirmation.
        showNotification(paste("Empty Rows"), duration = 4)
      }
      FinTieToSci <- exportTieRemNA
      
      FinTieToSci <- lapply(FinTieToSci, as.integer)
      
      
      FinTieToSci <- as.data.frame(lapply(FinTieToSci[,2], sprintf, fmt = "%e"))
      FinTieToSci <- as.data.frame(lapply(FinTieToSci[,4], sprintf, fmt = "%e"))
      FinTieToSci <- as.data.frame(lapply(FinTieToSci[,2], sprintf, fmt = "%g")) 
      FinTieToSci <- as.data.frame(lapply(FinTieToSci[,4], sprintf, fmt = "%g"))
      #for some reason, g is not changing the data but it is what Matlab is expecting... (why the %e is first)
      FinTieToSci <- as.data.frame(lapply(FinTieToSci[,1], sprintf, fmt = "%s"))
      FinTieToSci <- as.data.frame(lapply(FinTieToSci[,3], sprintf, fmt = "%s"))
      
      write.table(x = FinTieToSci, file = pathtie, sep = " ", col.names = FALSE, row.names = FALSE, quote = FALSE)
      print(head(FinTieToSci))
      print(dim(FinTieToSci))
    }
    
    

  })
  
  
  
  
  
  #~
  
  
  
  
  TieDataFilePath <- reactive(
    if (length(parseFilePaths(roots, input$files)$datapath)!=0) { #New tie file import
      return(stringr::str_remove(parseFilePaths(roots, input$files)$datapath, "^0+"))
    }
  )
  
  
  roots = c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()()) #New tie file import
  shinyFileChoose(input, 'files', roots=roots, filetypes=c('tie')) #New tie file import
  
  
  #~~~~~~~~~~~
  
  Empty <- data.table(V1 = NA, V2 = NA, V3 = NA, V4 = NA)
  output$downloadEmptyData <- downloadHandler(
    filename = function() {
      paste("NewTieFile", Sys.Date(), ".tie", sep = "")
    },
    content = function(file) {
      write.table(Empty, file, row.names = FALSE, col.names=F)
    }
  )
  
  #~~~~~~~~~~~~~
  
  

  
  observeEvent(input$tabs, {
    
    if(input$tabs == "Exit Application"){
      stopApp()
      js$closeWindow()
    } 
    
  })
  

}

