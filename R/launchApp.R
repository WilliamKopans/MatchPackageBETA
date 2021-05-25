#' launches the shinyAppDemo app
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
  message('Dev Version HC Prod0')
  #CurrentDir <- getwd()
  #CurrentDir <- paste(CurrentDir, "/R/app.R", sep="")
  #shiny::runApp(CurrentDir)
  
      print(paste0("Directory: ", getwd()))
  
      shiny::runApp("/Users/williamkopans/Documents/Coding/TimHerbertBrown/MatchPackageBETA/R/app.R")

  
  
}



