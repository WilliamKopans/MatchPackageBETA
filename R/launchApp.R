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
  #shinyApp(ui = shinyAppUI, server = shinyAppServer)
  #shinyApp(ui = shinyAppUI, server = shinyAppServer)
  #shinyApp(ui, server)
  
  CurrentDir <- getwd()
   #/Users/williamkopans/Documents/Coding/TimHerbertBrown/GUImatch/MatchPackage/R/launchApp.R
  #CurrentDir <- gsub("/MatchPackage.*", "/Package/R/app.R", CurrentDir)
  CurrentDir <- paste(CurrentDir, "/R/app.R", sep="")
  #print(CurrentDir)
  
  
  #shiny::runApp("~/GUImatch/Package/R/app.R")
  #shiny::runApp("/Users/williamkopans/Documents/Coding/TimHerbertBrown/MatchPackageBETA/R/app.R")
  shiny::runApp(CurrentDir)
}



