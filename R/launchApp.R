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
  
  #CurrentDir <- gsub("/MatchPackage.*", "/Package/R/app.R", CurrentDir)
  CurrentDir <- paste(CurrentDir, "/Package/R/app.R", sep="")
  #print(CurrentDir)
  
  
  #shiny::runApp("~/GUImatch/Package/R/app.R")
  shiny::runApp(CurrentDir)
}
