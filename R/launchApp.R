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

  CurrentDir <- getwd()
  #CurrentDir <- paste(CurrentDir, "/R/app.R", sep="")
  shiny::runApp(CurrentDir)
}



