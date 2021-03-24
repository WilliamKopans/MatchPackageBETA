#' launches the shinyAppDemo app
#'
#' @export launchApp
#'
#' @return shiny application object
#'
#' @example \dontrun {launchApp()}
#'
#' @import shiny


# wrapper for shiny::shinyApp()
launchApp <- function() {
  #shinyApp(ui = shinyAppUI, server = shinyAppServer)
  #shinyApp(ui = shinyAppUI, server = shinyAppServer)
  #shinyApp(ui, server)
  shiny::runApp("~/Documents/Coding/TimHerbertBrown/GUImatch/Package/R/app.R")
}
