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

  #CurrentDir <- getwd()
  #CurrentDir <- paste(CurrentDir, "/R/app.R", sep="")
  #shiny::runApp(CurrentDir)
  #shinyApp(ui = shinyAppUI, server = shinyAppServer)
  
  
  tryCatch(
    expr = {
      CurrentDir <- getwd()
      CurrentDir <- paste(CurrentDir, "/R/app.R", sep="")
      shiny::runApp(CurrentDir)
    },
    error = function(e){ 
      message('Caught an error!')
      print(e)
      print("WD:")
      message(getwd())
    },
    warning = function(w){
      message('Caught an warning!')
      print(e)
      print("WD:")
      message(getwd())
    },
    finally = {
      message('Something went wrong')
      print(e)
      print("WD:")
      message(getwd())
    }
  )

  
  
  
  
  
  
  
  
  
  
}



