#Check that r packages are downloaded:

#First checking that all of the other packages are installed.

PackagesToCheck <- list("ggplot2","ggrepel", "rio", "astrochron", "ggiraph", 
                        "shinyjs", "caret", "shinyFiles", "stringr", "data.table", 
                        "tibble", "shinythemes", "shinyWidgets", "stats", "utils", "dplyr", 
                        "ggplot2", "shiny", "plotly", "shinyFiles")

for (i in PackagesToCheck) {
  print(paste0(i, ": ", i %in% rownames(installed.packages())))
  if (i %in% rownames(installed.packages()) == FALSE) {
    install.packages(i)
  }
}


#Now checking that Match is installed.

print(paste0("Match Is Installed: ", "Match" %in% rownames(installed.packages())))
 
#If Match is FALSE then go to the readme and downlaod the binary.


