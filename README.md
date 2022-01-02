# Shiny Match App

## Installation

``` r
devtools::install_github("WilliamKopans/MatchPackageBETA",upgrade = c("always"), force = TRUE, quiet = TRUE)
```

...or...

``` r
source("https://install-github.me/WilliamKopans/MatchPackageBETA")
```

...or...

Click [HERE](https://github.com/WilliamKopans/MatchPackageBETA/blob/main/Match_1.0.0.tgz?raw=true) to download the binary package. Then type into R: 

install.packages("Path to downloaded file")

## Run the Shiny app

There's only one exported function in the package and it runs the Shiny app:

``` r
Match::launchApp()
```

---



Credit to https://github.com/MangoTheCat/shinyAppDemo for the boilerplate R Package code.

Executable "Getting Started" Jupyter Notebook available [HERE](https://github.com/WilliamKopans/MatchPackageBETA/blob/main/Match%20App%20Getting%20Started.ipynb).
