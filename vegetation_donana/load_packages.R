# Function to check, install, and load required packages
install_and_load_packages <- function(required_packages) {
  # Check which packages are not installed
  missing_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
  
  # Install missing packages
  if(length(missing_packages) > 0) {
    message("Installing missing packages: ", paste(missing_packages, collapse = ", "))
    install.packages(missing_packages, dependencies = TRUE)
  }
  
  # Load all required packages
  for(package in required_packages) {
    library(package, character.only = TRUE)
  }
  
  message("All required packages have been installed and loaded.")
}

# Example usage:
required_packages <- c("tidyverse", 
                       "signal",
                       "zoo", 
                       "lubridate",
                       "ggplot2",
                       "here",
                       "gtools",
                       "broom",
                       "FactoMineR", 
                       "factoextra",
                       "ggrepel",
                       "ggpubr",
                       "terra",
                       "readxl",
                       "e1071",
                       "DBI",
                       "RSQLite", 
                       "visNetwork",
                       "sf",
                       "devtools",
                       "reticulate",
                       "conflicted",
                       "ranger",
                       "mgcv",
                       "shiny",
                       "jagsUI",
                       "bs4Dash",
                       "shinythemes",
                       "shinyjs",
                       "shinycssloaders",
                       "plotly",
                       "colorspace",
                       "keyring",
                       "XML",
                       "httr",
                       "curl",
                       "jsonlite",
                       "ows4R",
                       "tidyterra"
                       
                       
)
install_and_load_packages(required_packages)

if(!"Rchelsa"%in%installed.packages()){
  install_git("https://gitlabext.wsl.ch/karger/rchelsa.git")
}else{cat("Rchelsa is already installed.")}
library(Rchelsa)
py_install("chelsa-cmip6", pip=T)
chelsa_cmip6 <- import('chelsa_cmip6')

conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("column", "bs4Dash")
conflict_prefer("layout", "plotly")
