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
                       "tidyterra")

# Install and load all required packages
install_and_load_packages(required_packages)

# Install BiocManager if needed (for BiocVersion dependency)
if (!require("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
  library(BiocManager)
}

# Install Rchelsa from Git (devtools is now loaded from required_packages)
if(!"Rchelsa" %in% installed.packages()){
  devtools::install_git("https://gitlabext.wsl.ch/karger/rchelsa.git")
} else {
  cat("Rchelsa is already installed.")
}

# Load Rchelsa
library(Rchelsa)

# Python setup (reticulate is now loaded from required_packages)
reticulate::py_install("chelsa-cmip6", pip = TRUE)
chelsa_cmip6 <- reticulate::import('chelsa_cmip6')

# Conflict preferences (conflicted is now loaded from required_packages)
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("column", "bs4Dash")
conflicted::conflict_prefer("layout", "plotly")

