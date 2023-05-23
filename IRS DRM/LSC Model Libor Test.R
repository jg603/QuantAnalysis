# LSC Model Libor Test.R
# Slightly modified from 6.3 LSC Model IR Swap Test.R
# LSC model with Libor data
# rmarkdown::render("6.3 LSC Model IR Swap Test.R", "word_document")
rm(list = ls())  # Take out the Environment "trash"
cat("\014") # Clear Console, making error checking easier.
while (!is.null(dev.list()))  dev.off() # Clear old plots
par(family = 'Times New Roman') # Globally set fonts for graphs
# Libraries
#  date - functions for handling dates
#  openxlsx - opening IRSwapBook.xlsx
#  tis: Time Indexes and Time Indexed Series (holidays)
Packages <- c("openxlsx", "date", "tis")
if(length(setdiff(Packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(Packages, rownames(installed.packages())))
} # Make sure libraries are installed on this computer
lapply(Packages, library, character.only=TRUE) # Load and attach libraries
rm(Packages)
# Input dataset: SwapInputData.dat
# Maximum of 6 coefficients in LSC model
NumberOfFactors <- 1 
source('LSC Libor Curve Generation.R')
NumberOfFactors <- 2 
source('LSC Libor Curve Generation.R')
NumberOfFactors <- 3 
source('LSC Libor Curve Generation.R')
NumberOfFactors <- 4 
source('LSC Libor Curve Generation.R')
NumberOfFactors <- 5 
source('LSC Libor Curve Generation.R')
NumberOfFactors <- 6 
source('LSC Libor Curve Generation.R')


Intercept
Slope
Curvature1
