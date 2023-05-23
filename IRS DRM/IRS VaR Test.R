# IRS VaR Test.R
# Calculate VaR of a portfolio of 10 interest rate swaps
rm(list = ls())  # Take out the Environment "trash"
cat("\014") # Clear Console, making error checking easier.
while (!is.null(dev.list()))  dev.off() # Clear old plots
par(family = 'Times New Roman') # Globally set fonts for graphs
# Libraries
#  date - functions for handling dates
#  optimx - general purpose optimization
#  openxlsx - manipulate spreadsheet files
#  stats - general statistical functions
#  expss - vlookup (not used now)
#  dplyr - rounding in dataframes
#  MASS - Multivariate normal random number generator
#  tis - isBusinessDay
Packages <- c("date", "optimx", "openxlsx", "stats", "expss", "dplyr", 
              "MASS", "tis") 
if(length(setdiff(Packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(Packages, rownames(installed.packages())))
} # Make sure libraries are installed on this computer
lapply(Packages, library, character.only=TRUE) # Load and attach libraries
rm(Packages)
#INPUTS
#Input swap book spreadsheet
SwapBookFile <- 'Swap Inputs.xlsx' 
#Input historical interest rate curve spreadsheet
CurvesFile <- 'LiborCurves.xlsx'
#Input period of curves in the above spreadsheet (1-yearly, 4-quarterly, 12-monthly)
CurvePeriod <- 12 
#Input horizon type (1 = yearly, 2 = monthly, 3 = daily)
HorizType <- 3
#Input number of time periods for specified type
HorizCount <- 1
#Input desired number of simulations
NSim = 1000
#Input VaR type (i.e. 95 for 95%VaR or 99 for 99%VaR)
VaRType = 99
#END INPUTS
#Create multivariate RNG matrix for changes in Level, Slope, and Curvature
source('LSC Multivariate RNG.R')
#Extract relevant data from Swap book file
source('Swap Input File Management.R')
#Set days until horizon
Horizon = HorizCount
#yearly horizon
if(HorizType == 1){
  Horizon = 365*HorizCount
}
#monthly horizon
if(HorizType == 2){
  Horizon = 30*HorizCount
}
#Simulate LSC Values at horizon using RNG matrix
source('IRS DRM Functions.R')
LSC = SimLSC(InputL,InputS,InputC,RNGMatrix,Horizon)
#Compute and print initial value of Portfolio
source('Portfolio DRM.R')
PortfolioValue(Portfolio)
#Calculate distribution of possible outcomes for each swap and portfolio
Gains = PortfolioSim(Portfolio,LSC,Horizon)
#Compute and print relevant statistics
VaR(Gains$Portfolio,VaRType) #VaR
CVaR(Gains$Portfolio,VaRType) #CVaR
max(LSC$Level) #maximum Level
mean(LSC$Level) #mean Level
min(LSC$Level) #minimum Level
max(LSC$Slope) #maximum Slope
mean(LSC$Slope) #mean Slope
min(LSC$Slope) #minimum Slope
max(LSC$Curvature) #maximum Curvature
mean(LSC$Curvature) #mean Curvature
min(LSC$Curvature) #minimum Curvature
max(Gains$Portfolio) #maximum portfolio gain
mean(Gains$Portfolio) #mean portfolio gain
min(Gains$Portfolio) #minimum portfolio gain
#Generate Histogram of Data
stitle <- paste(HorizCount,' Day')
if(HorizType == 1){
  stitle = paste(HorizCount,' Year')
}
if(HorizType == 2){
  stitle = paste(HorizCount,' Month')
}
hist(Gains$Portfolio, xlab = 'Gain or Loss', ylab = '# of Observations',
     main = 'Simulation Results',sub = stitle, breaks = 20)

