# IRS DRM Functions.R
# Contains functions for DRM of individual interest rate swaps
Packages <- c('date','stats')
source('SRM Swap Functions.R')
# Create simulated LSC curves for Monte Carlo simulation
SimLSC = function(Level,Slope,Curvature,InputMatrix,Horiz){
  #create arrays for simulated LSC paramters
  SimL <- array()
  SimS <- array()
  SimC <- array()
  #randomize LSC parameters
  for(i in 1:nrow(InputMatrix)){
    SimL[i] = Level + InputMatrix[i,1]*Horiz/365
    SimS[i] = Slope + InputMatrix[i,2]*Horiz/365
    SimC[i] = Curvature + InputMatrix[i,3]*Horiz/365
  }
  SimCurve <- list(SimL,SimS,SimC)
  names(SimCurve) <- c('Level','Slope','Curvature')
  return(SimCurve)
}
# Calculate possibilities of Swaps at Horizon
SwapSim = function(Swap,LSC,Horiz){
  Gain <- array()
  StartValue <- SwapValuett(Swap)
  #adjust evaluation date
  EvalDate = mdy.date(Swap$EM,Swap$ED,Swap$EY,nineteen = FALSE)
  HorizJulian = EvalDate + Horiz
  HorizDate = date.mdy(HorizJulian)
  #set new evaluation date
  Swap$EM = HorizDate$month
  Swap$ED = HorizDate$day
  Swap$EY = HorizDate$year
  #create list of simulated swaps
  SimSwaps <- list()
  for(i in 1:length(LSC$Level)){
    #set Swap LSC parameters to simulated parameters
    Swap$FRParm1 = LSC$Level[i]
    Swap$FRParm2 = LSC$Slope[i]
    Swap$FRParm3 = LSC$Curvature[i]
    #Calculate gain
    Gain[i] = SwapValuett(Swap)-StartValue
  }
  return(Gain)
}
