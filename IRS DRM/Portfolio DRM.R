#Portfolio DRM.R
#Contains functions useful for DRM of Interest Rate Swap Portfolios
Packages <- c('date','stats')
source('SRM Swap Functions.R')
PortfolioValue = function(Port){
  Value <- 0  
  #Sum each swap value with fitted LSC curve
  for(i in 1:length(Port)){
    Value = Value + SwapValuett(Port[[i]])
  }
  return(Value)
}
#Apply LSC simulation to Portfolio and return possible values
PortfolioSim = function(Port,Curve,Horiz){
  #create array that will store gain or loss for each simulation
  SG1 <- SwapSim(Port[[1]],Curve,Horiz)
  SG2 <- SwapSim(Port[[2]],Curve,Horiz)
  SG3 <- SwapSim(Port[[3]],Curve,Horiz)
  SG4 <- SwapSim(Port[[4]],Curve,Horiz)
  SG5 <- SwapSim(Port[[5]],Curve,Horiz)
  SG6 <- SwapSim(Port[[6]],Curve,Horiz)
  SG7 <- SwapSim(Port[[7]],Curve,Horiz)
  SG8 <- SwapSim(Port[[8]],Curve,Horiz)
  SG9 <- SwapSim(Port[[9]],Curve,Horiz)
  SG10 <- SwapSim(Port[[10]],Curve,Horiz)
  #Add Swap results together to get portfolio results
  PG <- SG1+SG2+SG3+SG4+SG5+SG6+SG7+SG8+SG9+SG10
  
  SimResults <- list(SG1,SG2,SG3,SG4,SG5,SG6,SG7,SG8,SG9,SG10,PG)
  names(SimResults) <- c('Swap1','Swap2','Swap3','Swap4','Swap5','Swap6',
                         'Swap7','Swap8','Swap9','Swap10','Portfolio')
  
  return(SimResults)
}
#Calculate VaR of a swap or portfolio
VaR = function(Gains,VType){
  Value <- quantile(Gains, probs = (1-VType/100))
  return(Value)
}
#Calculate CVaR of a swap or portfolio
CVaR = function(Gains,VType){
  Total <- 0
  Count <- 0
  Threshold <- quantile(Gains, probs = (1-VType/100))
  for(i in 1:length(Gains)){
    if(Gains[i]<=Threshold){
      Total = Total + Gains[i]
      Count = Count + 1
    }
  }
  Value = Total/Count
  return(Value)
}
