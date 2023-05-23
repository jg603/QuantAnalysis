# VaR Plot Generation.R
# Create plots used for presentation
# Develop a plot that shows how VaR changes over time
H <- 30
NSim <- 100
source('LSC Multivariate RNG.R')
source('IRS DRM Functions.R')
source('Portfolio DRM.R')
VaRSeries <- matrix(nrow = 12, ncol = 11)
Days <- 1:12
# Create matrix of VaRs at different times
for(i in 1:12){
  Days[i] = H
  GraphLSC = SimLSC(InputL,InputS,InputC,RNGMatrix,H*i)
  G = PortfolioSim(Portfolio,GraphLSC,H)
  VaRSeries[i,1] = VaR(G$Swap1,95)
  VaRSeries[i,2] = VaR(G$Swap2,95)
  VaRSeries[i,3] = VaR(G$Swap3,95)
  VaRSeries[i,4] = VaR(G$Swap4,95)
  VaRSeries[i,5] = VaR(G$Swap5,95)
  VaRSeries[i,6] = VaR(G$Swap6,95)
  VaRSeries[i,7] = VaR(G$Swap7,95)
  VaRSeries[i,8] = VaR(G$Swap8,95)
  VaRSeries[i,9] = VaR(G$Swap9,95)
  VaRSeries[i,10] = VaR(G$Swap10,95)
  VaRSeries[i,11] = VaR(G$Portfolio,95)
  H = H+30
}
VaRSeries <- VaRSeries * (-1)
# Create plot illustrating how maturity affects VaR
plot(Days, VaRSeries[,1],type = "b", main = "VaR Over Time, Fixed Rate = 2%",xlab = "Time (Days)",
     ylab = "VaR", pch = 19)
lines(Days, VaRSeries[,2],type = "b", pch = 10)
legend("topleft", c("Maturity = 5yr","Maturity = 3yr"), pch = c(19,10), lwd = c(1,1))
# Create plot illustrating how interest rate affects VaR
plot(Days, VaRSeries[,9],type = "l", main = "VaR Over Time, Maturity = 5yr",xlab = "Time (Days)",
     ylab = "VaR", col = "blue")
lines(Days, VaRSeries[,7],type = "l", col = "red")
lines(Days, VaRSeries[,5],type = "l", col = "black")
lines(Days, VaRSeries[,3],type = "l", col = "orange")
legend("topleft", c("FixRate = 4.00%","FixRate = 3.50%", "FixRate = 3.00%", "FixRate = 2.50%"),
       col = c("blue","red","black","orange"), lwd = c(1,1,1,1))


