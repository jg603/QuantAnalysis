# LSC Multivariate RNG.R
# Creates multivariate rng matrix for the change in level, slope, and curvature
# of a 3-factor LSC Curve
Packages <- c("openxlsx", "date", "tis")
if(length(setdiff(Packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(Packages, rownames(installed.packages())))
} # Make sure libraries are installed on this computer
lapply(Packages, library, character.only=TRUE) # Load and attach libraries
rm(Packages)
# Set number of LSC Factors and generate LSC Curves
NumberOfFactors <- 3 
source('LSC Libor Curve Generation.R')
# Create arrays that store the change in the LSC factors
DeltaL <- array()
DeltaS <- array()
DeltaC <- array()
for(i in 2:NumberOfDates){
  DeltaL[i-1] = Intercept[i]-Intercept[i-1]
  DeltaS[i-1] = Slope[i]-Slope[i-1]
  DeltaC[i-1] = Curvature1[i] - Curvature1[i-1]
}
# create array for mean and multiply by CurvePeriod to annualize
Mean <- as.numeric(c(1:NumberOfFactors))
Mean[1] = mean(DeltaL)
Mean[2] = mean(DeltaS)
Mean[3] = mean(DeltaC)
Mean = Mean*CurvePeriod
# create matrix for covariance and multiply by CurvePeriod to annualize
Covariance <- matrix(nrow = NumberOfFactors, ncol = NumberOfFactors)
Covariance[1,1] = cov(DeltaL,DeltaL)
Covariance[1,2] = cov(DeltaL,DeltaS)
Covariance[1,3] = cov(DeltaL,DeltaC)
Covariance[2,1] = Covariance[1,2]
Covariance[2,2] = cov(DeltaS,DeltaS)
Covariance[2,3] = cov(DeltaS,DeltaC)
Covariance[3,1] = Covariance[1,3]
Covariance[3,2] = Covariance[2,3]
Covariance[3,3] = cov(DeltaC,DeltaC)
Covariance = Covariance * CurvePeriod
# create RNG matrix
RNGMatrix = mvrnorm(n = NSim, Mean, Covariance, tol = 1e-6,empirical = TRUE, EISPACK = FALSE)

