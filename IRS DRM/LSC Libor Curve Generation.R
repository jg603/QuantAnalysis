# LSC Libor Curve Generation.R
# LSC model with Libor data
# Sourced from LSC Swap Curve Fit.R with slight modifications
if(NumberOfFactors == 1){
  # No Tau  
  sTitle <- paste0("NF=", NumberOfFactors, ", No scalars")
}
if(NumberOfFactors == 2){
  Tau <- c(1:(NumberOfFactors-1))
  Tau[1] <- 2.0
  sTitle <- paste0("NF=", NumberOfFactors, ", Scalar=", Tau[1])
}
if(NumberOfFactors == 3){
  Tau <- c(1:(NumberOfFactors-2))
  Tau[1] <- 2.0
  sTitle <- paste0("NF=", NumberOfFactors, ", Scalar=", Tau[1])
}  
if(NumberOfFactors == 4){
  Tau <- c(1:(NumberOfFactors-1))
  Tau[1] <- 2.0
  Tau[2] <- 0.5
  sTitle <- paste0("NF=", NumberOfFactors, ", Scalars=", 
                   Tau[1], ",", Tau[2])
}
if(NumberOfFactors == 5){
  Tau <- c(1:(NumberOfFactors-1))
  Tau[1] <- 2.0
  Tau[2] <- 0.5
  Tau[3] <- 7.5
  sTitle <- paste0("NF=", NumberOfFactors, ", Scalars=", 
                   Tau[1], ",", Tau[2], ",", Tau[3])
}
if(NumberOfFactors == 6){
  Tau <- c(1:(NumberOfFactors-1))
  Tau[1] <- 2.0
  Tau[2] <- 0.5
  Tau[3] <- 7.5
  Tau[4] <- 15.0
  sTitle <- paste0("NF=", NumberOfFactors, ", Scalars=", 
                   Tau[1], ",", Tau[2], ",", Tau[3], ",", Tau[4])
}
LSCData <- read.xlsx(xlsxFile = CurvesFile, skipEmptyRows = FALSE)
NumberOfMaturities <- ncol(LSCData) - 1
NumberOfDates <- length(LSCData$V1) - 1
Maturity <- c(1:NumberOfMaturities)
Maturity[1] <- LSCData$V1[1]
Maturity[2] <- LSCData$V2[1]
Maturity[3] <- LSCData$V3[1]
Maturity[4] <- LSCData$V4[1]
Maturity[5] <- LSCData$V5[1]
Maturity[6] <- LSCData$V6[1]
Maturity[7] <- LSCData$V7[1]
Maturity[8] <- LSCData$V8[1]
head(Maturity,8)
Dates <- c(1:NumberOfDates)
for(i in 1:NumberOfDates){
  Dates[i] = as.character(LSCData$X1[i+1])
}
Rates <- matrix(nrow = NumberOfDates, ncol = NumberOfMaturities)
for(i in 1:NumberOfDates){
  Rates[i,1] <- LSCData$V1[i+1]
  Rates[i,2] <- LSCData$V2[i+1]
  Rates[i,3] <- LSCData$V3[i+1]
  Rates[i,4] <- LSCData$V4[i+1]
  Rates[i,5] <- LSCData$V5[i+1]
  Rates[i,6] <- LSCData$V6[i+1]
  Rates[i,7] <- LSCData$V7[i+1]
  Rates[i,8] <- LSCData$V8[i+1]
}
if(NumberOfFactors > 1){
  Factors <- matrix(nrow = (NumberOfFactors - 1), ncol = NumberOfMaturities)
  for (j in 1:(NumberOfFactors-1)) {
    for (i in 1:NumberOfMaturities) {
      if (j == 1) Factors[j,i] = (1.0 - exp(-Maturity[i]/Tau[j]))/(Maturity[i]/Tau[j])
      else Factors[j, i] = (1.0 - exp(-Maturity[i]/Tau[j-1]))/(Maturity[i]/Tau[j-1]) -
          exp(-Maturity[i]/Tau[j-1])
    }
  }
}
Intercept <- c(1:NumberOfDates)
Slope <- c(1:NumberOfDates)
Curvature1 <- c(1:NumberOfDates)
Curvature2 <- c(1:NumberOfDates)
Curvature3 <- c(1:NumberOfDates)
Curvature4 <- c(1:NumberOfDates)
for (d in 1:NumberOfDates){
  if(NumberOfFactors==1){
    LSC <- lm(formula = Rates[d,]~1)
    Betas <- LSC$coefficients
    Intercept[d] <- Betas[1]
  }  
  if(NumberOfFactors==2){
    LSC <- lm(formula = Rates[d,]~Factors[1,])
    Betas <- LSC$coefficients
    Intercept[d] <- Betas[1]
    Slope[d] <- Betas[2]
  }  
  if(NumberOfFactors==3){
    LSC <- lm(formula = Rates[d,]~Factors[1,]+Factors[2,])
    Betas <- LSC$coefficients
    Intercept[d] <- Betas[1]
    Slope[d] <- Betas[2]
    Curvature1[d] <- Betas[3]
  }  
  if(NumberOfFactors==4){
    LSC <- lm(formula = Rates[d,]~Factors[1,]+Factors[2,]+Factors[3,])
    Betas <- LSC$coefficients
    Intercept[d] <- Betas[1]
    Slope[d] <- Betas[2]
    Curvature1[d] <- Betas[3]
    Curvature2[d] <- Betas[4]
  }  
  if(NumberOfFactors==5){
    LSC <- lm(formula = Rates[d,]~Factors[1,]+Factors[2,]+Factors[3,]+
                Factors[4,])
    Betas <- LSC$coefficients
    Intercept[d] <- Betas[1]
    Slope[d] <- Betas[2]
    Curvature1[d] <- Betas[3]
    Curvature2[d] <- Betas[4]
    Curvature3[d] <- Betas[5]
  }  
  if(NumberOfFactors==6){
    LSC <- lm(formula = Rates[d,]~Factors[1,]+Factors[2,]+Factors[3,]+
                Factors[4,]+Factors[5,])
    Betas <- LSC$coefficients
    Intercept[d] <- Betas[1]
    Slope[d] <- Betas[2]
    Curvature1[d] <- Betas[3]
    Curvature2[d] <- Betas[4]
    Curvature3[d] <- Betas[5]
    Curvature4[d] <- Betas[6]
  }  
}  
FittedRates <- matrix(nrow = NumberOfDates, ncol = NumberOfMaturities)
for (i in 1:NumberOfDates) {
  for (j in 1:NumberOfMaturities){
    if(NumberOfFactors==1){
      FittedRates[i,j] = Intercept[i]
    }
    if(NumberOfFactors==2){
      FittedRates[i,j] = Intercept[i] + Slope[i]*((1.0 - 
                                                     exp(-Maturity[j]/Tau[1]))/(Maturity[j]/Tau[1]))
    }
    if(NumberOfFactors==3){
      FittedRates[i,j] = Intercept[i] + Slope[i]*((1.0 - 
                                                     exp(-Maturity[j]/Tau[1]))/(Maturity[j]/Tau[1])) + 
        Curvature1[i]*((1.0 - exp(-Maturity[j]/Tau[1]))/(Maturity[j]/Tau[1]) -
                         exp(-Maturity[j]/Tau[1]))
    }
    if(NumberOfFactors==4){
      FittedRates[i,j] = Intercept[i] + Slope[i]*((1.0 - 
                                                     exp(-Maturity[j]/Tau[1]))/(Maturity[j]/Tau[1])) + 
        Curvature1[i]*((1.0 - exp(-Maturity[j]/Tau[1]))/(Maturity[j]/Tau[1]) - 
                         exp(-Maturity[j]/Tau[1])) +
        Curvature2[i]*((1.0 - exp(-Maturity[j]/Tau[2]))/(Maturity[j]/Tau[2]) - 
                         exp(-Maturity[j]/Tau[2]))
    }
    if(NumberOfFactors==5){
      FittedRates[i,j] = Intercept[i] + Slope[i]*((1.0 - 
                                                     exp(-Maturity[j]/Tau[1]))/(Maturity[j]/Tau[1])) + 
        Curvature1[i]*((1.0 - exp(-Maturity[j]/Tau[1]))/(Maturity[j]/Tau[1]) - 
                         exp(-Maturity[j]/Tau[1])) +
        Curvature2[i]*((1.0 - exp(-Maturity[j]/Tau[2]))/(Maturity[j]/Tau[2]) - 
                         exp(-Maturity[j]/Tau[2])) +
        Curvature3[i]*((1.0 - exp(-Maturity[j]/Tau[3]))/(Maturity[j]/Tau[3]) - 
                         exp(-Maturity[j]/Tau[3]))
    }
    if(NumberOfFactors==6){
      FittedRates[i,j] = Intercept[i] + Slope[i]*((1.0 - 
                                                     exp(-Maturity[j]/Tau[1]))/(Maturity[j]/Tau[1])) + 
        Curvature1[i]*((1.0 - exp(-Maturity[j]/Tau[1]))/(Maturity[j]/Tau[1]) - 
                         exp(-Maturity[j]/Tau[1])) +
        Curvature2[i]*((1.0 - exp(-Maturity[j]/Tau[2]))/(Maturity[j]/Tau[2]) - 
                         exp(-Maturity[j]/Tau[2])) +
        Curvature3[i]*((1.0 - exp(-Maturity[j]/Tau[3]))/(Maturity[j]/Tau[3]) - 
                         exp(-Maturity[j]/Tau[3])) +
        Curvature4[i]*((1.0 - exp(-Maturity[j]/Tau[4]))/(Maturity[j]/Tau[4]) - 
                         exp(-Maturity[j]/Tau[4]))
    }
  }
}
# Selected plots
MinMaturity = 0
MaxMaturity = max(Maturity)
xlim1 = c(1:2)
xlim1[1] = MinMaturity
xlim1[2] = MaxMaturity
MinRate = min(Rates, FittedRates)
MaxRate = max(Rates, FittedRates)
ylim1 = c(1:2)
ylim1[1] = MinRate
ylim1[2] = MaxRate
mTitle = "Actual Libor Rates (Every month: Jan2020-Mar2023)"
xTitle = "Maturity"
yTitle = "Rates"
plot(Maturity, Rates[1,], type = "b", main = mTitle,
     sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
     ylim = ylim1, pch = 1, cex = 0.5)
for(i in 2:NumberOfDates){
  lines(Maturity, Rates[i,], type = "b", col = "black", xlim = xlim1, 
        ylim = ylim1, pch = i, cex = 0.5)
}
mTitle = "LSC Fitted Libor Rates (Every month: Jan2020-Mar2023)"
xTitle = "Maturity"
yTitle = "Rates"
plot(Maturity, FittedRates[1,], type = "b", main = mTitle,
     sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
     ylim = ylim1, pch = 1, cex = 0.5)
for(i in 2:NumberOfDates){
  lines(Maturity, FittedRates[i,], type = "b", col ="black", xlim = xlim1,
        ylim = ylim1, pch = i, cex = 0.5)
}
Error = FittedRates - Rates
MinError = min(Error)
MaxError = max(Error)
ylim1 = c(1:2)
# ylim1[1] = MinError
# ylim1[2] = MaxError
ylim1[1] = -0.50
ylim1[2] = 0.50
mTitle = "Fit Error (Every month: Jan2020-Mar2023)"
xTitle = "Maturity"
yTitle = "Error in Rates"
plot(Maturity, Error[1,], type = "b", main = mTitle,
     sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
     ylim = ylim1, pch = 1, cex = 0.5)
for(i in 2:NumberOfDates){
  lines(Maturity, Error[i,], type = "b", col = "black", xlim = xlim1, ylim = ylim1,
        pch = i, cex = 0.5)
}
# Create three data.frames and combine to produce output file
InputRates <- as.data.frame(Rates)
colnames(InputRates)
colnames(InputRates)[colnames(InputRates) == 'V1'] <- 'Rate(0.25)'
colnames(InputRates)[colnames(InputRates) == 'V2'] <- 'Rate(1)'
colnames(InputRates)[colnames(InputRates) == 'V3'] <- 'Rate(2)'
colnames(InputRates)[colnames(InputRates) == 'V4'] <- 'Rate(3)'
colnames(InputRates)[colnames(InputRates) == 'V5'] <- 'Rate(4)'
colnames(InputRates)[colnames(InputRates) == 'V6'] <- 'Rate(5)'
colnames(InputRates)[colnames(InputRates) == 'V7'] <- 'Rate(7)'
colnames(InputRates)[colnames(InputRates) == 'V8'] <- 'Rate(10)'
colnames(InputRates)[colnames(InputRates) == 'V9'] <- 'Rate(30)'
FittedRates <- as.data.frame(FittedRates)
colnames(FittedRates)
colnames(FittedRates)[colnames(FittedRates) == 'V1'] <- 'FitRate(0.25)'
colnames(FittedRates)[colnames(FittedRates) == 'V2'] <- 'FitRate(1)'
colnames(FittedRates)[colnames(FittedRates) == 'V3'] <- 'FitRate(2)'
colnames(FittedRates)[colnames(FittedRates) == 'V4'] <- 'FitRate(3)'
colnames(FittedRates)[colnames(FittedRates) == 'V5'] <- 'FitRate(4)'
colnames(FittedRates)[colnames(FittedRates) == 'V6'] <- 'FitRate(5)'
colnames(FittedRates)[colnames(FittedRates) == 'V7'] <- 'FitRate(7)'
colnames(FittedRates)[colnames(FittedRates) == 'V8'] <- 'FitRate(10)'
colnames(FittedRates)[colnames(FittedRates) == 'V9'] <- 'FitRate(30)'
LSCSwapCurveOutput <- as.data.frame(Dates)
if(NumberOfFactors > 0)LSCSwapCurveOutput$Level <- Intercept
if(NumberOfFactors > 1)LSCSwapCurveOutput$Slope <- Slope
if(NumberOfFactors > 2)LSCSwapCurveOutput$Curvature1 <- Curvature1
if(NumberOfFactors > 3)LSCSwapCurveOutput$Curvature2 <- Curvature2
if(NumberOfFactors > 4)LSCSwapCurveOutput$Curvature3 <- Curvature3
LSCSwapCurveOutput <- cbind(LSCSwapCurveOutput, InputRates)
LSCSwapCurveOutput <- cbind(LSCSwapCurveOutput, FittedRates)
if(NumberOfFactors == 1)LSCOut <- "LSC Swap Curve Output 1.xlsx"
if(NumberOfFactors == 2)LSCOut <- "LSC Swap Curve Output 2.xlsx"
if(NumberOfFactors == 3)LSCOut <- "LSC Swap Curve Output 3.xlsx"
if(NumberOfFactors == 4)LSCOut <- "LSC Swap Curve Output 4.xlsx"
if(NumberOfFactors == 5)LSCOut <- "LSC Swap Curve Output 5.xlsx"
write.xlsx(LSCSwapCurveOutput, LSCOut)
