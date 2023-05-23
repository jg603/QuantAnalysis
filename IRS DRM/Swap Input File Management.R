#Swap Input File Management.R
#Contains functions to input a singular swap or a portfolio of swaps
#Input initial LSC curves
IFC <- read.xlsx(xlsxFile = SwapBookFile, sheet = "Initial Forward Curve", 
                 skipEmptyRows = FALSE)
IBC <- read.xlsx(xlsxFile = SwapBookFile, sheet = "Initial Basis Curve", 
                 skipEmptyRows = FALSE)
IDC <- read.xlsx(xlsxFile = SwapBookFile, sheet = "Initial Discount Curve", 
                 skipEmptyRows = FALSE)
#Import an individual swap
ImportSwap = function(SheetName){
  #MI = model inputs
  MI <- read.xlsx(xlsxFile = SwapBookFile, sheet = SheetName, 
                  skipEmptyRows = FALSE)
  # Evaluation date (21,916 due to MS Excel base year of 12-30-1899 cp w 1-1-1960)
  date.mdy(-21916) # Off two days
  jED <- as.integer(MI$Parameters[1]) - 21916
  mdyED <- date.mdy(jED)
  inputEM <- mdyED$month
  inputED <- mdyED$day
  inputEY <- mdyED$year
  # Maturity date
  jMD <- as.integer(MI$Parameters[3]) - 21916
  mdyMD <- date.mdy(jMD)
  inputMM <- mdyMD$month
  inputMD <- mdyMD$day
  inputMY <- mdyMD$year
  inputFixedPF <- MI$PaymentFrequency[1]
  inputFloatingPF <- MI$PaymentFrequency[2]
  if(MI$NAD[1] == "30-Days"){
    inputFixedNAD <- 1
  } else {
    inputFixedNAD <- 0
  }
  if(MI$NAD[2] == "30-Days"){
    inputFloatingNAD <- 1
  } else {
    inputFloatingNAD <- 0
  }
  inputFixedNTD <- MI$NTD[1]
  inputFloatingNTD <- MI$NTD[2]
  inputReceivePay <- as.numeric(MI$Parameters[7])
  inputCurveType <- MI$Parameters[8]
  inputNotionalAmount <- as.numeric(MI$Parameters[4])
  inputFixedRate <- as.numeric(MI$Parameters[5])
  inputCashCollateral <- as.numeric(MI$Parameters[6])
  inputFixedConvention <- MI$Convention[1]
  inputFloatingConvention <- MI$Convention[2]
  # Check inputting process
  MI$Parameters[1]; jED; inputEM; inputED; inputEY
  MI$Parameters[3]; jMD; inputMM; inputMD; inputMY
  inputFixedPF; inputFloatingPF
  inputFixedNAD; inputFloatingNAD
  inputFixedNTD; inputFloatingNTD
  inputReceivePay; inputCurveType; 
  inputNotionalAmount; inputFixedRate; inputCashCollateral
  inputFixedConvention; inputFloatingConvention
  # Evaluation date information
  # Forward curve
  inputNumberLSCEFR <- IFC$Parameters[1]
  inputEDFCParameters <- numeric(inputNumberLSCEFR)
  inputEDFCScalars <- numeric(inputNumberLSCEFR-1)
  for(i in 1:inputNumberLSCEFR){
    if(i < inputNumberLSCEFR)inputEDFCScalars[i] <- IFC$Scalars[i+2]
    inputEDFCParameters[i] <- IFC$Parameters[i+1]
  }
  #
  # Basis or discount curve
  #  GR and GC denote generic rate and generic curve (basis or discount)
  #
  if(inputCurveType=='BC'){
    inputNumberLSCEGR <- IBC$Parameters[1]
    inputEDGCParameters <- numeric(inputNumberLSCEGR)
    inputEDGCScalars <- numeric(inputNumberLSCEGR-1)
    for(i in 1:inputNumberLSCEGR){
      if(i < inputNumberLSCEGR)inputEDGCScalars[i] <- IBC$Scalars[i+2]
      inputEDGCParameters[i] <- IBC$Parameters[i+1]
    }
  } else {
    inputNumberLSCEGR <- IDC$Parameters[1]
    inputEDGCParameters <- numeric(inputNumberLSCEGR)
    inputEDGCScalars <- numeric(inputNumberLSCEGR-1)
    for(i in 1:inputNumberLSCEGR){
      if(i < inputNumberLSCEGR)inputEDGCScalars[i] <- IDC$Scalars[i+2]
      inputEDGCParameters[i] <- IDC$Parameters[i+1]
    }
  }
  #
  # Migrate Excel-based inputs into swap structure
  #
  TradeName <- SheetName
  EM <- inputEM   # Evaluation month
  ED <- inputED   # Evaluation day
  EY <- inputEY   # Evaluation year
  MM <- inputMM   # Maturity month
  MD <- inputMD   # Maturity day
  MY <- inputMY   # Maturity year
  FixPF <- inputFixedPF  # Fixed payment frequency, times per year
  FltPF <- inputFloatingPF  # Floating payment frequency, times per year
  FixNAD <- inputFixedNAD  # Fixed number of accrued days: 0=30/FixNTD, 1=ACT/FixNTD
  FltNAD <- inputFloatingNAD  # Floating number of accrued days: 0=30/FixNTD, 1=ACT/FixNTD
  FixNTD <- inputFixedNTD  # Day count convention for fixed leg
  FltNTD <- inputFloatingNTD  # Day count convention for floating leg
  FixConv <- inputFixedConvention # Modified business following (MBF) or preceeding (MBP)
  FltConv <- inputFloatingConvention
  SwapType <- inputReceivePay # 1 - receive fixed, -1 - receive floating
  DiscountType <- inputCurveType # BC - basis curve, DC - discount curve (default)
  NAmt <- inputNotionalAmount # Notional Amount
  FixedRate <- inputFixedRate # Fixed swap rate
  CashCollateral <- inputCashCollateral # As percent of NAmt
  NLSCFR <- inputNumberLSCEFR # Number LSC forward rate factors
  NLSCGR <- inputNumberLSCEGR # Number LSC discount rate factors
  #
  # Forward rate (default is evaluation date)
  #
  FRParm1 <- 0.0 # Forward rate LSC parameters (fill w/ zeros if not inputted)
  FRParm2 <- 0.0
  FRParm3 <- 0.0
  FRParm4 <- 0.0
  FRParm5 <- 0.0
  FRParm6 <- 0.0
  if(NLSCFR > 0)FRParm1 <- inputEDFCParameters[1]
  if(NLSCFR > 1)FRParm2 <- inputEDFCParameters[2]
  if(NLSCFR > 2)FRParm3 <- inputEDFCParameters[3]
  if(NLSCFR > 3)FRParm4 <- inputEDFCParameters[4]
  if(NLSCFR > 4)FRParm5 <- inputEDFCParameters[5]
  if(NLSCFR > 5)FRParm6 <- inputEDFCParameters[6]
  FRScalar1 <- 0.0 # Forward rate LSC scalars
  FRScalar2 <- 0.0
  FRScalar3 <- 0.0
  FRScalar4 <- 0.0
  FRScalar5 <- 0.0
  if(NLSCFR > 1)FRScalar1 <- inputEDFCScalars[1]
  if(NLSCFR > 2)FRScalar2 <- inputEDFCScalars[2]
  if(NLSCFR > 3)FRScalar3 <- inputEDFCScalars[3]
  if(NLSCFR > 4)FRScalar4 <- inputEDFCScalars[4]
  if(NLSCFR > 5)FRScalar5 <- inputEDFCScalars[5]
  #
  # GR -- denote generic rate (default evaluation date)
  #
  GRParm1 <- 0.0 # Discount rate LSC parameters
  GRParm2 <- 0.0
  GRParm3 <- 0.0
  GRParm4 <- 0.0
  GRParm5 <- 0.0
  GRParm6 <- 0.0
  if(NLSCGR > 0)GRParm1 <- inputEDGCParameters[1]
  if(NLSCGR > 1)GRParm2 <- inputEDGCParameters[2]
  if(NLSCGR > 2)GRParm3 <- inputEDGCParameters[3]
  if(NLSCGR > 3)GRParm4 <- inputEDGCParameters[4]
  if(NLSCGR > 4)GRParm5 <- inputEDGCParameters[5]
  if(NLSCGR > 5)GRParm6 <- inputEDGCParameters[6]  
  GRScalar1 <- 0.0 # Generic (basis or discount) rate LSC scalars
  GRScalar2 <- 0.0
  GRScalar3 <- 0.0
  GRScalar4 <- 0.0
  GRScalar5 <- 0.0
  if(NLSCGR > 1)GRScalar1 <- inputEDGCScalars[1]
  if(NLSCGR > 2)GRScalar2 <- inputEDGCScalars[2]
  if(NLSCGR > 3)GRScalar3 <- inputEDGCScalars[3]
  if(NLSCGR > 4)GRScalar4 <- inputEDGCScalars[4]
  if(NLSCGR > 5)GRScalar5 <- inputEDGCScalars[5]
  # Place all data in single data frame
  SVInputs <- data.frame(TradeName, EM, ED, EY, MM, MD, MY, FixPF, FltPF, 
                         FixNAD, FltNAD, FixNTD, FltNTD, FixConv, FltConv, SwapType, NAmt, 
                         DiscountType, FixedRate, CashCollateral,
                         NLSCFR, NLSCGR, FRParm1, FRParm2, FRParm3, FRParm4, FRParm5, FRParm6, 
                         GRParm1, GRParm2, GRParm3, GRParm4, GRParm5, GRParm6,
                         FRScalar1, FRScalar2, FRScalar3, FRScalar4, FRScalar5, 
                         GRScalar1, GRScalar2, GRScalar3, GRScalar4, GRScalar5)
  names(SVInputs) <- c("TradeName", "EM", "ED", "EY", "MM", "MD", "MY", 
                       "FixPF", "FltPF", "FixNAD", "FltNAD", "FixNTD", "FltNTD", "FixConv", 
                       "FltConv", "SwapType", "NAmt", "DiscountType",
                       "FixedRate", "CashCollateral", "NLSCFR", "NLSCGR", 
                       "FRParm1", "FRParm2", "FRParm3", "FRParm4", "FRParm5", "FRParm6", 
                       "GRParm1", "GRParm2", "GRParm3", "GRParm4", "GRParm5", "GRParm6",
                       "FRScalar1", "FRScalar2", "FRScalar3", "FRScalar4", "FRScalar5",
                       "GRScalar1", "GRScalar2", "GRScalar3", "GRScalar4", "GRScalar5")
  
  #rm(TradeName, EM, ED, EY, MM, MD, MY, FixPF, FltPF, 
   #  FixNAD, FltNAD, FixNTD, FltNTD, FixConv, FltConv, SwapType, NAmt, 
    # DiscountType, FixedRate, CashCollateral, 
     #NLSCFR, NLSCGR, FRParm1, FRParm2, FRParm3, FRParm4, FRParm5, FRParm6, 
     #GRParm1, GRParm2, GRParm3, GRParm4, GRParm5, GRParm6,
     #FRScalar1, FRScalar2, FRScalar3, FRScalar4, FRScalar5, 
     #GRScalar1, GRScalar2, GRScalar3, GRScalar4, GRScalar5)
  Swap = SVInputs
  return(Swap)
}
#Set input Level, Slope, and Curvature
InputL = IFC$Parameters[2]
InputS = IFC$Parameters[3]
InputC = IFC$Parameters[4]
#Create portfolio (stored as a list of swaps)
Portfolio <- list()
Portfolio[[1]] = ImportSwap('Swap1')
Portfolio[[2]] = ImportSwap('Swap2')
Portfolio[[3]] = ImportSwap('Swap3')
Portfolio[[4]] = ImportSwap('Swap4')
Portfolio[[5]] = ImportSwap('Swap5')
Portfolio[[6]] = ImportSwap('Swap6')
Portfolio[[7]] = ImportSwap('Swap7')
Portfolio[[8]] = ImportSwap('Swap8')
Portfolio[[9]] = ImportSwap('Swap9')
Portfolio[[10]] = ImportSwap('Swap10')

