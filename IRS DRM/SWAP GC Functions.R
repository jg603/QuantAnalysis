# SWAP GC Functions.R
# Functions:
#   AdjustDate: Attempts to convert invalid dates to valid and also move off week-ends and holidays
#   PaymentsRemaining: Calculates number of payment dates remaing
#   NAD: Number of accrued days
#   LSCRate: Estimates rate based on maturity and LSC model
#     Discount rate: Maturity is at end of period 
#     Forward rate: Maturity is at beginning of rate period
#   SwapRate: Given details of the discount curve and forward curve compute swap rate
#
# Appraise the validity of a date: (And adjust as necessary)
#   TM - Test month, TD - Test day, TY - Test year
AdjustDate = function(TM, TD, TY, Convention){
  TDate <- mdy.date(TM, TD, TY, nineteen = FALSE)
  # Evaluate month: If < 1, make 12 and decrement year, if > 12, make 12
  while(is.na(TDate)){
    if(TM < 1){
      TM = 12
      TY = TY - 1
    }
    if(TM > 12) TM = 12
    TDate = mdy.date(TM, TD, TY, nineteen = FALSE)
    while(is.na(TDate)){
      # Decrement one day: Note TD = 1 is always valid
      if(TD > 0)TD = TD - 1
      # If Day is negative
      if(TD <= 0){
        # Decrement month by 1, start with TD = 31
        TM = TM - 1
        if(TM < 1){
          TM = 12
          TY = TY - 1
        }
        TD = 31
      }
      TDate = mdy.date(TM, TD, TY, nineteen = FALSE)
    }
  }
  # Modified business preceeding
  if(Convention == "MBP"){
    TDayOfWeek = weekdays(as.Date(TDate))
    if(TDayOfWeek == "Sunday"){
      TD = TD - 2
    } else if(TDayOfWeek == "Saturday"){
      TD = TD - 1
    }
    TDate = mdy.date(TM, TD, TY, nineteen = FALSE)
    while(is.na(TDate)){
      # Decrement one day: Note TD = 1 is always valid
      if(TD > 0)TD = TD - 1
      # If Day is negative
      if(TD <= 0){
        # Decrement month by 1, start with TD = 31
        TM = TM - 1
        if(TM < 1){
          TM = 12
          TY = TY - 1
        }
        TD = 31
      }
      TDate = mdy.date(TM, TD, TY, nineteen = FALSE)
    }
    if(!isBusinessDay(TDate))TD = TD - 1
    TDate = mdy.date(TM, TD, TY, nineteen = FALSE)
    while(is.na(TDate)){
      # Decrement one day: Note TD = 1 is always valid
      if(TD > 0)TD = TD - 1
      # If Day is negative
      if(TD <= 0){
        # Decrement month by 1, start with TD = 31
        TM = TM - 1
        if(TM < 1){
          TM = 12
          TY = TY - 1
        }
        TD = 31
      }
      TDate = mdy.date(TM, TD, TY, nineteen = FALSE)
    }
    # Check preceding date is week-end
    TDayOfWeek = weekdays(as.Date(TDate))
    if(TDayOfWeek == "Sunday"){
      TD = TD - 2
    } else if(TDayOfWeek == "Saturday"){
      TD = TD - 1
    }
    TDate = mdy.date(TM, TD, TY, nineteen = FALSE)
    while(is.na(TDate)){
      # Decrement one day: Note TD = 1 is always valid
      if(TD > 0)TD = TD - 1
      # If Day is negative
      if(TD <= 0){
        # Decrement month by 1, start with TD = 31
        TM = TM - 1
        if(TM < 1){
          TM = 12
          TY = TY - 1
        }
        TD = 31
      }
      TDate = mdy.date(TM, TD, TY, nineteen = FALSE)
    }
  }
  # Modified business following
  if(Convention == "MBF"){
    TDayOfWeek = weekdays(as.Date(TDate))
    if(TDayOfWeek == "Sunday"){
      TD = TD + 1
    } else if(TDayOfWeek == "Saturday"){
      TD = TD + 2
    }
    TDate = mdy.date(TM, TD, TY, nineteen = FALSE)
    while(is.na(TDate)){
      # Decrement one day: Note TD = 1 is always valid
      if(TD > 0)TD = TD - 1
      # If Day is negative
      if(TD <= 0){
        # Decrement month by 1, start with TD = 31
        TM = TM - 1
        if(TM < 1){
          TM = 12
          TY = TY - 1
        }
        TD = 31
      }
      TDate = mdy.date(TM, TD, TY, nineteen = FALSE)
    }
    if(!isBusinessDay(TDate))TD = TD + 1
    TDate = mdy.date(TM, TD, TY, nineteen = FALSE)
    while(is.na(TDate)){
      # Decrement one day: Note TD = 1 is always valid
      if(TD > 0)TD = TD - 1
      # If Day is negative
      if(TD <= 0){
        # Decrement month by 1, start with TD = 31
        TM = TM - 1
        if(TM < 1){
          TM = 12
          TY = TY - 1
        }
        TD = 31
      }
      TDate = mdy.date(TM, TD, TY, nineteen = FALSE)
    }
  }
  # Check if following is a week-end
  TDayOfWeek = weekdays(as.Date(TDate))
  if(TDayOfWeek == "Sunday"){
    TD = TD + 1
  } else if(TDayOfWeek == "Saturday"){
    TD = TD + 2
  }
  TDate = mdy.date(TM, TD, TY, nineteen = FALSE)
  while(is.na(TDate)){
    # Decrement one day: Note TD = 1 is always valid
    if(TD > 0)TD = TD - 1
    # If Day is negative
    if(TD <= 0){
      # Decrement month by 1, start with TD = 31
      TM = TM - 1
      if(TM < 1){
        TM = 12
        TY = TY - 1
      }
      TD = 31
    }
    TDate = mdy.date(TM, TD, TY, nineteen = FALSE)
  }
  return(mdy.date(TM, TD, TY, nineteen = FALSE))
}
#
# Compute number of payments remaining: Handles annual, semi-annual,
#  quarterly, and monthly payments
#
PaymentsRemaining = function(N){
  PayDates <- c(1:1200) # Assumes maximum number is 1,200
  PayDates[] <- NA # Fill vector with NA
  with(N,{
    SM = as.numeric(SettlementDateMonth)
    SD = as.numeric(SettlementDateDay)
    SY = as.numeric(SettlementDateYear)
    MM = as.numeric(MaturityDateMonth)
    MD = as.numeric(MaturityDateDay)
    MY = as.numeric(MaturityDateYear)
# Decrement settlement date until it is a valid date (e.g., 2/31/YYYY)
    JSettlementDate = AdjustDate(SM, SD, SY, Convention)
# Decrement maturity date until it is a valid date (e.g., 2/31/YYYY)
    JMaturityDate = AdjustDate(MM, MD, MY, Convention)
    PayDates[1] = JMaturityDate
# Solve for number of remaining cash flows depending on payment frequency
    Counter = 0
    if(Frequency == 1){   # Annual
      while (JMaturityDate > JSettlementDate){
        MY = MY - 1
        JMaturityDate = AdjustDate(MM, MD, MY, Convention)
        Counter = Counter + 1
        PayDates[Counter+1] = JMaturityDate
      }
    } else if (Frequency == 2) { # Semi-annual
      while (JMaturityDate > JSettlementDate){
        if(MM > 6){
          MM = MM - 6
          JMaturityDate = AdjustDate(MM, MD, MY, Convention)
        } else {
          MM = MM + 6
          MY = MY - 1
          JMaturityDate = AdjustDate(MM, MD, MY, Convention)
        }
        Counter = Counter + 1
        PayDates[Counter+1] = JMaturityDate
      }
    } else if (Frequency == 4) { # Quarterly
      while (JMaturityDate > JSettlementDate){
        if(MM > 3){
          MM = MM - 3
          JMaturityDate = AdjustDate(MM, MD, MY, Convention)
        } else {
          MM = MM + 9
          MY = MY - 1
          JMaturityDate = AdjustDate(MM, MD, MY, Convention)
        }
        Counter = Counter + 1
        PayDates[Counter+1] = JMaturityDate
      }
    } else { # Monthly
      while (JMaturityDate > JSettlementDate){
        if(MM > 1){
          MM = MM - 1
          JMaturityDate = AdjustDate(MM, MD, MY, Convention)
        } else {
          MM = MM + 11
          MY = MY - 1
          JMaturityDate = AdjustDate(MM, MD, MY, Convention)
        }
        Counter = Counter + 1
        PayDates[Counter+1] = JMaturityDate
      }
    }
    RData <- list(Counter, PayDates)
    names(RData) <- c("Counter", "PayDates")
    return(RData)
  })
}
#
# NAD: Number of Accrued Days
#  Finds last payment date based on maturity date and rolling backward
#  Then computes number of days between last payment date and evaluation date
#
NAD = function(N){
  with(N, {
    SM = as.numeric(SettlementDateMonth)
    SD = as.numeric(SettlementDateDay)
    SY = as.numeric(SettlementDateYear)
    MM = as.numeric(MaturityDateMonth)
    MD = as.numeric(MaturityDateDay)
    MY = as.numeric(MaturityDateYear)
# Decrement settlement date until it is a valid date (e.g., 2/31/YYYY)
    JSettlementDate = AdjustDate(SM, SD, SY, Convention)
# Decrement maturity date until it is a valid date (e.g., 2/31/YYYY)
    JMaturityDate = AdjustDate(MM, MD, MY, Convention)
    Counter = 0
    if(Frequency == 1){   # Annual
      while (JMaturityDate > JSettlementDate){
        MY = MY - 1
        JMaturityDate = AdjustDate(MM, MD, MY, Convention)
        Counter = Counter + 1
      }
      MY = MY + 1
      JNextMaturityDate = AdjustDate(MM, MD, MY, Convention)
      OutNAD = as.numeric(JSettlementDate - JMaturityDate)
    } else if (Frequency == 2) { # Semi-annual
      while (JMaturityDate > JSettlementDate){
        if(MM > 6){
          MM = MM - 6
          JMaturityDate = AdjustDate(MM, MD, MY, Convention)
        } else {
          MM = MM + 6
          MY = MY - 1
          JMaturityDate = AdjustDate(MM, MD, MY, Convention)
        }
        Counter = Counter + 1
      }
      if(MM > 6){
        MM = MM - 6
        MY = MY + 1
      } else {
        MM = MM + 6
      }
      JNextMaturityDate = AdjustDate(MM, MD, MY, Convention)
      OutNAD = as.numeric(JSettlementDate - JMaturityDate)
    } else if (Frequency == 4) { # Quarterly
      while (JMaturityDate > JSettlementDate){
        if(MM > 3){
          MM = MM - 3
          JMaturityDate = AdjustDate(MM, MD, MY, Convention)
        } else {
          MM = MM + 9
          MY = MY - 1
          JMaturityDate = AdjustDate(MM, MD, MY, Convention)
        }
        Counter = Counter + 1
      }
      if(MM > 9){
        MM = MM - 9
        MY = MY + 1
      } else {
        MM = MM + 3
      }
      JNextMaturityDate = AdjustDate(MM, MD, MY, Convention)
      OutNAD = as.numeric(JSettlementDate - JMaturityDate)
    } else { # Monthly
      while (JMaturityDate > JSettlementDate){
        if(MM > 1){
          MM = MM - 1
          JMaturityDate = AdjustDate(MM, MD, MY, Convention)
        } else {
          MM = MM + 11
          MY = MY - 1
          JMaturityDate = AdjustDate(MM, MD, MY, Convention)
        }
        Counter = Counter + 1
      }
      if(MM > 11){
        MM = MM - 11
        MY = MY + 1
      } else {
        MM = MM + 1
      }
      JNextMaturityDate = AdjustDate(MM, MD, MY, Convention)
      OutNAD = as.numeric(JSettlementDate - JMaturityDate)
    }
    return(OutNAD)
  })
}
#
# LSC Rates: Based on information in LSC data frame, compute estimated rate
#  for a particular maturity date
#
LSCRate = function(LSC){
  with(LSC, {
    if(NumberOfFactors==1){
      Rate = Intercept
    }
    if(NumberOfFactors==2){
      Rate = Intercept + Slope*((1.0 - exp(-Maturity/Tau1))/(Maturity/Tau1))
    }
    if(NumberOfFactors==3){
      Rate = Intercept + Slope*((1.0 - exp(-Maturity/Tau1))/(Maturity/Tau1)) +
        Curvature1*((1.0 - exp(-Maturity/Tau2))/(Maturity/Tau2) - exp(-Maturity/Tau2))
    }
    if(NumberOfFactors==4){
      Rate = Intercept + Slope*((1.0 - exp(-Maturity/Tau1))/(Maturity/Tau1)) +
        Curvature1*((1.0 - exp(-Maturity/Tau2))/(Maturity/Tau2) - exp(-Maturity/Tau2)) +
        Curvature2*((1.0 - exp(-Maturity/Tau3))/(Maturity/Tau3) - exp(-Maturity/Tau3))
    }
    if(NumberOfFactors==5){
      Rate = Intercept + Slope*((1.0 - exp(-Maturity/Tau1))/(Maturity/Tau1)) +
        Curvature1*((1.0 - exp(-Maturity/Tau2))/(Maturity/Tau2) - exp(-Maturity/Tau2)) +
        Curvature2*((1.0 - exp(-Maturity/Tau3))/(Maturity/Tau3) - exp(-Maturity/Tau3)) +
        Curvature3*((1.0 - exp(-Maturity/Tau4))/(Maturity/Tau4) - exp(-Maturity/Tau4))
    }
    if(NumberOfFactors==6){
      Rate = Intercept + Slope*((1.0 - exp(-Maturity/Tau1))/(Maturity/Tau1)) +
        Curvature1*((1.0 - exp(-Maturity/Tau2))/(Maturity/Tau2) - exp(-Maturity/Tau2)) +
        Curvature2*((1.0 - exp(-Maturity/Tau3))/(Maturity/Tau3) - exp(-Maturity/Tau3)) +
        Curvature3*((1.0 - exp(-Maturity/Tau4))/(Maturity/Tau4) - exp(-Maturity/Tau4)) +
        Curvature4*((1.0 - exp(-Maturity/Tau5))/(Maturity/Tau5) - exp(-Maturity/Tau5))
    }
    return(Rate)
  })
}
#
# Swap Rate: Compute fixed swap rate given all other inputs
#
SwapRateGC = function(dfSWAP){
  with(dfSWAP, {
# 
#     attach(SWAPINP)
# 
#    
# Work on components for Fixed
#
# Either Modified Business Following (MBF), Modified Business Preceding (MBP)
    Convention <- FixConv 
    NData <- list(EM, ED, EY, MM, MD, MY, Convention, FixPF)
    names(NData) <- c("SettlementDateMonth", "SettlementDateDay", 
      "SettlementDateYear", "MaturityDateMonth", "MaturityDateDay", 
      "MaturityDateYear", "Convention", "Frequency")
    N <- NData
    NTDFix = FixNTD
    FixData <- PaymentsRemaining(N)
    nFix <- FixData$Counter
    CFDatesFix <- as.date(FixData$PayDates)
# Remove NAs and sort in ascending order
    CFDatesFix <- sort(CFDatesFix[!is.na(CFDatesFix)])
    CFDatesFixDate <- format(as.Date(CFDatesFix),"%m/%d/%y")
    NADFix <- as.numeric(c(1:nFix))
    NADFix[] <- 0
    APFix <- NADFix
    MaturityFix <- APFix
    for(i in 1:nFix){
      if(FixNAD < 1) NADFix[i] = CFDatesFix[i+1] - CFDatesFix[i]
      else {
        if(FixPF == 1) NADFix[i] = 360
        else if (FixPF == 2) NADFix[i] = 180
        else if (FixPF == 4) NADFix[i] = 90
        else NADFix[i] = 30 
      }
      APFix[i] = NADFix[i]/NTDFix
      MaturityFix[i] = (CFDatesFix[i+1] - CFDatesFix[1])/365
    }
    # NADFix; APFix; MaturityFix
# Set main parameters for LSC forward rates
    Maturity = MaturityFix[1]
    NumberOfFactors = NLSCFR
    Intercept = FRParm1
    Slope = FRParm2
    Curvature1 = FRParm3
    Curvature2 = FRParm4
    Curvature3 = FRParm5
    Curvature4 = FRParm6
    Tau1 = FRScalar1
    Tau2 = FRScalar2
    Tau3 = FRScalar3
    Tau4 = FRScalar4
    Tau5 = FRScalar5
    LSCFC <- list(Maturity, NumberOfFactors, Intercept, Slope, Curvature1, 
      Curvature2, Curvature3, Curvature4, Tau1, Tau2, Tau3, Tau4, Tau5)
    names(LSCFC) <- c("Maturity", "NumberOfFactors", "Intercept", "Slope", 
      "Curvature1", "Curvature2", "Curvature3", "Curvature4", 
      "Tau1", "Tau2", "Tau3", "Tau4", "Tau5")    
# Set main parameters for LSC generic rates
    NumberOfFactors = NLSCGR
    Intercept = GRParm1
    Slope = GRParm2
    Curvature1 = GRParm3
    Curvature2 = GRParm4
    Curvature3 = GRParm5
    Curvature4 = GRParm6
    Tau1 = GRScalar1
    Tau2 = GRScalar2
    Tau3 = GRScalar3
    Tau4 = GRScalar4
    Tau5 = GRScalar5
    LSCGC <- list(Maturity, NumberOfFactors, Intercept, Slope, Curvature1, 
      Curvature2, Curvature3, Curvature4, Tau1, Tau2, Tau3, Tau4, Tau5)
    names(LSCGC) <- c("Maturity", "NumberOfFactors", "Intercept", "Slope", 
      "Curvature1", "Curvature2", "Curvature3", "Curvature4", 
      "Tau1", "Tau2", "Tau3", "Tau4", "Tau5")
# Calculate discount factors
    DR <- as.numeric(c(1:nFix))
    DR[] <- 0
    DF <- DR
    AFix = 0
    for(i in 1:nFix){
      LSCFC$Maturity = LSCGC$Maturity = MaturityFix[i] 
      if(DiscountType == 'BC'){
        DR[i] = (LSCRate(LSCFC) + LSCRate(LSCGC))/100.0
      } else {
        DR[i] = LSCRate(LSCGC)/100.0
      }
      DF[i] = exp(-DR[i]*MaturityFix[i])
      AFix = AFix + APFix[i]*NAmt*DF[i]
    }
#
# Work on components for Floating
#
    NTDFlt = FltNTD
    N$Frequency <- FltPF
    N$Convention <- FltConv 
    FltData <- PaymentsRemaining(N)
    nFlt <- FltData$Counter
    CFDatesFlt <- as.date(FltData$PayDates)
    # Remove NAs and sort in ascending order
    CFDatesFlt <- sort(CFDatesFlt[!is.na(CFDatesFlt)])
    CFDatesFltDate <- format(as.Date(CFDatesFlt),"%m/%d/%y")
    NADFlt <- as.numeric(c(1:nFlt))
    NADFlt[] <- 0
    APFlt <- NADFlt
    MaturityFlt <- APFlt
    for(i in 1:nFlt){
      if(FltNAD < 1) NADFlt[i] = CFDatesFlt[i+1] - CFDatesFlt[i]
      else {
        if(FltPF == 1) NADFlt[i] = 360
        else if (FltPF == 2) NADFlt[i] = 180
        else if (FltPF == 4) NADFlt[i] = 90
        else NADFlt[i] = 30 
      }
      APFlt[i] = NADFlt[i]/NTDFlt
      MaturityFlt[i] = (CFDatesFlt[i+1] - CFDatesFlt[1])/365
    }
# LSC discount rates for floating
# Calculate discount factors
    DR <- as.numeric(c(1:nFlt))
    DR[] <- 0
    DF <- DR
    AFlt = 0
    for(i in 1:nFlt){
      LSCFC$Maturity = LSCGC$Maturity = MaturityFlt[i] 
      if(DiscountType == 'BC'){
        DR[i] = (LSCRate(LSCFC) + LSCRate(LSCGC))/100.0
      } else {
        DR[i] = LSCRate(LSCGC)/100.0
      }
      DF[i] = exp(-DR[i]*MaturityFlt[i])
    }
# LSC forward rate analysis
    FR <- as.numeric(c(1:nFlt))
    FR[] <- 0
    AFlt <- FR
    TAFlt <- 0
    # Advanced set, settled in arrears assumed
    LSCFC$Maturity = 0.0001 
    FR[1] <- LSCRate(LSCFC)/100.0
    LFR <- FR[1] # Lagged forward rate used later
    FR[1] <- (exp(FR[1]*MaturityFlt[1]) - 1.0)/(NADFlt[1]/NTDFlt) # Periodic forward rate
    AFlt[1] = APFlt[1]*NAmt*DF[1]*FR[1]
    TAFlt = TAFlt + AFlt[1]
    for(i in 2:nFlt){
      LSCFC$Maturity = MaturityFlt[i] 
      FR[i] = LSCRate(LSCFC)/100.0
      TFR <- FR[i]
      FR[i] <- (exp(FR[i]*MaturityFlt[i]-LFR*MaturityFlt[i-1]) - 1.0) / 
        (NADFlt[i]/NTDFlt)
      LFR <- TFR
      AFlt[i] = APFlt[i]*NAmt*DF[i]*FR[i]
      TAFlt = TAFlt + AFlt[i]
    }
# Use both LSC for forward and discount    
    if(Output == 'Rate1' || Output == 'Value1'){
      FixSwapRate1 = (TAFlt/AFix)*100.0
      SwapValue1 = SwapType*((FixedRate/100.0)*AFix - TAFlt)
      EquilibriumSwapValueCheck = SwapType*((FixSwapRate1/100.0)*AFix-TAFlt)
      if(Output == 'Rate1')return(FixSwapRate1) 
      if(Output == 'Value1')return(SwapValue1) 
    }
# Use both discount LSC only    
    else if(Output == 'Rate2' || Output == 'Value2'){
      FixSwapRate2 = (NAmt*(1.0 - DF[nFlt])/AFix)*100.0
      SwapValue2 = SwapType*((FixedRate/100.0)*AFix - NAmt*(1.0 - DF[nFlt]))
      EquilibriumSwapValueCheck = SwapType*((FixSwapRate2/100.0)*AFix-TAFlt)
      if(Output == 'Rate2')return(FixSwapRate2) 
      if(Output == 'Value2')return(SwapValue2) 
    }
    else return(-99)
  })
}
#
# Floating term 1
#
TFlt1 = function(dfSWAP){
  with(dfSWAP, {
# Set main parameters for LSC forward rates
    Maturity = 0.00001 # Placeholder, not used
    NumberOfFactors = NLSCFR
    Intercept = FRParm1
    Slope = FRParm2
    Curvature1 = FRParm3
    Curvature2 = FRParm4
    Curvature3 = FRParm5
    Curvature4 = FRParm6
    Tau1 = FRScalar1
    Tau2 = FRScalar2
    Tau3 = FRScalar3
    Tau4 = FRScalar4
    Tau5 = FRScalar5
    LSCFC <- list(Maturity, NumberOfFactors, Intercept, Slope, Curvature1, 
      Curvature2, Curvature3, Curvature4, Tau1, Tau2, Tau3, Tau4, Tau5)
    names(LSCFC) <- c("Maturity", "NumberOfFactors", "Intercept", "Slope", 
      "Curvature1", "Curvature2", "Curvature3", "Curvature4", 
      "Tau1", "Tau2", "Tau3", "Tau4", "Tau5")    
# Set main parameters for LSC generic rates
    NumberOfFactors = NLSCGR
    Intercept = GRParm1
    Slope = GRParm2
    Curvature1 = GRParm3
    Curvature2 = GRParm4
    Curvature3 = GRParm5
    Curvature4 = GRParm6
    Tau1 = GRScalar1
    Tau2 = GRScalar2
    Tau3 = GRScalar3
    Tau4 = GRScalar4
    Tau5 = GRScalar5
    LSCGC <- list(Maturity, NumberOfFactors, Intercept, Slope, Curvature1, 
      Curvature2, Curvature3, Curvature4, Tau1, Tau2, Tau3, Tau4, Tau5)
    names(LSCGC) <- c("Maturity", "NumberOfFactors", "Intercept", "Slope", 
      "Curvature1", "Curvature2", "Curvature3", "Curvature4", 
      "Tau1", "Tau2", "Tau3", "Tau4", "Tau5")
# Either Modified Business Following (MBF), Modified Business Preceding (MBP)
    Convention <- FltConv 
    NData <- list(EM, ED, EY, MM, MD, MY, Convention, FltPF)
    names(NData) <- c("SettlementDateMonth", "SettlementDateDay", 
      "SettlementDateYear", "MaturityDateMonth", "MaturityDateDay", 
      "MaturityDateYear", "Convention", "Frequency")
    FltData <- PaymentsRemaining(NData)
    nFlt <- FltData$Counter
    CFDatesFlt <- as.date(FltData$PayDates) # CFDatesFlt[1] is evaluation date
    CFDatesFlt <- sort(CFDatesFlt[!is.na(CFDatesFlt)])
# Stub analysis
    JEvaluationDate <- AdjustDate(EM, ED, EY, Convention)
    EvaluationDateFlt <- as.date(as.integer(JEvaluationDate))
    FR <- as.numeric(c(1:nFlt))
    FR[] <- 0
    BR <- FR
    DF <- FR
    TDF <- 0.0
    for(i in 1:nFlt){  # CFDatesFlt[1] is evaluation date
      # LSCGC$Maturity = (CFDatesFlt[i+1] - CFDatesFlt[1])/365
      LSCGC$Maturity = (CFDatesFlt[i+1] - EvaluationDateFlt)/365
      BR[i] = LSCRate(LSCGC)/100.0
      if(i>1){
        # LSCFC$Maturity = (CFDatesFlt[i] - CFDatesFlt[1])/365 
        LSCFC$Maturity = (CFDatesFlt[i] - EvaluationDateFlt)/365 
      } else {
        LSCFC$Maturity = 0.00001
      }
      FR[i] = LSCRate(LSCFC)/100.0
      DF[i] = exp(-(BR[i]*LSCGC$Maturity + FR[i]*LSCFC$Maturity))
      TDF <- DF[i] + TDF
    }
    return( NAmt*TDF)
  })
}
#
# Floating term 2
#
TFlt2 = function(dfSWAP){
  with(dfSWAP, {
# Set main parameters for LSC forward rates
    Maturity = 0.00001 # Placeholder, not used
    NumberOfFactors = NLSCFR
    Intercept = FRParm1
    Slope = FRParm2
    Curvature1 = FRParm3
    Curvature2 = FRParm4
    Curvature3 = FRParm5
    Curvature4 = FRParm6
    Tau1 = FRScalar1
    Tau2 = FRScalar2
    Tau3 = FRScalar3
    Tau4 = FRScalar4
    Tau5 = FRScalar5
    LSCFC <- list(Maturity, NumberOfFactors, Intercept, Slope, Curvature1, 
      Curvature2, Curvature3, Curvature4, Tau1, Tau2, Tau3, Tau4, Tau5)
    names(LSCFC) <- c("Maturity", "NumberOfFactors", "Intercept", "Slope", 
      "Curvature1", "Curvature2", "Curvature3", "Curvature4", 
      "Tau1", "Tau2", "Tau3", "Tau4", "Tau5")    
# Set main parameters for LSC generic rates
    NumberOfFactors = NLSCGR
    Intercept = GRParm1
    Slope = GRParm2
    Curvature1 = GRParm3
    Curvature2 = GRParm4
    Curvature3 = GRParm5
    Curvature4 = GRParm6
    Tau1 = GRScalar1
    Tau2 = GRScalar2
    Tau3 = GRScalar3
    Tau4 = GRScalar4
    Tau5 = GRScalar5
    LSCGC <- list(Maturity, NumberOfFactors, Intercept, Slope, Curvature1, 
      Curvature2, Curvature3, Curvature4, Tau1, Tau2, Tau3, Tau4, Tau5)
    names(LSCGC) <- c("Maturity", "NumberOfFactors", "Intercept", "Slope", 
      "Curvature1", "Curvature2", "Curvature3", "Curvature4", 
      "Tau1", "Tau2", "Tau3", "Tau4", "Tau5")
# Either Modified Business Following (MBF), Modified Business Preceding (MBP)
    Convention <- FltConv 
    NData <- list(EM, ED, EY, MM, MD, MY, Convention, FltPF)
    names(NData) <- c("SettlementDateMonth", "SettlementDateDay", 
      "SettlementDateYear", "MaturityDateMonth", "MaturityDateDay", 
      "MaturityDateYear", "Convention", "Frequency")
    FltData <- PaymentsRemaining(NData)
    nFlt <- FltData$Counter
    CFDatesFlt <- as.date(FltData$PayDates) # CFDatesFlt[1] is evaluation date
    CFDatesFlt <- sort(CFDatesFlt[!is.na(CFDatesFlt)])
# Stub analysis
    JEvaluationDate <- AdjustDate(EM, ED, EY, Convention)
    EvaluationDateFlt <- as.date(as.integer(JEvaluationDate))
    FR <- as.numeric(c(1:nFlt))
    FR[] <- 0
    BR <- FR
    DF <- FR
    TDF <- 0.0
    for(i in 1:nFlt){  # CFDatesFlt[1] is evaluation date
      LSCFC$Maturity = LSCGC$Maturity = (CFDatesFlt[i+1] - EvaluationDateFlt)/365
      BR[i] = LSCRate(LSCGC)/100.0
      FR[i] = LSCRate(LSCFC)/100.0
      DF[i] = exp(-(BR[i]*LSCGC$Maturity + FR[i]*LSCGC$Maturity))
      TDF <- DF[i] + TDF
    }
    return( NAmt*TDF)
  })
}
#
# Fixed term 1
#
TFix1 = function(dfSWAP){
  with(dfSWAP, {
# Set main parameters for LSC forward rates
    Maturity = 0.00001 # Placeholder, not used
    NumberOfFactors = NLSCFR
    Intercept = FRParm1
    Slope = FRParm2
    Curvature1 = FRParm3
    Curvature2 = FRParm4
    Curvature3 = FRParm5
    Curvature4 = FRParm6
    Tau1 = FRScalar1
    Tau2 = FRScalar2
    Tau3 = FRScalar3
    Tau4 = FRScalar4
    Tau5 = FRScalar5
    LSCFC <- list(Maturity, NumberOfFactors, Intercept, Slope, Curvature1, 
      Curvature2, Curvature3, Curvature4, Tau1, Tau2, Tau3, Tau4, Tau5)
    names(LSCFC) <- c("Maturity", "NumberOfFactors", "Intercept", "Slope", 
      "Curvature1", "Curvature2", "Curvature3", "Curvature4", 
      "Tau1", "Tau2", "Tau3", "Tau4", "Tau5")    
# Set main parameters for LSC generic rates
    NumberOfFactors = NLSCGR
    Intercept = GRParm1
    Slope = GRParm2
    Curvature1 = GRParm3
    Curvature2 = GRParm4
    Curvature3 = GRParm5
    Curvature4 = GRParm6
    Tau1 = GRScalar1
    Tau2 = GRScalar2
    Tau3 = GRScalar3
    Tau4 = GRScalar4
    Tau5 = GRScalar5
    LSCGC <- list(Maturity, NumberOfFactors, Intercept, Slope, Curvature1, 
      Curvature2, Curvature3, Curvature4, Tau1, Tau2, Tau3, Tau4, Tau5)
    names(LSCGC) <- c("Maturity", "NumberOfFactors", "Intercept", "Slope", 
      "Curvature1", "Curvature2", "Curvature3", "Curvature4", 
      "Tau1", "Tau2", "Tau3", "Tau4", "Tau5")
# Either Modified Business Following (MBF), Modified Business Preceding (MBP)
    Convention <- FixConv 
    NData <- list(EM, ED, EY, MM, MD, MY, Convention, FixPF)
    names(NData) <- c("SettlementDateMonth", "SettlementDateDay", 
      "SettlementDateYear", "MaturityDateMonth", "MaturityDateDay", 
      "MaturityDateYear", "Convention", "Frequency")
    FixData <- PaymentsRemaining(NData)
    nFix <- FixData$Counter
    CFDatesFix <- as.date(FixData$PayDates) # CFDatesFlt[1] is evaluation date
    CFDatesFix <- sort(CFDatesFix[!is.na(CFDatesFix)])
# Stub analysis
    JEvaluationDate <- AdjustDate(EM, ED, EY, Convention)
    EvaluationDateFix <- as.date(as.integer(JEvaluationDate))
    FR <- as.numeric(c(1:nFix))
    FR[] <- 0
    BR <- FR
    DF <- FR
    APFix <- FR
    NADFix <- APFix
    TDF <- 0.0
    for(i in 1:nFix){  # CFDatesFlt[1] is evaluation date
      LSCFC$Maturity = LSCGC$Maturity = (CFDatesFix[i+1] - EvaluationDateFix)/365
      BR[i] = LSCRate(LSCGC)/100.0
      FR[i] = LSCRate(LSCFC)/100.0
      DF[i] = exp(-(BR[i]*LSCGC$Maturity + FR[i]*LSCGC$Maturity))
      if(FixNAD < 1) NADFix[i] = CFDatesFix[i+1] - CFDatesFix[i]
      else {
        if(FixPF == 1) NADFix[i] = 360
        else if (FixPF == 2) NADFix[i] = 180
        else if (FixPF == 4) NADFix[i] = 90
        else NADFix[i] = 30
      }
      APFix[i] = NADFix[i]/FixNTD
      TDF <- APFix[i]*DF[i] + TDF
    }
    return( NAmt*TDF)
  })
}
# Swap rates based on forward curve and basis curve
SwapRates3T <- function(SWAPINP){
  FltTerm1 <- TFlt1(SWAPINP)
  FltTerm2 <- TFlt2(SWAPINP)
  FixTerm <- TFix1(SWAPINP)  
  return(100*(FltTerm1 - FltTerm2)/FixTerm)
}
# Swap values based on forward curve and basis curve
SwapValue3T <- function(SWAPINP){
  FltTerm1 <- TFlt1(SWAPINP)
  FltTerm2 <- TFlt2(SWAPINP)
  FixTerm <- TFix1(SWAPINP)  
  SV <- -SWAPINP$SwapType*(FltTerm1 - FltTerm2 - (SWAPINP$FixedRate/100)*FixTerm)  
  return(SV)
}
#
# Given coefficients for discount curve based on LSC, estimate squared difference
#  Used in optimization
#
DiffSwRates <- function(x, NFac, S, NSwaps, MSR){
  C <- numeric(NFac)   # Coefficients
  C[1] <- 1.0
  SR <- numeric(NSwaps)
  for(j in 1:NSwaps){
    NFix <- j*2 # Semi-annual pay (30/360 assumed)
    r <- numeric(NFix) # Discount rates
    DF <- numeric(NFix) # Discount factors  
    for(i in 1:NFix){
      tau = i/2.0
      if(NFac==1){
        r[i] <- C[1]*x[1]
      }
      if(NFac==2){
        C[2] <- (S[1]/tau)*(1-exp(-tau/S[1]))
        r[i] <- C[1]*x[1] + C[2]*x[2]
      }
      if(NFac==3){
        C[2] <- (S[1]/tau)*(1-exp(-tau/S[1]))
        C[3] <- (S[1]/tau)*(1-exp(-tau/S[1])) - exp(-tau/S[1]) 
        r[i] <- C[1]*x[1] + C[2]*x[2] + C[3]*x[3]
      }
      if(NFac==4){
        C[2] <- (S[1]/tau)*(1-exp(-tau/S[1]))
        C[3] <- (S[1]/tau)*(1-exp(-tau/S[1])) - exp(-tau/S[1]) 
        C[4] <- (S[2]/tau)*(1-exp(-tau/S[2])) - exp(-tau/S[2]) 
        r[i] <- C[1]*x[1] + C[2]*x[2] + C[3]*x[3] + C[4]*x[4]
      }
      if(NFac==5){
        C[2] <- (S[1]/tau)*(1-exp(-tau/S[1]))
        C[3] <- (S[1]/tau)*(1-exp(-tau/S[1])) - exp(-tau/S[1]) 
        C[4] <- (S[2]/tau)*(1-exp(-tau/S[2])) - exp(-tau/S[2]) 
        C[5] <- (S[3]/tau)*(1-exp(-tau/S[3])) - exp(-tau/S[3]) 
        r[i] <- C[1]*x[1] + C[2]*x[2] + C[3]*x[3] + C[4]*x[4] + C[5]*x[5]
      }
      if(NFac>5){
        C[2] <- (S[1]/tau)*(1-exp(-tau/S[1]))
        C[3] <- (S[1]/tau)*(1-exp(-tau/S[1])) - exp(-tau/S[1]) 
        C[4] <- (S[2]/tau)*(1-exp(-tau/S[2])) - exp(-tau/S[2]) 
        C[5] <- (S[3]/tau)*(1-exp(-tau/S[3])) - exp(-tau/S[3]) 
        C[6] <- (S[4]/tau)*(1-exp(-tau/S[4])) - exp(-tau/S[4]) 
        r[i] <- C[1]*x[1] + C[2]*x[2] + C[3]*x[3] + C[4]*x[4] + C[5]*x[5] + 
          C[6]*x[6]
      }
      DF[i] <- exp(-(r[i]/100.0)*tau)
    }
    # r; DF
    SR[j] <- ((1.0 - DF[NFix])/(0.5*sum(DF)))*100.0
  }
  Diff <- sum((MSR - SR)^2, na.rm = TRUE)
  return(Diff)
}
#
# Given LSC estimates of discount rates, compute swap curve
#
SwRates <- function(y, NFac, S, NSwaps){
  C <- numeric(NFac)   # Coefficients
  C[1] <- 1.0
  SR <- numeric(NSwaps)
  for(j in 1:NSwaps){
    NFix <- j*2 # Semi-annual pay (30/360 assumed)
    r <- numeric(NFix) # Discount rates
    DF <- numeric(NFix) # Discount factors  
    for(i in 1:NFix){
      tau = i/2.0
      if(NFac==1){
        r[i] <- C[1]*y[1]
      }
      if(NFac==2){
        C[2] <- (S[1]/tau)*(1-exp(-tau/S[1]))
        r[i] <- C[1]*y[1] + C[2]*y[2]
      }
      if(NFac==3){
        C[2] <- (S[1]/tau)*(1-exp(-tau/S[1]))
        C[3] <- (S[1]/tau)*(1-exp(-tau/S[1])) - exp(-tau/S[1]) 
        r[i] <- C[1]*y[1] + C[2]*y[2] + C[3]*y[3]
      }
      if(NFac==4){
        C[2] <- (S[1]/tau)*(1-exp(-tau/S[1]))
        C[3] <- (S[1]/tau)*(1-exp(-tau/S[1])) - exp(-tau/S[1]) 
        C[4] <- (S[2]/tau)*(1-exp(-tau/S[2])) - exp(-tau/S[2]) 
        r[i] <- C[1]*y[1] + C[2]*y[2] + C[3]*y[3] + C[4]*y[4]
      }
      if(NFac==5){
        C[2] <- (S[1]/tau)*(1-exp(-tau/S[1]))
        C[3] <- (S[1]/tau)*(1-exp(-tau/S[1])) - exp(-tau/S[1]) 
        C[4] <- (S[2]/tau)*(1-exp(-tau/S[2])) - exp(-tau/S[2]) 
        C[5] <- (S[3]/tau)*(1-exp(-tau/S[3])) - exp(-tau/S[3]) 
        r[i] <- C[1]*y[1] + C[2]*y[2] + C[3]*y[3] + C[4]*y[4] + C[5]*y[5]
      }
      if(NFac>5){
        C[2] <- (S[1]/tau)*(1-exp(-tau/S[1]))
        C[3] <- (S[1]/tau)*(1-exp(-tau/S[1])) - exp(-tau/S[1]) 
        C[4] <- (S[2]/tau)*(1-exp(-tau/S[2])) - exp(-tau/S[2]) 
        C[5] <- (S[3]/tau)*(1-exp(-tau/S[3])) - exp(-tau/S[3]) 
        C[6] <- (S[4]/tau)*(1-exp(-tau/S[4])) - exp(-tau/S[4]) 
        r[i] <- C[1]*y[1] + C[2]*y[2] + C[3]*y[3] + C[4]*y[4] + C[5]*y[5] + 
          C[6]*y[6]
      }
      DF[i] <- exp(-(r[i]/100.0)*tau)
    }
    # Other possible outputs for debugging: r, DF
    SR[j] <- ((1.0 - DF[NFix])/(0.5*sum(DF)))*100.0
  }
  return(SR)
}
#
# Given LSC estimates of discount curve, estimate discount rates
#
DiscountRates <- function(y, NFac, S, NSwaps){
  C <- numeric(NFac)   # Coefficients
  C[1] <- 1.0
  SR <- numeric(NSwaps)
  r <- numeric(NSwaps) # Discount rates
  for(j in 1:NSwaps){
    tau = j
    if(NFac==1){
      r[j] <- C[1]*y[1]
    }
    if(NFac==2){
      C[2] <- (S[1]/tau)*(1-exp(-tau/S[1]))
      r[j] <- C[1]*y[1] + C[2]*y[2]
    }
    if(NFac==3){
      C[2] <- (S[1]/tau)*(1-exp(-tau/S[1]))
      C[3] <- (S[1]/tau)*(1-exp(-tau/S[1])) - exp(-tau/S[1]) 
      r[j] <- C[1]*y[1] + C[2]*y[2] + C[3]*y[3]
    }
    if(NFac==4){
      C[2] <- (S[1]/tau)*(1-exp(-tau/S[1]))
      C[3] <- (S[1]/tau)*(1-exp(-tau/S[1])) - exp(-tau/S[1]) 
      C[4] <- (S[2]/tau)*(1-exp(-tau/S[2])) - exp(-tau/S[2]) 
      r[j] <- C[1]*y[1] + C[2]*y[2] + C[3]*y[3] + C[4]*y[4]
    }
    if(NFac==5){
      C[2] <- (S[1]/tau)*(1-exp(-tau/S[1]))
      C[3] <- (S[1]/tau)*(1-exp(-tau/S[1])) - exp(-tau/S[1]) 
      C[4] <- (S[2]/tau)*(1-exp(-tau/S[2])) - exp(-tau/S[2]) 
      C[5] <- (S[3]/tau)*(1-exp(-tau/S[3])) - exp(-tau/S[3]) 
      r[j] <- C[1]*y[1] + C[2]*y[2] + C[3]*y[3] + C[4]*y[4] + C[5]*y[5]
    }
    if(NFac>5){
      C[2] <- (S[1]/tau)*(1-exp(-tau/S[1]))
      C[3] <- (S[1]/tau)*(1-exp(-tau/S[1])) - exp(-tau/S[1]) 
      C[4] <- (S[2]/tau)*(1-exp(-tau/S[2])) - exp(-tau/S[2]) 
      C[5] <- (S[3]/tau)*(1-exp(-tau/S[3])) - exp(-tau/S[3]) 
      C[6] <- (S[4]/tau)*(1-exp(-tau/S[4])) - exp(-tau/S[4]) 
      r[j] <- C[1]*y[1] + C[2]*y[2] + C[3]*y[3] + C[4]*y[4] + C[5]*y[5] + 
        C[6]*y[6]
    }
  }
  return(r)
}
