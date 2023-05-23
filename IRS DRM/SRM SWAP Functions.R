# SRM SWAP Functions.R
source('SWAP GC Functions.R')
#
# Swap values based on forward curve and basis curve 
# Parameters at t, evaluated at t
#
SwapValuett <- function(SWAPINP){
# Move horizon date information to evaluation date  
  SWAPINP1 <- SWAPINP
  FltTerm1 <- TFlt1(SWAPINP1)
  FltTerm2 <- TFlt2(SWAPINP1)
  FixTerm <- TFix1(SWAPINP1)  
  SV <- -SWAPINP1$SwapType*(FltTerm1 - FltTerm2 - (SWAPINP1$FixedRate/100)*FixTerm)
  return(SV)
}
#
# Swap values based on forward curve and basis curve 
# Parameters at t, evaluated at t + delta (tdtd)
#
SwapValuettd <- function(SWAPINP, HDInputs){
# Move horizon date information to evaluation date  
  SWAPINP1 <- SWAPINP
  SWAPINP1$EM <- HDInputs$HM
  SWAPINP1$ED <- HDInputs$HD
  SWAPINP1$EY <- HDInputs$HY
  FltTerm1 <- TFlt1(SWAPINP1)
  FltTerm2 <- TFlt2(SWAPINP1)
  FixTerm <- TFix1(SWAPINP1)  
  SV <- -SWAPINP1$SwapType*(FltTerm1 - FltTerm2 - (SWAPINP1$FixedRate/100)*FixTerm)
  return(SV)
}
#
# Swap values based on forward curve and basis curve 
# Parameters at t+delta, evaluated at t + delta (tdtd)
#
SwapValuetdtd <- function(SWAPINP, HDInputs){
  # Move horizon date information to evaluation date  
  SWAPINP1 <- SWAPINP
  SWAPINP1$EM <- HDInputs$HM
  SWAPINP1$ED <- HDInputs$HD
  SWAPINP1$EY <- HDInputs$HY
  SWAPINP1$FRParm1 <- HDInputs$FRParm1
  SWAPINP1$FRParm2 <- HDInputs$FRParm2
  SWAPINP1$FRParm3 <- HDInputs$FRParm3
  SWAPINP1$FRParm4 <- HDInputs$FRParm4
  SWAPINP1$FRParm5 <- HDInputs$FRParm5
  SWAPINP1$FRParm6 <- HDInputs$FRParm6
  SWAPINP1$GRParm1 <- HDInputs$GRParm1
  SWAPINP1$GRParm2 <- HDInputs$GRParm2
  SWAPINP1$GRParm3 <- HDInputs$GRParm3
  SWAPINP1$GRParm4 <- HDInputs$GRParm4
  SWAPINP1$GRParm5 <- HDInputs$GRParm5
  SWAPINP1$GRParm6 <- HDInputs$GRParm6
  SWAPINP1$FRScalar1 <- HDInputs$FRScalar1
  SWAPINP1$FRScalar2 <- HDInputs$FRScalar2
  SWAPINP1$FRScalar3 <- HDInputs$FRScalar3
  SWAPINP1$FRScalar4 <- HDInputs$FRScalar4
  SWAPINP1$FRScalar5 <- HDInputs$FRScalar5
  SWAPINP1$GRScalar1 <- HDInputs$GRScalar1
  SWAPINP1$GRScalar2 <- HDInputs$GRScalar2
  SWAPINP1$GRScalar3 <- HDInputs$GRScalar3
  SWAPINP1$GRScalar4 <- HDInputs$GRScalar4
  SWAPINP1$GRScalar5 <- HDInputs$GRScalar5
  FltTerm1 <- TFlt1(SWAPINP1)
  FltTerm2 <- TFlt2(SWAPINP1)
  FixTerm <- TFix1(SWAPINP1)  
  SV <- -SWAPINP1$SwapType*(FltTerm1 - FltTerm2 - (SWAPINP1$FixedRate/100)*FixTerm) 
  return(SV)
}
#
# Swap values based on forward curve at t + delta and basis curve at t
# 
SwapValueFCtdBCttd <- function(SWAPINP, HDInputs){
  SWAPINP1 <- SWAPINP
  SWAPINP1$EM <- HDInputs$HM
  SWAPINP1$ED <- HDInputs$HD
  SWAPINP1$EY <- HDInputs$HY
  SWAPINP1$FRParm1 <- HDInputs$FRParm1
  SWAPINP1$FRParm2 <- HDInputs$FRParm2
  SWAPINP1$FRParm3 <- HDInputs$FRParm3
  SWAPINP1$FRParm4 <- HDInputs$FRParm4
  SWAPINP1$FRParm5 <- HDInputs$FRParm5
  SWAPINP1$FRParm6 <- HDInputs$FRParm6
  SWAPINP1$FRScalar1 <- HDInputs$FRScalar1
  SWAPINP1$FRScalar2 <- HDInputs$FRScalar2
  SWAPINP1$FRScalar3 <- HDInputs$FRScalar3
  SWAPINP1$FRScalar4 <- HDInputs$FRScalar4
  SWAPINP1$FRScalar5 <- HDInputs$FRScalar5
  FltTerm1 <- TFlt1(SWAPINP1)
  FltTerm2 <- TFlt2(SWAPINP1)
  FixTerm <- TFix1(SWAPINP1)  
  SV <- -SWAPINP1$SwapType*(FltTerm1 - FltTerm2 - (SWAPINP1$FixedRate/100)*FixTerm) 
  return(SV)
}
#
# Swap values based on forward curve at t + delta and basis curve at t
# 
SwapValueFCtBCtdtd <- function(SWAPINP, HDInputs){
  SWAPINP1 <- SWAPINP
  SWAPINP1$EM <- HDInputs$HM
  SWAPINP1$ED <- HDInputs$HD
  SWAPINP1$EY <- HDInputs$HY
  SWAPINP1$GRParm1 <- HDInputs$GRParm1
  SWAPINP1$GRParm2 <- HDInputs$GRParm2
  SWAPINP1$GRParm3 <- HDInputs$GRParm3
  SWAPINP1$GRParm4 <- HDInputs$GRParm4
  SWAPINP1$GRParm5 <- HDInputs$GRParm5
  SWAPINP1$GRParm6 <- HDInputs$GRParm6
  SWAPINP1$GRScalar1 <- HDInputs$GRScalar1
  SWAPINP1$GRScalar2 <- HDInputs$GRScalar2
  SWAPINP1$GRScalar3 <- HDInputs$GRScalar3
  SWAPINP1$GRScalar4 <- HDInputs$GRScalar4
  SWAPINP1$GRScalar5 <- HDInputs$GRScalar5
  FltTerm1 <- TFlt1(SWAPINP1)
  FltTerm2 <- TFlt2(SWAPINP1)
  FixTerm <- TFix1(SWAPINP1)  
  SV <- -SWAPINP1$SwapType*(FltTerm1 - FltTerm2 - (SWAPINP1$FixedRate/100)*FixTerm) 
  return(SV)
}
