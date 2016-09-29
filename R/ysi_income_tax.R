#' YSI Income Tax Assesment Function
#'
#' This function combines all the nessesary income tax functions and data set mutations to be an all-in-one function for calculating Income Tax
#'

#'
#' @param df Argument is intended to be a fresh HILDA data frame
#' @return A data frame
#' @export

ysi_income_tax <- function(df= NULL) {
  temp <- ysi_mutate_combine(df)
  temp$Deduct <- mapply(temp, DEDfn)
  temp <- ysi_tax_inc_mut(temp)
  temp$Supertax <- mapply(SUPERTAXfn, temp$Super_Inc, temp$Age)
  temp$Supertax[is.na(temp$Supertax)] <- 0

  return(temp)
}

#' REG Income Tax Function
#'
#' This function finds the amount of income tax payable, with the 2014 rates
#'
#'
#' @param income Taxable Income
#' @param brackets Tax brackes as vector, typically with the last bracket being Inf
#' @param rates tax rates as vector, must equal number of brakets
#' @return A vector


RegIncTAXfn <- function(income = NULL,brackets=c(18200, 37000, 80000, 180000,Inf),
                        rates=c(0,.19,.325,.37,.45)){
  sum(diff(c(0,pmin(income,brackets)))*rates)
}





#' YSI Taxable Income Standalone Function
#'
#' This function just finds taxable income.
#'
#' @param df Argument is intended to be a fresh HILDA data frame
#' @return A data frame
#' @export

ysi_tax_inc <- function(df = NULL) {
  temp <- ysi_mutate_combine(df)
  temp$Deduct <- sapply(temp$RegInc, DEDfn)
  temp <- ysi_tax_inc_mut(temp)

  return(temp)
}



#' Medicare Levy Function
#'
#' Medicare Levy  for use on HIDLA, as part of the Income Tax Assesment
#' For use with sapply()
#'
#' Requires TaxInc (Taxable Income) variable to have been calulated previously
#'
#' @param TaxInc Whole dollar value of taxable income
#' @return A vector



MLfn<-function(TaxInc = TaxInc)  {if(TaxInc < 20542)
{ML <- 0
return(ML)}
  else if(TaxInc < 24167)
  {ML <-  0.1*(TaxInc - 20542)
  return(ML)}
  else
  {ML <- 0.015*TaxInc
  return(ML)}
}

#' Low Income Tax Offset (LITO) Function
#'
#' Low Income Tax Offset (LITO) for use on HIDLA, as part of the Income Tax Assesment
#' For use with sapply()
#'
#' Requires TaxInc (Taxable Income) variable to have been calulated previously
#'
#' @param TaxInc Whole dollar value of taxable income
#' @return A vector

LITOfn<-function(TaxInc= TaxInc)  {if(TaxInc < 37000)
{LITO <- 445
return(LITO)}
  else if(TaxInc < 66667)
  {LITO <- 445 - 0.015*(TaxInc - 37000)
  return(LITO)}
  else
  {LITO <- 0
  return(LITO)}
}

#' Mature Age Tax Offset (MATO) Function
#'
#' Mature Age Tax Offset (MATO) for use on HIDLA, as part of the Income Tax Assesment
#' For use with mapply()
#'
#'
#' @param WageIncImp_SalSac Whole dollar value of Wage income with salary sacrifice
#' @param YoB The year of birth

#' @return A vector


MATOfn<-function(WageIncImp_SalSac= WageIncImp_SalSac, YoB= YoB)  {if(WageIncImp_SalSac < 10000)
{MATO <- 0.05*WageIncImp_SalSac
return(MATO)}
  else if(WageIncImp_SalSac < 53000)
  {MATO <- 500
  return(MATO)}
  else if(WageIncImp_SalSac < 63000)
  {MATO <- 500-0.05*(WageIncImp_SalSac-53000)
  return(MATO)}
  else
  {MATO <- 0
  return(MATO)}
}

#' Seniors and Pensioners Tax Offset (SAPTO) Function
#'
#' Seniors and Pensioners Tax Offset (SAPTO) for use on HIDLA, as part of the Income Tax Assesment
#' For use with sapply()
#'
#' Requires TaxInc (Taxable Income) variable to have been calulated previously
#'
#' @param TaxInc Whole dollar value of taxable income
#' @return A vector

SAPTOfn <-function(TaxInc = TaxInc) {if(TaxInc < 32279)
{SAPTO <- 2230
return(SAPTO)}
  else if(TaxInc < 50119)
  {SAPTO <- 2230 - 0.125*(TaxInc - 32279)
  return(SAPTO)}
  else
  {SAPTO <- 0
  return(SAPTO)}
}

#' Other Tax Offsets Function
#'
#' Other Tax Offsets Function for use on HIDLA, as part of the Income Tax Assesment
#' For use with sapply()
#'
#' Requires TaxInc (Taxable Income) variable to have been calulated previously
#'
#' @param TaxInc Whole dollar value of taxable income
#' @return A vector

OTHEROFFfn <- function(TaxInc = TaxInc) {if(TaxInc < 6000)
{ Otheroff <- 0.003 * TaxInc
return(Otheroff)}
  else if(TaxInc < 10000)
  { Otheroff <- 0.002 * TaxInc
  return(Otheroff)}
  else if(TaxInc < 15000)
  { Otheroff <- 0.002 * TaxInc
  return(Otheroff)}
  else if(TaxInc < 20000)
  { Otheroff <- 0.006 * TaxInc
  return(Otheroff)}
  else if(TaxInc < 25000)
  { Otheroff <- 0.007 * TaxInc
  return(Otheroff)}
  else if(TaxInc < 30000)
  { Otheroff <- 0.006 * TaxInc
  return(Otheroff)}
  else if(TaxInc < 35000)
  { Otheroff <- 0.007 * TaxInc
  return(Otheroff)}
  else if(TaxInc < 40000)
  { Otheroff <- 0.006 * TaxInc
  return(Otheroff)}
  else if(TaxInc < 45000)
  { Otheroff <- 0.006 * TaxInc
  return(Otheroff)}
  else if(TaxInc < 50000)
  { Otheroff <- 0.005 * TaxInc
  return(Otheroff)}
  else if(TaxInc < 55000)
  { Otheroff <- 0.005 * TaxInc
  return(Otheroff)}
  else if(TaxInc < 60000)
  { Otheroff <- 0.004 * TaxInc
  return(Otheroff)}
  else if(TaxInc < 70000)
  { Otheroff <- 0.004 * TaxInc
  return(Otheroff)}
  else if(TaxInc < 80000)
  { Otheroff <- 0.004 * TaxInc
  return(Otheroff)}
  else if(TaxInc < 90000)
  { Otheroff <- 0.004 * TaxInc
  return(Otheroff)}
  else if(TaxInc < 100000)
  { Otheroff <- 0.004 * TaxInc
  return(Otheroff)}
  else if(TaxInc < 150000)
  { Otheroff <- 0.005 * TaxInc
  return(Otheroff)}
  else if(TaxInc < 180000)
  { Otheroff <- 0.006 * TaxInc
  return(Otheroff)}
  else if(TaxInc < 250000)
  { Otheroff <- 0.008 * TaxInc
  return(Otheroff)}
  else if(TaxInc < 500000)
  { Otheroff <- 0.008 * TaxInc
  return(Otheroff)}
  else if(TaxInc < 1000000)
  { Otheroff <- 0.005 * TaxInc
  return(Otheroff)}
  else
  { Otheroff <- 0.002 * TaxInc
  return(Otheroff)}
}

#' Deductions Function
#'
#' Deductions Function for use on HIDLA, as part of the Income Tax Assesment
#' For use with sapply()
#'
#' Requires TaxInc (Taxable Income) variable to have been calulated previously
#'
#' @param TaxInc Whole dollar value of taxable income
#' @return A vector


DEDfn <- function(RegInc = RegInc) {if(RegInc < 6000)
{ Deduct <- 0.118 * RegInc
return(Deduct)}
  else if(RegInc < 10000)
  { Deduct <- 0.059 * RegInc
  return(Deduct)}
  else if(RegInc < 15000)
  { Deduct <- 0.049 * RegInc
  return(Deduct)}
  else if(RegInc < 20000)
  { Deduct <- 0.059 * RegInc
  return(Deduct)}
  else if(RegInc < 25000)
  { Deduct <- 0.056 * RegInc
  return(Deduct)}
  else if(RegInc < 30000)
  { Deduct <- 0.054 * RegInc
  return(Deduct)}
  else if(RegInc < 35000)
  { Deduct <- 0.053 * RegInc
  return(Deduct)}
  else if(RegInc < 40000)
  { Deduct <- 0.051 * RegInc
  return(Deduct)}
  else if(RegInc < 45000)
  { Deduct <- 0.048 * RegInc
  return(Deduct)}
  else if(RegInc < 50000)
  { Deduct <- 0.048 * RegInc
  return(Deduct)}
  else if(RegInc < 55000)
  { Deduct <- 0.047 * RegInc
  return(Deduct)}
  else if(RegInc < 60000)
  { Deduct <- 0.047 * RegInc
  return(Deduct)}
  else if(RegInc < 70000)
  { Deduct <- 0.047 * RegInc
  return(Deduct)}
  else if(RegInc < 80000)
  { Deduct <- 0.047 * RegInc
  return(Deduct)}
  else if(RegInc < 90000)
  { Deduct <- 0.046 * RegInc
  return(Deduct)}
  else if(RegInc < 100000)
  { Deduct <- 0.043 * RegInc
  return(Deduct)}
  else if(RegInc < 150000)
  { Deduct <- 0.042 * RegInc
  return(Deduct)}
  else if(RegInc < 180000)
  { Deduct <- 0.042 * RegInc
  return(Deduct)}
  else if(RegInc < 250000)
  { Deduct <- 0.042 * RegInc
  return(Deduct)}
  else if(RegInc < 500000)
  { Deduct <- 0.042 * RegInc
  return(Deduct)}
  else if(RegInc < 1000000)
  { Deduct <- 0.036 * RegInc
  return(Deduct)}
  else
  { Deduct <- 0.029 * RegInc
  return(Deduct)}
}



#' Super Tax Function
#'
#' Super Tax function for use on HIDLA, as part of the Income Tax Assesment
#'
#' @param Super_Inc Whole dollar value of super income
#' @param Age The age of the tax payer
#' @return A vector

SUPERTAXfn <- function(Super_Inc = Super_Inc, Age = Age) {
  if (Super_Inc <  100697) {
    Supertax <- 0
  } else if(Super_Inc > 100696) {
    if (Age > 59){
      Supertax <- 0
    } else if (Age < 55) {
      if (Super_Inc %in% 80001:180000){
        Supertax <- 0.37*Super_Inc
      } else if (Super_Inc > 180000) {
        Supertax <- 0.47*Super_Inc
      }
    } else if (Age %in% 55:58) {
      if (Super_Inc %in% 80001:180000){
        Supertax <- 0.22*Super_Inc
      } else if (Super_Inc > 180000) {
        Supertax <- 0.30*Super_Inc
      }
    }
  }
}


#' Imputation Credits Function
#'
#' Imputation Credits function for use on HIDLA, as part of the Income Tax Assesment
#'
#' @param ShareDiv
#' @param Citizenship
#' @param InvestInc_p
#' @return A vector

#Calculating imputation credits
IMPfn <- function(ShareDiv = ShareDiv, Citizenship = Citizenship, InvestInc_p = InvestInc_p) {
  if (Citizenship > 3) {
    Impcred <- 0
  }
  else {
    if (ShareDiv < 1) {
      Impcred <- InvestInc_p*0.15
    }
    else {
      Impcred <- ShareDiv*0.41
    }
  }
}
