#' YSI Income Tax Assesment Function
#'
#' This function combines all the nessesary income tax functions and data set mutations to be an all-in-one function for calculating Income Tax
#'

#' @name ysi_income_tax
#' @param df Argument is intended to be a fresh HILDA data frame
#' @return A data frame
#' @export

ysi_income_tax <- function(df = NULL) {
  temp <- ysi_mutate_combine(df)

  # Convert Imput Cred variables
  temp$ShareDiv <- as.numeric(temp$ShareDiv)
  temp$ShareDiv[is.na(temp$ShareDiv)] <- 0
  temp$Citizenship[is.na(temp$Citizenship)] <- 0

  # Main Function steps
  temp <- temp %>% mutate(Deduct = sapply(temp$RegInc, DEDfn),
                          TaxInc=RegInc-Super_Inc-PrivTran-PubTransImp-ScholarshipsImp-SalSac_MainImp*52-SalSac_OtherImp*52-Deduct,
                          Supertax = mapply(SUPERTAXfn, temp$Super_Inc, temp$Age),
                          RegInctax = sapply(temp$TaxInc, RegIncTAXfn),
                          RegInctaxchange = sapply(temp$TaxInc, RegIncTAXchangefn),
                          Ml = sapply(temp$Taxinc, MLfn))



  temp$Supertax[is.na(temp$Supertax)] <- 0

  return(temp)
}









# hidden functions


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



RegIncTAXfn <- function(income = NULL,brackets=c(18200, 37000, 80000, 180000,Inf),
                        rates=c(0,.19,.325,.37,.45)){
  sum(diff(c(0,pmin(income,brackets)))*rates)
}


RegIncTAXchangefn <- function(income,brackets=c(35000, 60000, 180000,Inf),
                              rates=c(0,.15,.35,.45)){
  sum(diff(c(0,pmin(income,brackets)))*rates)
}


ysi_tax_inc_mut <- function(df = NULL) {

  mutate(df, TaxInc=RegInc-Super_Inc-PrivTran-PubTransImp-ScholarshipsImp-SalSac_MainImp*52-SalSac_OtherImp*52-Deduct)

}




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




DEDfn <- function(RegInc = NULL) {if(RegInc < 6000)
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
