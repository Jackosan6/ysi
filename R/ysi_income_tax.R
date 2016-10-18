#' YSI Income Tax Assesment Function
#'
#' This function combines all the nessesary income tax functions and data set mutations to be an all-in-one function for calculating Income Tax
#'

#' @name ysi_income_tax
#' @param df Argument is intended to be a survey with the Tax_Matrix set, as a data frame
#' @return A data frame
#' @export

ysi_income_tax <- function(df = NULL) {
  print("Starting function...")
  temp <- (ysi_mutate_combine(df))
  print("Calculating Taxable Income")
  temp <- temp %>% mutate(
    # Varibles for Imput Credits
    ShareDiv = ifelse(is.na(ShareDiv), 0, ShareDiv),
    Citizenship = ifelse(is.na(Citizenship), 0, ShareDiv),
    # Income tax assessment
    Deduct = mapply(DEDfn,.$RegInc),
    TaxInc = RegInc - Super_Inc - PrivTran - PubTransImp - ScholarshipsImp - SalSac_MainImp *52 - SalSac_OtherImp * 52 - Deduct,
    TaxInc = ifelse(is.na(TaxInc), 0, TaxInc),
    Supertax = mapply(SUPERTAXfn, Super_Inc, Age),
    RegInctax = mapply(income_tax_rates_func, income = TaxInc, year = as.character(Wave_year)), # If there is room for improvement this is the bottleneck
    WageIncImp_SalSac = ifelse(is.na(WageIncImp_SalSac), 0, WageIncImp_SalSac),
    Supertax = ifelse(is.na(Supertax), 0, Supertax))

  # Offsets
  print("Calculating Medicare Levy Surcharge, estimated run time is 1 minute...")
  temp <- temp %>% mutate(
    Ml = MLfn(TaxInc, Wave_year),
    Ml = ifelse(is.na(Ml), 0, Ml))
  print("Calculating LITO, estimated run time is 1 minute...")
  temp <- temp %>% mutate(
    LITO = LITOfn(TaxInc, Wave_year),
    LITO = ifelse(is.na(LITO), 0, LITO))
  print("Calculating SAPTO, estimated run time is 1 minute...")
  temp <- temp %>% mutate(
    Sapto = ifelse(Age < 65, 0 , SAPTOfn(TaxInc, Wave_year)),
    Sapto = ifelse(is.na(Sapto), 0, Sapto))

  print("Calculating Other Offsets, estimated run time is 1 minute...")
  temp <- temp %>% mutate(
    Mato = ifelse(YoB > 1957,0, sapply(WageIncImp_SalSac,MATOfn)),
    Mato = ifelse(is.na(Mato), 0, Mato))
  print("Calculating Other 1")
  temp <- temp %>% mutate(
    Otheroff =ifelse(TaxInc < 0,0, sapply(TaxInc, OTHEROFFfn)),
    Otheroff = ifelse(is.na(Otheroff), 0, Otheroff))
  print("Calculating Other 2")
  temp <- temp %>% mutate(
    Totaloff = (LITO + Sapto + Mato + Otheroff),
    Totaloff = ifelse(is.na(Totaloff), 0, Totaloff))
  print("Calculating Other 3")
  temp <- temp %>% mutate(
    # Other
    Impcred = ifelse(YoB>1957,0, mapply(IMPfn, ShareDiv,Citizenship, InvestInc_p)),
    Impcred = ifelse(is.na(Impcred), 0, Impcred))
  print("Calculating Other 4")
  temp <- temp %>% mutate(
    Redtax = mapply(REDTAXfn, RedundancyImp, Age),
    Redtax = ifelse(is.na(Redtax), 0, Redtax))
  temp <<- temp
  print("Calculating Other 5")
  temp <<- temp %>% mutate(
    # Tax Totals
    YSITotalIncTax_preIC = RegInctax + Ml - Totaloff )
  print("Calculating Other 6")
  temp <- temp %>% mutate(
    YSITotalIncTax_preIC = ifelse(YSITotalIncTax_preIC<0,0,YSITotalIncTax_preIC))
  print("Calculating Other 7")
  temp <- temp %>% mutate(
    YSI_Income_Tax = YSITotalIncTax_preIC - Impcred + Redtax + Supertax
  )

  return(temp)
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





#' YSI Applied Income Tax Rates Function
#'
#' Can be used on taxable income (TaxInc) or another income source that uses marginal income tax rates.
#' Previously called RegIncTAXfn()
#'
#' To model a new set of rates and thresholds use income_tax_model_func()
#'
#' @param income A numeric vector of income ($)
#' @param year The finanical year of interest, in the form "2013-14", as a vector.
#' @return A vector
#' @export

income_tax_rates_func <- function(income = NULL, year = NULL) {

  temp <- income_tax_rates_tbl %>%
    dplyr::filter(fyear == year)

  sum(diff(c(0,pmin(income,as.numeric(temp$upper_bracket))))*as.numeric(temp$marginal_rate))


}


#' YSI Applied Income Tax Rates Function for modeling new rates and/or thresholds
#'
#' Can be used on taxable income (TaxInc) or another income source that uses marginal income tax rates.
#'
#' @name income_tax_model_func
#' @param income A numeric vector of income ($)
#' @param Model_name The the model name .
#' @return A vector
#' @export

income_tax_model_func <- function(df = Null, Model_name = NULL) {
  df$Model_nam <- "2016-17"

  df <- df %>% mutate(
    Model_tax = mapply(income_tax_rates_func, income = df$TaxInc, year = df$Model_nam)
  )
}


# hidden functions

ysi_tax_inc_mut <- function(df = NULL) {

  mutate(df, TaxInc=RegInc-Super_Inc-PrivTran-PubTransImp-ScholarshipsImp-SalSac_MainImp*52-SalSac_OtherImp*52-Deduct)

}

MLfn <- function(TaxInc = NULL , fy_year = NULL)  {
  input <-data.frame(income = TaxInc, fy_year = fy_year)
  #hold <- filter(medicare_tbl, sapto == F)
  temp <- merge(medicare_tbl, input, by = "fy_year", sort = FALSE, all.y = TRUE)
  temp <- pminV(pmaxC(temp$taper * (temp$income - temp$lower_threshold),0), temp$rate * temp$income)
  temp[temp < 0 ] <-0
  return(temp)
}




LITOfn <- function(TaxInc = TaxInc, fy_year = NULL) {
  input <-data.frame(income = TaxInc, fy_year = fy_year)
  temp <- merge(lito_tbl, input, by = "fy_year", sort = FALSE, all.y = TRUE)
  temp <- pminV(pmaxC(temp$max_lito - (temp$income - temp$min_bracket) * temp$lito_taper, 0), temp$max_lito)
  return(temp)
}



REDTAXfn <- function(RedundancyImp, Age) {
  if (RedundancyImp <=  180000) {
    Redtax <- 0
  } else if(RedundancyImp %in% 180001:195000) {
    if (Age < 55){
      Redtax <- 0.315*RedundancyImp
    } else {
      Redtax <- 0.165*RedundancyImp
    }
  } else if(RedundancyImp > 195000) {
    if (Age < 55) {
      Redtax <- (0.315*15000) + 0.465*(RedundancyImp-195000)
    } else {
      Redtax <- (0.165*15000) + 0.465*(RedundancyImp-195000)
    }
  }
}


MATOfn <- function(WageIncImp_SalSac= WageIncImp_SalSac, YoB= YoB)  {if(WageIncImp_SalSac < 10000)
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

SAPTOfn <- function(TaxInc = NULL, fy_year = NULL) {
  input <-data.frame(income = TaxInc, fy_year = fy_year)
  temp <- merge(sapto_tbl, input, by = "fy_year", sort = FALSE, all.y = TRUE)
  temp <- pmaxC(pminV(temp$max_offset, temp$upper_threshold * temp$taper_rate - temp$rebate_income * temp$taper_rate),0)
  return(temp)

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
